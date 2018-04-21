package headache
package rest

import io.netty.util.HashedWheelTimer
import java.nio.charset.Charset
import java.util.Arrays
import java.util.concurrent.atomic.AtomicInteger
import org.asynchttpclient.{AsyncHttpClient, Param, RequestBuilder}
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.control.ControlThrowable
import Json4sUtils._, CustomPicklers._, Json4sPConfig.conf
import scala.util.control.NoStackTrace

private[headache] trait DiscordRestApiSupport {

  protected def ahc: AsyncHttpClient
  protected def timer:HashedWheelTimer
  protected def token: String
  
  @volatile var maxQueuedRequests: Int = 100
  
  object channels extends Endpoint {
    def baseRequest(channelId: String) = s"/channels/${channelId}"
    
    def get(channelId: Snowflake)(implicit s: BackPressureStrategy): Future[Channel] = request(channelId)(parseJson(_).dyn.extract[Channel])
    def modify(channel: Channel)(implicit s: BackPressureStrategy): Future[Unit] = request(channel.id, method = "PATCH", body = toJson(channel), expectedStatus = 201)(_ => ())
    def delete(channelId: Snowflake)(implicit s: BackPressureStrategy): Future[Unit] = request(channelId, method = "DELETE")(_ => ())
    
    def getMessage(channelId: Snowflake, messageId: Snowflake)(implicit s: BackPressureStrategy): Future[Message] = 
      request(channelId, extraPath = s"/messages/$messageId")(parseJson(_).dyn.extract[Message])
    
    def getMessages(channelId: Snowflake, around: Snowflake = null, before: Snowflake = null, after: Snowflake = null, limit: Int = 100)(implicit s: BackPressureStrategy): Future[Seq[Message]] = {
      require(around != null || before != null || after != null)
      val params = Seq("limit" -> limit.toString) ++ Option(around).map("around" -> _) ++ Option(before).map("before" -> _) ++ Option(after).map("after" -> _)
      request(channelId, extraPath = "/messages", queryParams = params.toSeq)(parseJson(_).dyn.extract[Seq[Message]])
    }
    
    def createMessage(channelId: String, message: String, embed: Embed = null, tts: Boolean = false)(implicit s: BackPressureStrategy): Future[Message] = {
      val body = ("content" -> message) ~ ("nonce" -> (null: String)) ~ ("tts" -> tts) ~ ("embed" -> toJson(Option(embed)))
      request(channelId, extraPath = "/messages", body = body)(parseJson(_).dyn.extract[Message])
    }
    
    //more to come
  }
  
  private[this] val rateLimitRegistry = new RateLimitRegistry()
  protected[headache] val baseHeaders = Map[String, java.util.Collection[String]]("Authorization" -> Arrays.asList(token)).asJava
  private[DiscordRestApiSupport] trait Endpoint {
    private[this] val queuedRequests = new AtomicInteger()
    
    protected def baseRequest(token: String): String
    
    protected def request[T](token: String, extraPath: String = "", method: String = "GET", queryParams: Seq[(String, String)] = Seq.empty,
                             body: JValue = null, expectedStatus: Int = 200)(parser: String => T)(implicit s: BackPressureStrategy): Future[T] = {
      
      val base = baseRequest(token)
      var reqBuilder = new RequestBuilder(method).setUrl(base + extraPath).
        setHeaders(baseHeaders).addHeader("Content-Type", "application/json").setCharset(Charset.forName("utf-8")).
        setQueryParams(queryParams.map(t => new Param(t._1, t._2)).asJava)
      if (body != null) reqBuilder = reqBuilder.setBody(renderJson(body))
      
      val req = reqBuilder.build()
      
      def request(s: BackPressureStrategy): Future[T] = {
        val res = rateLimitRegistry.rateLimitFor(base) match {
          case Some(deadline) => Future.failed(RateLimitException(deadline.timeLeft))
          case _ => 
            AhcUtils.request(ahc.prepareRequest(req)){ resp =>
              lazy val body = resp.getResponseBody(Charset.forName("utf-8"))
              resp.getStatusCode match {
                case `expectedStatus` => parser(body)
                case 429 =>
                  val rl = parseJson(body).dyn
                  val deadline = rl.retry_after.extract[Int].millis
                  if (rl.global.extract) rateLimitRegistry.registerGlobalRateLimit(deadline.fromNow)
                  else rateLimitRegistry.registerGlobalRateLimit(deadline.fromNow)
                  throw RateLimitException(rl.retry_after.extract[Int].millis)

                case other => throw new RuntimeException(s"Unexpected status code $other:\n$body")
              }
            }
        }
        
        s match {
          case BackPressureStrategy.Retry(attempts) if attempts > 0 =>
            import scala.concurrent.ExecutionContext.Implicits.global //it's okay to use it for tihs here
            res.recoverWith {
              case RateLimitException(reset) => 
                if (queuedRequests.get >= maxQueuedRequests) throw TooManyQueuedRequests
                val promise = Promise[T]()
                timer.newTimeout({ timeout => 
                    promise completeWith request(BackPressureStrategy.Retry(attempts - 1))
                    queuedRequests.decrementAndGet
                  }, reset.length, reset.unit)
                promise.future
            }
            
          case _ => res
        }
      }
      request(s)
    }
  }
  
  case class RateLimitException private(retryAfter: FiniteDuration) extends ControlThrowable
  object TooManyQueuedRequests extends Exception with NoStackTrace 
}

