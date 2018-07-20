package discordccc

import better.files._
import discordccc.model._
import headache.{Message => _, _}
import headache.rest.BackPressureStrategy
import scala.io.AnsiColor
import scala.concurrent._, ExecutionContext.Implicits._
import JsonUtils._
import GatewayEvents._

/**
 * Connect to an account and retrieve all memebers.
 */
object MessageCompressionTest {
  def main(args: Array[String]): Unit = {
    val token = file"test-token".contentAsString()
    
    val store = new util.CompressedSequentialStorage[Message](new util.KryoPool(8, util.CustomKryoInstantiator, 600, 1024*10), 100)
    
    val client = new DiscordClient(token, new DiscordClient.DiscordListener {
        def prettyPrint(js: DynJValueSelector) = JsonUtils.renderJson(js.jv.result.get, true)
        override def onUnexpectedGatewayOp(connection: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector): Unit =
          println(AnsiColor.RED + s"unexpected op: $op data: ${prettyPrint(data)}" + AnsiColor.RESET)
        override def onUnexpectedVoiceOp(connection: DiscordClient#VoiceConnection, op: Int, data: => DynJValueSelector): Unit =
          println(AnsiColor.RED + s"unexpected voice op: $op data: ${prettyPrint(data)}" + AnsiColor.RESET)
//        override def onMessageBeingSent(connection: DiscordClient#Connection, msg: String): Unit =
//          println(AnsiColor.CYAN + s"sending: $msg" + AnsiColor.RESET)

        override def onReconnecting(connection: DiscordClient#Connection, reason: DiscordClient.ReconnectReason): Unit =
          println(AnsiColor.RED + s"reconnecting $connection due to $reason" + AnsiColor.RESET)
        override def onConnectionOpened(connection: DiscordClient#Connection): Unit =
          println(s"connected $connection")
        override def onConnectionClosed(connection: DiscordClient#Connection): Unit =
          println(s"connected closed $connection")
        override def onDisconnected(connection: DiscordClient#Connection, code: Int, reason: String): Unit =
          println(AnsiColor.RED + s"disconnected $connection code: $code reason: $reason" + AnsiColor.RESET)
        override def onConnectionError(connection: DiscordClient#Connection, error: Throwable): Unit = {
          val stacktrace = Iterator.iterate(error)(_.getCause).takeWhile(_ != null).foldLeft(new StringBuilder){ (sb, ex) =>
            sb.append("caused by: ").append(ex)
            ex.getStackTrace foreach (t => sb.append("\n  ").append(t))
            sb
          }.result
          println(AnsiColor.RED_B + AnsiColor.WHITE + s"connection error $connection $stacktrace" + AnsiColor.RESET)
        }
      
        var ready: Ready = _
        override def onGatewayEvent(connection: DiscordClient#GatewayConnection) = {
          case ReadyEvent(evt) =>
            ready = evt
            println(AnsiColor.YELLOW + evt + AnsiColor.RESET)
            val guilds = evt.guilds.collect { case Right(g) => g }
            val selectedGuild = guilds.maxBy(_.memberCount)
            val channel = selectedGuild.channels.find(_.name.map(_ contains "pubg-general-chat").getOrElse(false)).get
            println(s"Selected guild ${selectedGuild.name} and channel $channel")
            
            implicit val strategy = BackPressureStrategy.Retry(3)
            
            def fetchPage(before: Snowflake, pageNumber: Int, allMessages: Vector[Message]): Unit = {
              connection.client.channels.getMessages(channel.id, before = before, limit = 100).foreach { messages =>
                println(s"Fetched page $pageNumber starting at ${before.snowflakeString}")
                
                val newMessages = messages map (m => Message(
                    m.id, Seq(Content.Text(m.content)), m.timestamp, None, Seq.empty, channel.id,
                    selectedGuild.id, m.author.id, null))
                  
                newMessages foreach store.append
                val newAllMessages = allMessages ++ newMessages
                
                if (messages.isEmpty || pageNumber == 0 || before == messages.last.id) {
                  println(s"Total messages ${allMessages.size}")
                  val uncompletedPageMethod = classOf[util.CompressedSequentialStorage[_]].getDeclaredField("uncompletedPage")
                  val pagesMethod = classOf[util.CompressedSequentialStorage[_]].getDeclaredField("pages")
                  uncompletedPageMethod.setAccessible(true)
                  pagesMethod.setAccessible(true)
                  
                  println("Size of compressed " + org.openjdk.jol.info.GraphLayout.parseInstance((pagesMethod.get(store), uncompletedPageMethod.get(store))).toFootprint)
                  println("Size of uncompressed " + org.openjdk.jol.info.GraphLayout.parseInstance(newAllMessages).toFootprint)
                } else {
                  fetchPage(messages.head.id, pageNumber - 1, newAllMessages)
                }
              }
              
            }
            fetchPage(channel.lastMessageId.get, 100, Vector.empty)
            
            
          case _ =>
        }
      })
    client.login().onComplete(println)
  }
}
