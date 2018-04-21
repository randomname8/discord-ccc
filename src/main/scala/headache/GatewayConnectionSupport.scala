package headache

import enumeratum.values.IntEnumEntry
import java.io.{ByteArrayInputStream, BufferedReader, InputStreamReader}
import java.time.Instant
import java.util.Arrays
import java.util.zip.InflaterInputStream
import org.asynchttpclient.ws

import org.json4s.JsonAST.{JValue, JInt}
import org.json4s.JsonDSL._
import org.json4s.native.JsonParser
import scala.annotation.tailrec
import scala.concurrent._, duration._, ExecutionContext.Implicits._
import scala.util.control.NoStackTrace
import scala.util.control.NonFatal
import Json4sUtils._, CustomPicklers._, Json4sPConfig.conf

private[headache] trait GatewayConnectionSupport { self: DiscordClient =>
  import DiscordClient._

  protected def startShard(gw: String, shard: Int, totalShards: Int, lastSession: Option[LastSessionState] = None): Future[GatewayConnection] = try {
    val res = new GatewayConnectionImpl(gw, shard, totalShards, lastSession)
    val websocketFuture = ahc.prepareGet(gw).execute(new ws.WebSocketUpgradeHandler(Arrays.asList(res)))
    val ready = Promise[GatewayConnection]()
    websocketFuture.toCompletableFuture.handle[Unit] {
      case (_, null) =>
        ready.success(res); ()
      case (_, ex) => ready.failure(ex); ()
    }
    ready.future
  } catch {
    case NonFatal(ex) => Future.failed(ex)
  }

  protected case class SessionData(id: String, gatewayProtocol: Int)
  protected case class LastSessionState(data: SessionData, seq: Long, connectionAttempt: Int)
  protected final class GatewayConnectionImpl(val gateway: String, val shardNumber: Int, val totalShards: Int,
      lastSession: Option[LastSessionState]) extends GatewayConnection with ws.WebSocketTextListener with ws.WebSocketCloseCodeReasonListener with ws.WebSocketByteListener {
    @volatile private[this] var active = true
    @volatile private[this] var websocket: ws.WebSocket = _
    @volatile private[this] var seq = 0l
    @volatile private[this] var session: Option[SessionData] = None

    override def isActive = active
    override def close() = if (active && websocket != null) websocket.close()

    override def onOpen(ws): Unit = {
      websocket = ws
      listener.onConnectionOpened(this)
    }
    override def onClose(ws): Unit = if (isActive) {
      websocket = null
      active = false
      listener.onConnectionClosed(this)
    }
    override def onClose(ws, code, reason): Unit = if (isActive) { //after a break down, netty will eventually realize that the socket broke, and even though we already called websocket.close(), it will eventually invoke this method.
      listener.onDisconnected(this, code, reason)
      reconnect(DisconnectedByServer)
    }
    override def toString = s"GatewayConnection(gw=$gateway, shardNumber=$shardNumber, totalShards=$totalShards, seq=$seq, session=${session.map(_.id)})"

    /**
     * **************************************************
     * Definition of possible states for the connection *
     * **************************************************
     */

    case class GatewayMessage(op: GatewayOp, tpe: Option[String], payload: () => DynJValueSelector)
    val stateMachine = new StateMachine[GatewayMessage] {
      private[this] val identityMsg = renderJson {
        gatewayMessage(
          GatewayOp.Identify,
          ("token" -> s"Bot $token") ~
            ("properties" -> (
              ("$os" -> System.getProperty("os.name")) ~
              ("$browser" -> "strife v1.0") ~
              ("$device" -> "strife") ~
              ("$referring_domain" -> "") ~
              ("$referrer" -> "")
            )) ~
              ("compress" -> true) ~
              ("large_threshold" -> 50) ~
              ("shard" -> Seq(shardNumber, totalShards))
        )
      }

      def initState = hello
      def hello = transition {
        case GatewayMessage(GatewayOp.Hello, _, payload) =>
          nextHeartbeat(payload().d.heartbeatInterval.extract)
          lastSession match {
            case None =>
              send(identityMsg)
              handshake
            case Some(lastSession) =>
              send(renderJson(gatewayMessage(GatewayOp.Resume, ("token" -> s"Bot $token") ~
                ("session_id" -> lastSession.data.id) ~
                ("seq" -> lastSession.seq))))
              resume
          }
      }

      def handshake = transition {
        case evt @ GatewayMessage(GatewayOp.Dispatch, Some("READY"), payload) =>
          session = Some(SessionData(payload().d.sessionId.extract, payload().d.v.extract))
          dispatcher(evt)
          dispatcher

        case GatewayMessage(GatewayOp.InvalidSession, _, _) =>
          listener.onConnectionError(GatewayConnectionImpl.this, new IllegalStateException("Received an invalid session after sending identification.") with NoStackTrace)
          close()
          done
      }

      def resume: Transition = transition {
        case GatewayMessage(GatewayOp.InvalidSession, _, _) =>
          send(identityMsg)
          handshake

        case evt @ GatewayMessage(GatewayOp.Dispatch, Some("RESUMED"), _) =>
          session = lastSession.map(_.data) //only session needs to be assgined, seq is obtained from the resumed message
          dispatcher(evt)
          dispatcher

        case evt @ GatewayMessage(GatewayOp.Dispatch, _, _) => //replayed messages
          dispatcher(evt)
          resume //continue resuming
      }

      def dispatcher: Transition = transition {
        case GatewayMessage(GatewayOp.Dispatch, Some(tpe), payload) =>
          try {
            listener.onGatewayEvent(GatewayConnectionImpl.this)(GatewayEvents.GatewayEvent(
              GatewayEvents.EventType.withValueOpt(tpe).getOrElse(GatewayEvents.EventType.Unknown), payload
            ))
          } catch { case NonFatal(e) => listener.onConnectionError(GatewayConnectionImpl.this, e) }
          dispatcher
        case GatewayMessage(GatewayOp.Reconnect, _, _) =>
          reconnect(RequestedByServer)
          done
      }
    }

    override def onError(ex): Unit = listener.onConnectionError(this, ex)
    override def onMessage(msg: String): Unit = {
      //do basic stream parsing to obtain general headers, the idea is to avoid computation as much as possible here.

      val parser = (p: JsonParser.Parser) => {
        import JsonParser._
        @tailrec def parse(seq: Option[Long] = null, op: Int = -1, tpe: Option[String] = null): (Option[Long], Int, Option[String]) = {
          if (seq != null && op != -1 && tpe != null) (seq, op, tpe)
          else p.nextToken match {
            case End => (if (seq eq null) None else seq, op, if (tpe eq null) None else tpe)
            case FieldStart("t") => p.nextToken match {
              case StringVal(tpe) => parse(seq, op, Some(tpe))
              case NullVal => parse(seq, op, None)
              case _ => p.fail("event type not a string")
            }
            case FieldStart("s") => p.nextToken match {
              case LongVal(seq) => parse(Some(seq), op, tpe)
              case IntVal(seq) => parse(Some(seq.intValue), op, tpe)
              case NullVal => parse(None, op, tpe)
              case _ => p.fail("event type not a long")
            }
            case FieldStart("op") => p.nextToken match {
              case LongVal(op) => parse(seq, op.toInt, tpe)
              case IntVal(op) => parse(seq, op.intValue, tpe)
              case _ => p.fail("op type not a long")
            }
            case _ => parse(seq, op, tpe)
          }
        }
        parse()
      }

      try {
        val (s, op, tpe) = JsonParser.parse(msg, parser)
        if (op == -1) throw new IllegalStateException("no option found in discord message?\n" + msg)
        lazy val payload = parseJson(msg).camelizeKeys.dyn

        s foreach (seq = _)
        GatewayOp.withValueOpt(op).fold {
          listener.onUnexpectedGatewayOp(this, op, payload)
        } { op =>

          listener.onGatewayOp(this, op, payload)
          stateMachine.orElse[GatewayMessage, Unit] {
            case GatewayMessage(GatewayOp.Heartbeat, _, _) => send(renderJson(("op" -> GatewayOp.Heartbeat.value) ~ ("d" -> seq)))
            case GatewayMessage(GatewayOp.Dispatch, _, _) =>
            case _ =>
          }.apply(GatewayMessage(op, tpe, () => payload))
        }

      } catch { case NonFatal(e) => listener.onConnectionError(this, e) }
    }
    override def onMessage(bytes: Array[Byte]): Unit = {
      val reader = new BufferedReader(new InputStreamReader(new InflaterInputStream(new ByteArrayInputStream(bytes))))
      val msg = reader.lines.collect(java.util.stream.Collectors.joining())
      onMessage(msg)
    }

    def send(msg: String) = {
      if (!active) throw new IllegalStateException(s"Shard $shardNumber is closed!")
      listener.onMessageBeingSent(this, msg)
      websocket.sendMessage(msg)
    }

    def gatewayMessage(op: IntEnumEntry, data: JValue, eventType: Option[String] = None): JValue = {
      ("op" -> op.value) ~
        ("t" -> eventType.orNull) ~
        ("s" -> seq) ~
        ("d" -> data)
    }

    override def sendStatusUpdate(idleSince: Option[Instant], status: Status): Unit = {
      send(renderJson(
        gatewayMessage(GatewayOp.StatusUpdate, ("idle_since" -> idleSince.map(e => JInt(e.toEpochMilli)).orNull) ~ ("game" -> (status match {
          case Status.PlayingGame(game) => ("name" -> game): JValue
          case Status.Streaming(name, url) => ("name" -> name) ~ ("url" -> url)
          case Status.Empty => null
        })), Some("STATUS_UPDATE"))
      ))
    }
    override def sendRequestGuildMembers(guildId: String, query: String = "", limit: Int = 0): Unit = {
      send(renderJson(
        gatewayMessage(GatewayOp.RequestGuildMembers, ("guild_id" -> guildId) ~ ("query" -> query) ~ ("limit" -> limit))
      ))
    }
    override def sendVoiceStateUpdate(guildId: String, channelId: Option[String], selfMute: Boolean, selfDeaf: Boolean): Unit = {
      send(renderJson(
        gatewayMessage(GatewayOp.VoiceStateUpdate, ("guild_id" -> guildId) ~ ("channel_id" -> channelId.orNull) ~
          ("self_mute" -> selfMute) ~ ("self_deaf" -> selfDeaf), Some("VOICE_STATE_UPDATE"))
      ))
    }

    def nextHeartbeat(interval: Int): Unit = {
      timer.newTimeout(timeout => if (isActive) { // don't hog the timer thread
        Future {
          send(renderJson(("op" -> GatewayOp.Heartbeat.value) ~ ("d" -> seq)))
          //after sending the heartbeat, change the current behaviour to detect the answer
          //if no answer is received in 5 seconds, reconnect.
          val prevBehaviour = stateMachine.current

          val heartbeatTimeout = (interval * 0.8).toInt.millis.toSeconds
          val timeout = timer.newTimeout({ timeout =>
            if (!timeout.isCancelled && isActive) {
              listener.onConnectionError(this, new RuntimeException(s"Did not receive a HeartbeatAck in ${heartbeatTimeout} seconds!") with NoStackTrace)
              reconnect(HeartbeatMissed)
            }
          }, heartbeatTimeout, SECONDS)

          lazy val detectHeartbeatAck: stateMachine.Transition = stateMachine.transition {
            case GatewayMessage(GatewayOp.HeartbeatAck, _, _) =>
              timeout.cancel()
              prevBehaviour
            case other if prevBehaviour.isDefinedAt(other) =>
              prevBehaviour(other)
              detectHeartbeatAck
          }

          stateMachine.switchTo(detectHeartbeatAck)
          nextHeartbeat(interval)
        }
      }, interval, MILLISECONDS)
    }

    def reconnect(reason: ReconnectReason): Unit = {
      websocket.close()
      onClose(websocket)
      listener.onReconnecting(this, reason)
      val reconnectInstance = if (session.isDefined) 0 else lastSession.map(_.connectionAttempt + 1).getOrElse(0)
      val newLastSession = session.map(s => LastSessionState(s, seq, reconnectInstance))
      def reconnectAttempt(duration: FiniteDuration): Unit = {
        timer.newTimeout(
          _ =>
            startShard(gateway, shardNumber, totalShards, newLastSession).failed.foreach(_ => reconnectAttempt(5.seconds)),
          duration.length, duration.unit
        )
      }
      if (reconnectInstance > 0) reconnectAttempt(5.seconds)
      else reconnectAttempt(0.seconds)
    }
  }
}
