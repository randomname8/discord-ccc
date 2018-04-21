package headache

import java.util.concurrent.ArrayBlockingQueue
import org.json4s.native.JsonMethods.{pretty, render}
import scala.annotation.tailrec
import scala.collection.JavaConverters._

class DiscordListenerBuffer(var logOutgoingGatewayMessage: Boolean = false, var logIncomingGatewayMessage: Boolean = false) extends DiscordClient.DiscordListener {
  private val queue = new ArrayBlockingQueue[GatewayEvents.GatewayEvent](1000)

  override def onGatewayEvent(connection) = {
    case evt =>
      if (!queue.offer(evt)) {
        queue.synchronized {
          if (queue.remainingCapacity == 0) {
            queue.poll()
          }
          queue.put(evt)
        }
      }
  }
  override def onGatewayData(data): Unit = if (logIncomingGatewayMessage) println("received: " + pretty(render(data.jv)))
  override def onMessageBeingSent(connection, msg): Unit = if (logOutgoingGatewayMessage) println("sending: " + msg)
  override def onUnexpectedGatewayOp(connection, op: Int, data): Unit = println(s"unexepected GW op $op: " + pretty(render(data.jv)))
  override def onReconnecting(connection, reason): Unit = println(s"Reconnecting $connection due to $reason")
  override def onConnectionOpened(connection: DiscordClient#Connection): Unit = println(s"$connection stablished")
  override def onConnectionClosed(connection: DiscordClient#Connection): Unit = println(s"$connection closed")
  override def onDisconnected(connection: DiscordClient#Connection, code: Int, reason: String): Unit = println(s"$connection lost, code: $code, reason: $reason")
  override def onConnectionError(connection: DiscordClient#Connection, error: Throwable): Unit = println(s"$connection error: $error")

  def peek = queue.peek()
  def pop() = queue.poll()
  @tailrec final def popUntil(f: GatewayEvents.GatewayEvent => Boolean): GatewayEvents.GatewayEvent = {
    val res = pop()
    if (f(res)) res
    else popUntil(f)
  }

  def remaining = queue.size()
  def iterator = queue.iterator.asScala
  def consumingIterator = Iterator.continually(pop).takeWhile(_ != null)
}
