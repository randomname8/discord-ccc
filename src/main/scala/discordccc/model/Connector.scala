package discordccc
package model

import scala.concurrent.Future

/**
 * Connectors represent integration to a protocol such as XMPP or Discord.
 * You should typically implement your connector in a trait, and then register it in the ConnectorRegistry
 * by defining an entry mixing in your trait.
 */
trait Connector {
  
  val listeners = collection.mutable.Buffer[PartialFunction[Connector.Event, Any]]()
  
  def getServer(id: Long): Option[Server]
  def getChannels(): Seq[Channel]
  def getChannel(id: Long): Option[Channel]
  def getMember(userId: Long, channelId: Long): Option[Member]
  def getMembers(channel: Channel): IndexedSeq[Member Either User]
  def getUser(id: Long): Option[User]
  
  def sendMessage(channel: Channel, content: Content): Future[Message]
  def getLastMessages(channel: Channel, from: Option[Long] = None, limit: Option[Int] = None): Future[Seq[Message]]
  
  def getCustomEmoji(id: Long): Option[String]
  
  def init(): Future[Any]
}
object Connector {
  sealed trait Event
  case class ServerEvent(server: Server, status: Status, connector: Connector) extends Event
  case class ChannelEvent(channel: Channel, status: Status, connector: Connector) extends Event
  case class ConnectionLost(connector: Connector) extends Event
  case class ConnectionRestablished(connector: Connector) extends Event
  case class MessageEvent(message: Message, status: Status, connector: Connector) extends Event
  
  sealed trait Status
  case object Created extends Status
  case object Updated extends Status
  case object Deleted extends Status
}
trait ConnectorEntity {
  def connector: Connector
}