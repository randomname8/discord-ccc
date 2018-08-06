package discordccc

import discordccc.model._
import org.agrona.collections.Long2ObjectHashMap
import org.asynchttpclient.request.body.multipart.Part
import scala.collection.JavaConverters._
import scala.concurrent.Future

class SimpleConnector extends Connector {
  val servers = new Long2ObjectHashMap[Server]()
  val channels = new Long2ObjectHashMap[Channel]()
  val members = new collection.mutable.HashMap[(Long, Long), Member]
  val users = new Long2ObjectHashMap[User]()
  val customEmojis = new Long2ObjectHashMap[String]()
  
  def getServer(id: Long): Option[Server] = Option(servers.get(id))
  def getChannels(): Seq[Channel] = channels.values.asScala.to[Vector]
  def getChannel(id: Long): Option[Channel] = Option(channels.get(id))
  def getMember(userId: Long, channelId: Long): Option[Member] = members.get(userId -> channelId)
  def getMembers(channel: Channel): IndexedSeq[Member Either User] = IndexedSeq.empty
  def getUser(id: Long): Option[User] = Option(users.get(id))
  
  def sendMessage(channel: Channel, content: Content, attachments: Seq[Part] = Seq.empty, progressListener: (Long, Long, Long) => Unit = (_, _, _) => ()): Future[Message] = Future.failed(new UnsupportedOperationException)
  def getLastMessages(channel: Channel, from: Option[Long] = None, limit: Option[Int] = None): Future[Seq[Message]] = Future.failed(new UnsupportedOperationException)
  def markAsRead(message: Message): Future[Unit] = Future.failed(new UnsupportedOperationException)
  
  def getCustomEmoji(id: Long): Option[String] = Option(customEmojis.get(id))
  
  def init(): Future[Any] = Future.successful(())
}
