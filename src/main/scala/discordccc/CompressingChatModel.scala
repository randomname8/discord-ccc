package discordccc

import com.twitter.chill.ScalaKryoInstantiator
import scala.collection.JavaConverters._

class CompressingChatModel extends ChatModel {

  val kryo = new ScalaKryoInstantiator().newKryo()
  
  private val guilds = collection.mutable.HashSet[Server]()
  private val guildChannels = new java.util.HashMap[String, Array[String]]().asScala
  private val channels = new util.CompressingBucketStore[Channel](kryo, "channels", 1200, 400, 10, _.id)
  private val members = new util.CompressingBucketStore[Member](kryo, "members", 12000, 400, 20, m => m.userId + "|" + m.serverId)
  private val users = new util.CompressingBucketStore[User](kryo, "users", 6000, 400, 20, m => m.id)
  
  private def statsFor(store: util.CompressingBucketStore[_]) = StoreStats(store.bucketsUsage.clone, store.bucketEntries.clone, store.overrunBuckets.size, store.cacheHits, store.cacheMisses)
  def statistics(): Statistics = Statistics(statsFor(channels), statsFor(members))
  
  case class StoreStats private(usage: Array[Float], entriesPerBucket: Array[Int], overrunBuckets: Int, cacheHits: Long, cacheMisses: Long)
  case class Statistics private(channels: StoreStats, members: StoreStats)
  
  def putMember(member: Member): Unit = members store member
  def getMember(userId: String, serverId: String): Option[Member] = members.retrieve(userId + "|" + serverId)
  
  def putUser(user: User): Unit = users store user
  def findUser(id: String): Option[User] = users retrieve id
  
  def putServer(server: Server): Unit = guilds += server
  def findServer(id: String): Option[Server] = guilds find (_.id == id)
  
  def putChannel(channel: Channel): Unit = channels store channel
  def findChannel(id: String): Option[Channel] = channels retrieve id
  def getServerChannels(serverId: String): Seq[Channel] = guildChannels.get(serverId).fold(Seq.empty[Channel])(cs => cs flatMap findChannel)
  
  
  //store the last 5 visted channels
  private type ChannelId = String
  private val messagesCache = new util.LruMap[ChannelId, util.CompressingBucketStore[Message]](5) //store the last 5 messages
  private def newChannelCache = new util.CompressingBucketStore[Message](kryo, "messages", 12000, 400, 200, _.id)
  private def getOrCreateMessagesCache(channelId: ChannelId) = messagesCache.getOrElseUpdate(channelId, newChannelCache)
  def putMessage(message: Message): Unit = getOrCreateMessagesCache(message.channelId) store message
  def findMessage(messageId: String): Option[Message] = messagesCache.iterator.map(_._2 retrieve messageId).collectFirst { case Some(msg) => msg }
  
  //bump the last access of this channel by removing the key and reentering it.
  def switchToChannel(channel: Channel): Unit = messagesCache(channel.id) = messagesCache.getOrElse(channel.id, newChannelCache)
    
}
