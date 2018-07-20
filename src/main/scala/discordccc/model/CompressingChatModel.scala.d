package discordccc
package model

class CompressingChatModel extends ConnectorStore {

  val parallelismLevel = Runtime.getRuntime.availableProcessors
  val kryoPool = new util.KryoPool(parallelismLevel * 2, util.CustomKryoInstantiator, 1024*1024, 1024*1024)
  
  private val guilds = collection.mutable.HashSet[Server]()
  private val guildChannels = new collection.concurrent.TrieMap[(Long, Connector), Array[Long]]()
  private val groupedMembers = new collection.concurrent.TrieMap[(Long, Connector), util.CompressedSequentialStorage[Long]]()
  private val channels = new util.CompressingBucketStore[Channel, (Long, Connector)](kryoPool, "channels", 1200, 400, 10, parallelismLevel, c => (c.id, c.connector))
  private val members = new util.CompressingBucketStore[Member, (Long, Long, Connector)](kryoPool, "members", 20000, 600, 0, parallelismLevel, m => (m.userId, m.serverId, m.connector))
  private val users = new util.CompressingBucketStore[User, (Long, Connector)](kryoPool, "users", 6000, 400, 0, parallelismLevel, u => (u.id, u.connector))
  
  private def statsFor(store: util.CompressingBucketStore[_, _]) = StoreStats(store.bucketsUsage.clone, store.bucketEntries.clone, store.overrunBuckets.size, store.cacheHits.get, store.cacheMisses.get)
  def statistics(): Statistics = Statistics(statsFor(channels), statsFor(members))
  
  case class StoreStats private(usage: Array[Float], entriesPerBucket: Array[Int], overrunBuckets: Int, cacheHits: Long, cacheMisses: Long)
  case class Statistics private(channels: StoreStats, members: StoreStats)
  
  def putMember(member: Member): Unit = {
    members store member
    groupedMembers.getOrElseUpdate((member.serverId, member.connector), new util.CompressedSequentialStorage[Long](kryoPool, 500)).append(member.userId)
  }
  def getMember(userId: Long, serverId: Long, connector: Connector): Option[Member] = members.retrieve((userId, serverId, connector))
  
  def putUser(user: User): Unit = users store user
  def getUser(id: Long, connector: Connector): Option[User] = users.retrieve((id, connector))
  
  def putServer(server: Server): Unit = guilds += server
  def getServer(id: Long, connector: Connector): Option[Server] = guilds find (s => s.id == id && s.connector == connector)
  def getServerMembersCount(serverId: Long, connector: Connector): Int = groupedMembers.get((serverId, connector)).fold(0)(_.count)
  def getServerMembers(serverId: Long, connector: Connector): Iterator[Member] = {
    val userIds = groupedMembers.get((serverId, connector)).fold[Iterator[Long]](Iterator.empty)(_.iterator)
    userIds.flatMap(i => getMember(i, serverId, connector))
  }
  def getServerChannels(serverId: Long, connector: Connector): Seq[Channel] = guildChannels.get((serverId, connector)).
    fold(Seq.empty[Channel])(cs => cs flatMap (id => getChannel(id, connector)))
  
  def putChannel(channel: Channel): Unit = channels store channel
  def getChannel(id: Long, connector: Connector): Option[Channel] = channels.retrieve((id, connector))
  def registerChannelUser(channelId: Long, userId: Long, connector: Connector): Unit = 
    groupedMembers.getOrElseUpdate((channelId, connector), new util.CompressedSequentialStorage[Long](kryoPool, 20)).append(userId)
  def getChannelUsersCount(channelId: Long, connector: Connector): Int = groupedMembers.get((channelId, connector)).fold(0)(_.count)
  def getChannelUsers(channelId: Long, connector: Connector): Iterator[User] = groupedMembers.get((channelId, connector)).
    fold[Iterator[Long]](Iterator.empty)(_.iterator) flatMap (id => getUser(id, connector))
  
  
  //store the last 5 visted channels
  private type ChannelId = Long
  private val messagesCache = new util.LruMap[(ChannelId, Connector), util.CompressingBucketStore[Message, (Long, Connector)]](5) //store the last 5 messages
  private def newChannelCache = new util.CompressingBucketStore[Message, (Long, Connector)](kryoPool, "messages", 12000, 400, 200, parallelismLevel, c => (c.id, c.connector))
  private def getOrCreateMessagesCache(channelId: ChannelId, connector: Connector) = messagesCache.getOrElseUpdate((channelId, connector), newChannelCache)
  def putMessage(message: Message): Unit = getOrCreateMessagesCache(message.channelId, message.connector) store message
  def getMessage(messageId: Long, connector: Connector): Option[Message] = messagesCache.iterator.map(_._2.retrieve((messageId, connector))).collectFirst { case Some(msg) => msg }
  
  //bump the last access of this channel by removing the key and reentering it.
  def switchToChannel(channel: Channel): Unit = messagesCache((channel.id, channel.connector)) = messagesCache.getOrElse((channel.id, channel.connector), newChannelCache)
    
}
