package discordccc
package model

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}
import scala.collection.JavaConverters._

final class FileBasedChatModel extends ChatModel {

  private val kryoPool = KryoPool.withByteArrayOutputStream(2048, new ScalaKryoInstantiator())
  private val parallelism = Runtime.getRuntime.availableProcessors
  
  private val guilds = collection.mutable.HashSet[Server]()
  private val guildChannels = new java.util.HashMap[String, Array[String]]().asScala
  private val groupedMembers = new java.util.HashMap[String, util.CompressedSequentialStorage[String]]().asScala
  private val channels = new util.FileBackedBucketStorage[Channel](parallelism, "ccc/channels", kryoPool, _.id)
  private val members = new util.FileBackedBucketStorage[Member](parallelism, "ccc/members", kryoPool, m => m.userId + "|" + m.serverId)
  private val users = new util.FileBackedBucketStorage[User](parallelism, "ccc/users", kryoPool, _.id)
  
  def putMember(member: Member): Unit = {
    members put member
    groupedMembers.getOrElseUpdate(member.serverId, new util.CompressedSequentialStorage[String](kryoPool, 500)).append(member.userId)
  }
  def getMember(userId: String, serverId: String): Option[Member] = members.get(userId + "|" + serverId)
  
  def putUser(user: User): Unit = users put user
  def getUser(id: String): Option[User] = users get id
  
  def putServer(server: Server): Unit = guilds += server
  def getServer(id: String): Option[Server] = guilds find (_.id == id)
  def getServerMembersCount(serverId: String): Int = groupedMembers.get(serverId).fold(0)(_.count)
  def getServerMembers(serverId: String): Iterator[Member] = {
    val userIds = groupedMembers.get(serverId).fold[Iterator[String]](Iterator.empty)(_.iterator)
    userIds.flatMap(i => getMember(i, serverId))
  }
  def getServerChannels(serverId: String): Seq[Channel] = guildChannels.get(serverId).fold(Seq.empty[Channel])(cs => cs flatMap getChannel)
  
  def putChannel(channel: Channel): Unit = channels put channel
  def getChannel(id: String): Option[Channel] = channels get id
  def registerChannelUser(channelId: String, userId: String): Unit = 
    groupedMembers.getOrElseUpdate(channelId, new util.CompressedSequentialStorage[String](kryoPool, 20)).append(userId)
  def getChannelUsersCount(channelId: String): Int = groupedMembers.get(channelId).fold(0)(_.count)
  def getChannelUsers(channelId: String): Iterator[User] = groupedMembers.get(channelId).fold[Iterator[String]](Iterator.empty)(_.iterator) flatMap getUser
  
  
  //store the last 5 visted channels
  private type ChannelId = String
  private val messagesCache = new util.LruMap[ChannelId, util.FileBackedBucketStorage[Message]](5) //store the last 5 messages
  private def newChannelCache = new util.FileBackedBucketStorage[Message](parallelism, "ccc/messages", kryoPool, _.id)
  private def getOrCreateMessagesCache(channelId: ChannelId) = messagesCache.getOrElseUpdate(channelId, newChannelCache)
  def putMessage(message: Message): Unit = getOrCreateMessagesCache(message.channelId) put message
  def getMessage(messageId: String): Option[Message] = messagesCache.iterator.map(_._2 get messageId).collectFirst { case Some(msg) => msg }
  
  //bump the last access of this channel by removing the key and reentering it.
  def switchToChannel(channel: Channel): Unit = messagesCache(channel.id) = messagesCache.getOrElse(channel.id, newChannelCache)
}
