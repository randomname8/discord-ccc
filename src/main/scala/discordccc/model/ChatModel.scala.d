package discordccc
package model

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._

/**
 * Model definition for a chat.
 * It supports the operations for tracking and fetching data that mutates over time.
 */
class ChatModel(storageFactory: Connector => ConnectorStore) {
  
  private[this] val stores = new ConcurrentHashMap[Connector, ConnectorStore]().asScala
  private def storeFor(connector: Connector) = stores.getOrElseUpdate(connector, storageFactory(connector))
  
  /**
   * Adds or updates a user to the list of users.
   */
  def putUser(user: User): Unit = storeFor(user.connector).putUser(user)
  def getUser(id: Long, connector: Connector): Option[User] = storeFor(connector).getUser(id)
  
  /**
   * Adds a or updates a message to the list of messages known messages.
   */
  def putMessage(message: Message): Unit = storeFor(message.connector).putMessage(message)
  def getMessage(messageId: Long, connector: Connector): Option[Message] = storeFor(connector).getMessage(messageId)
  
  /**
   * Adds a or updates a server to the list of servers.
   */
  def putServer(server: Server): Unit = storeFor(server.connector).putServer(server)
  def getServer(id: Long, connector: Connector): Option[Server] = storeFor(connector).getServer(id)
  def getServerChannels(serverId: Long, connector: Connector): Seq[Channel] = storeFor(connector).getServerChannels(serverId)
  def getServerMembers(serverId: Long, connector: Connector): Iterator[Member] = storeFor(connector).getServerMembers(serverId)
  def getServerMembersCount(serverId: Long, connector: Connector): Int = storeFor(connector).getServerMembersCount(serverId)
  
  /**
   * Adds a or updates a member to the list of members.
   */
  def putMember(member: Member): Unit = storeFor(member.connector).putMember(member)
  def getMember(userId: Long, serverId: Long, connector: Connector): Option[Member] = storeFor(connector).getMember(userId, serverId)
  
  
  /**
   * Adds a or updates a channel to the list of channels.
   */
  def putChannel(channel: Channel): Unit = storeFor(channel.connector).putChannel(channel)
  def getChannel(id: Long, connector: Connector): Option[Channel] = storeFor(connector).getChannel(id)
  def getChannelUsers(channelId: Long, connector: Connector): Iterator[User] = storeFor(connector).getChannelUsers(channelId)
  def getChannelUsersCount(channelId: Long, connector: Connector): Int = storeFor(connector).getChannelUsersCount(channelId)
  def registerChannelUser(channelId: Long, userId: Long, connector: Connector): Unit = storeFor(connector).registerChannelUser(channelId, userId)
  
  /**
   * Signals switching the passed channel. This allows for a caching mechanism on messages.
   */
  def switchToChannel(channel: Channel): Unit = storeFor(channel.connector).switchToChannel(channel)
}

/**
 * Definition of storage for one connector
 */
trait ConnectorStore {

  def connector: Connector
  
  /**
   * Adds or updates a user to the list of users.
   */
  def putUser(user: User): Unit
  def getUser(id: Long): Option[User]
  
  /**
   * Adds a or updates a message to the list of messages known messages.
   */
  def putMessage(message: Message): Unit
  def getMessage(messageId: Long): Option[Message]
  
  /**
   * Adds a or updates a server to the list of servers.
   */
  def putServer(server: Server): Unit
  def getServer(id: Long): Option[Server]
  def getServerChannels(serverId: Long): Seq[Channel]
  def getServerMembers(serverId: Long): Iterator[Member]
  def getServerMembersCount(serverId: Long): Int
  
  /**
   * Adds a or updates a member to the list of members.
   */
  def putMember(member: Member): Unit
  def getMember(userId: Long, serverId: Long): Option[Member]
  
  
  /**
   * Adds a or updates a channel to the list of channels.
   */
  def putChannel(channel: Channel): Unit
  def getChannel(id: Long): Option[Channel]
  def getChannelUsers(channelId: Long): Iterator[User]
  def getChannelUsersCount(channelId: Long): Int
  def registerChannelUser(channelId: Long, userId: Long): Unit
  
  /**
   * Signals switching the passed channel. This allows for a caching mechanism on messages.
   */
  def switchToChannel(channel: Channel): Unit
}