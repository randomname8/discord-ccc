package discordccc
package model

/**
 * Model definition for a chat.
 * It supports the operations for tracking and fetching data that mutates over time.
 */
trait ChatModel {

  /**
   * Adds or updates a user to the list of users.
   */
  def putUser(user: User): Unit
  def getUser(id: Long, connector: Connector): Option[User]
  
  /**
   * Adds a or updates a message to the list of messages known messages.
   */
  def putMessage(message: Message): Unit
  def getMessage(messageId: Long, connector: Connector): Option[Message]
  
  /**
   * Adds a or updates a server to the list of servers.
   */
  def putServer(server: Server): Unit
  def getServer(id: Long, connector: Connector): Option[Server]
  def getServerChannels(serverId: Long, connector: Connector): Seq[Channel]
  def getServerMembers(serverId: Long, connector: Connector): Iterator[Member]
  def getServerMembersCount(serverId: Long, connector: Connector): Int
  
  /**
   * Adds a or updates a member to the list of members.
   */
  def putMember(member: Member): Unit
  def getMember(userId: Long, serverId: Long, connector: Connector): Option[Member]
  
  
  /**
   * Adds a or updates a channel to the list of channels.
   */
  def putChannel(channel: Channel): Unit
  def getChannel(id: Long, connector: Connector): Option[Channel]
  def getChannelUsers(channelId: Long, connector: Connector): Iterator[User]
  def getChannelUsersCount(channelId: Long, connector: Connector): Int
  def registerChannelUser(channelId: Long, userId: Long, connector: Connector): Unit
  
  /**
   * Signals switching the passed channel. This allows for a caching mechanism on messages.
   */
  def switchToChannel(channel: Channel): Unit
}

object ChatModel {
  /**
   * ProtocolConnectors are the entities responsible for integrating a particular protocol with CCC,
   * they are responsible for obtaining the Server, Channel, User and Member data.
   * 
   * This trait defines operations that the Chat model cant calculate on its own and so delegates to the connector.
   */
  trait ProtocolConnector {
    def name: String
  }
}