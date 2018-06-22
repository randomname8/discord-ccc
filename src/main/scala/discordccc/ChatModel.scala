package discordccc

/**
 * Model definition for a chat.
 * It supports the operations for tracking and fetching data that mutates over time.
 */
trait ChatModel {

  /**
   * Adds or updates a user to the list of users.
   */
  def putUser(user: User): Unit
  def findUser(id: String): Option[User]
  
  /**
   * Adds a or updates a message to the list of messages known messages.
   */
  def putMessage(message: Message): Unit
  def findMessage(messageId: String): Option[Message]
  
  /**
   * Adds a or updates a server to the list of servers.
   */
  def putServer(server: Server): Unit
  def findServer(id: String): Option[Server]
  
  /**
   * Adds a or updates a member to the list of members.
   */
  def putMember(member: Member): Unit
  def getMember(userId: String, serverId: String): Option[Member]
  
  /**
   * Adds a or updates a channel to the list of channels.
   */
  def putChannel(channel: Channel): Unit
  def findChannel(id: String): Option[Channel]
  def getServerChannels(serverId: String): Seq[Channel]
  
  /**
   * Signals switching the passed channel. This allows for a caching mechanism on messages.
   */
  def switchToChannel(channel: Channel): Unit
}
