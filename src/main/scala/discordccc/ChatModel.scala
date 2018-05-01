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
   * Fetches the latest messages for a given channel, the returned list is ordered from oldest to newest but fromIndex represents the newest
   * offset, and upTo means how far back to go. The messages seen are only those added via putMessage
   * 
   * ----------------------------------------------
   * |                        |        |          |
   * ----------------------------------------------
   * oldest                  upTo    fromIndex    newest
   */
  def getLatestMessagesForChannel(channel: Channel, fromIndex: Int = 0, upTo: Int = 100): Seq[Message]
}
