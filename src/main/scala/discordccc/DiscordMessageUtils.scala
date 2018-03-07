package discordccc

import net.dv8tion.jda.core.entities.impl.ReceivedMessage
import net.dv8tion.jda.core.entities.{Message, MessageEmbed}

object DiscordMessageUtils {

  private val receivedMessageEmbedsField = classOf[ReceivedMessage].getDeclaredField("embeds")
  receivedMessageEmbedsField.setAccessible(true)
  def setEmebds(message: Message, emebds: java.util.List[MessageEmbed]): Unit = {
    message match {
      case message: ReceivedMessage => receivedMessageEmbedsField.set(message, emebds)
      case _ => throw new IllegalArgumentException(s"Messsage[${message.getClass}] is not a ReceivedMessage")
    }
  }
}
