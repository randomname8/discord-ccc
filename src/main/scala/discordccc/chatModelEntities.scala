package discordccc

import java.time.Instant
import scala.collection.mutable.Buffer

case class Server(val id: String, var name: String, var description: String, var location: String, var imageUrl: Option[String], val channels: Buffer[Channel])
case class User(id: String, name: String, bot: Boolean, extra: String, imageUrl: Option[String])
case class Channel(val id: String, val serverId: String, var name: String, var topic: String, val members: Buffer[Member], var canTalk: Boolean)
case class Member(val userId: String, val serverId: String, var nickname: String, val roles: Buffer[String], var color: Int, val isOwner: Boolean)
case class Message(id: String, content: Seq[Content], created: Instant, edited: Option[Instant], attachments: Seq[Message.Attachment],
                   channelId: String, serverId: String) {
  def originalContent = content.map(_.originalText).mkString
}
object Message {
  case class Attachment(fileName: String, url: String)
}

sealed trait Content {
  def originalText: String
}
object Content {
  case class Text(text: String) extends Content { override def originalText = text }
  case class InlinedImage(name: String, url: String, originalText: String, isEmoji: Boolean) extends Content
  case class InlinedMedia(name: String, url: String, originalText: String, isVideo: Boolean) extends Content
  case class RichLayout(title: Option[String] = None,
                        description: Option[Content] = None,
                        url: Option[String] = None,
                        timestamp: Option[Instant] = None,
                        color: Option[Int] = None,
                        footer: Option[Content] = None,
                        image: Option[InlinedImage] = None,
                        thumbnail: Option[InlinedImage] = None,
                        author: Option[Content] = None,
                        fields: Seq[Field] = Seq.empty) extends Content {
    def originalText = {
      val sb = new StringBuilder()
      title foreach (t => sb.append("**").append(t).append("**\\\n"))
      description foreach (t => sb.append(t.originalText).append("**\\\n"))
      image foreach (i => sb.append(i.originalText).append("**\\\n"))
      url foreach (t => sb.append("[url:").append(t).append("]\\\n"))
      fields foreach { f => 
        sb.append(s"**${f.name}: **").append(f.value.originalText)
        if (f.inline) sb.append("\t") else sb.append("\\\n")
      }
      footer foreach (t => sb.append(t.originalText).append("**\\\n"))
      author foreach (t => sb.append("By ").append(t.originalText).append("**\\\n"))
      sb.result
    }
  }
  case class Field(name: String, value: Content, inline: Boolean)
}
