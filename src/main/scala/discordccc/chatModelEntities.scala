package discordccc

import java.time.Instant

case class Server(id: String, name: String, description: String, location: String, imageUrl: Option[String])
case class User(id: String, name: String, bot: Boolean, extra: String, imageUrl: Option[String], friend: Boolean)
case class Channel(id: String, serverId: Option[String], name: String, topic: String,
                   canTalk: Boolean, canRead: Boolean, canSubscibe: Boolean, dmUserId: Option[String])
case class Member(userId: String, serverId: String, nickname: String, roles: Seq[String], color: Int, isOwner: Boolean)
case class Message(id: String, content: Seq[Content], created: Instant, edited: Option[Instant], attachments: Seq[Message.Attachment],
                   channelId: String, serverId: String, authorId: String) {
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
