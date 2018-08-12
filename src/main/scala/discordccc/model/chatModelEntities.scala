package discordccc.model

import java.time.Instant

case class Server(id: Long, name: String, description: String, location: String, imageUrl: Option[String], connector: Connector) extends ConnectorEntity
case class User(id: Long, name: String, bot: Boolean, extra: String, imageUrl: Option[String], friend: Boolean, connector: Connector) extends ConnectorEntity
case class Channel(id: Long, serverId: Option[Long], name: String, topic: String, canTalk: Boolean, canRead: Boolean,
                   canSubscribe: Boolean, dmUserId: Option[Long], connector: Connector) extends ConnectorEntity
case class Member(userId: Long, serverId: Long, nickname: String, roles: Seq[String], color: Int, isOwner: Boolean,
                  connector: Connector) extends ConnectorEntity
case class Message(id: Long, content: Seq[Content], created: Instant, edited: Option[Instant], attachments: Seq[Message.Attachment],
                   channelId: Long, authorId: Long, connector: Connector, webhookID: Option[String] = None) extends ConnectorEntity {
  def originalContent = content.map(_.originalText).mkString
}
object Message {
  case class Attachment(fileName: String, url: String)
}
case class MessageUpdate(id: Long, content: Option[Seq[Content]], edited: Option[Instant], attachments: Option[Seq[Message.Attachment]],
                         channelId: Long, connector: Connector) extends ConnectorEntity

sealed trait Content {
  def originalText: String
}
object Content {
  case class Text(text: String) extends Content { override def originalText = text }
  case class InlinedImage(name: String, url: String, originalText: String, isEmoji: Boolean = false, width: Int = -1, height: Int = -1) extends Content
  case class InlinedMedia(name: String, url: String, originalText: String, isVideo: Boolean) extends Content
  case class RichLayout(title: Option[String] = None,
                        description: Seq[Content] = Seq.empty,
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
      title foreach (t => sb.append("**").append(t).append("**\\\n").append("**").append("=" * t.length).append("**\\\n"))
      description foreach (t => sb.append(t.originalText).append("\\\n"))
      image foreach (i => sb.append(i.originalText).append("\\\n"))
      url foreach (t => sb.append("[url:").append(t).append("]\\\n"))
      fields foreach { f => 
        sb.append(s"**${f.name}: **").append(f.value.originalText)
        if (f.inline) sb.append("\t") else sb.append("\\\n")
      }
      footer foreach (t => sb.append(t.originalText).append("\\\n"))
      author foreach (t => sb.append("By ").append(t.originalText).append("\\\n"))
      sb.result
    }
  }
  case class Field(name: String, value: Content, inline: Boolean)
}

