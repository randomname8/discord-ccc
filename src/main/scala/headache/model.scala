package headache

import enumeratum.values.{IntEnum, IntEnumEntry}
import java.time.Instant

case class User(
  id: String,
  userName: String,
  discriminator: String,
  avatar: String,
  bot: Boolean,
  mfaEnabled: Option[Boolean],
  verified: Option[Boolean],
  email: Option[String]
)

sealed abstract class ExplicitContentFilterLevel(val value: Int) extends IntEnumEntry
object ExplicitContentFilterLevel extends IntEnum[ExplicitContentFilterLevel] {
  val values = findValues
  case object Disabled extends ExplicitContentFilterLevel(0)
  case object MembersWithoutRoles extends ExplicitContentFilterLevel(1)
  case object AllMembers extends ExplicitContentFilterLevel(2)
}
sealed abstract class NotificationLevel(val value: Int) extends IntEnumEntry
object NotificationLevel extends IntEnum[NotificationLevel] {
  val values = findValues
  case object AllMessages extends NotificationLevel(0)
  case object OnlyMentions extends NotificationLevel(1)
}
trait GuildDef {
  def id: Snowflake
  def name: String
  def icon: String
  def splash: String
  def ownerId: Snowflake
  def region: String
  def afkChannelId: Snowflake
  def afkTimeout: Int
  def embedEnabled: Option[Boolean]
  def embedChannelId: Option[Snowflake]
  def verificationLevel: Int
  def defaultMessageNotifications: NotificationLevel
  def explicitContentFilter: ExplicitContentFilterLevel
  def roles: Seq[Role]
  def emojis: Seq[Emoji]
  def features: Seq[String]
  def mfaLevel: Int
  def applicationId: Option[Snowflake]
  def widgetEnabled: Option[Boolean]
  def widgetChannelId: Option[Snowflake]
}
case class Guild(
  id: Snowflake,
  name: String,
  icon: String,
  splash: String,
  ownerId: Snowflake,
  region: String,
  afkChannelId: Snowflake,
  afkTimeout: Int,
  embedEnabled: Option[Boolean],
  embedChannelId: Option[Snowflake],
  verificationLevel: Int,
  defaultMessageNotifications: NotificationLevel,
  explicitContentFilter: ExplicitContentFilterLevel,
  roles: Seq[Role],
  emojis: Seq[Emoji],
  features: Seq[String],
  mfaLevel: Int,
  applicationId: Option[Snowflake],
  widgetEnabled: Option[Boolean],
  widgetChannelId: Option[Snowflake]
) extends GuildDef

case class UnavailableGuild(id: String, unavailable: Boolean)

case class GuildMember(
  user: User,
  nick: Option[String],
  roles: Seq[String],
  joinedAt: Instant,
  deaf: Boolean,
  mute: Boolean
)

object GameStatus {
  sealed abstract class Type(val value: Int) extends IntEnumEntry
  object Type extends IntEnum[Type] {
    val values = findValues
    case object Playing extends Type(0)
    case object Streaming extends Type(1)
    case object Listening extends Type(2)
  }
}
case class GameStatus(
  name: String,
  tpe: Option[GameStatus.Type],
  url: Option[String],
  details: Option[String]
)

case class GuildPresence(
  user: PresenceUser,
  game: Option[GameStatus],
  nick: Option[String],
  status: String
)
case class PresenceUser(val id: String)

case class Role(
  id: String,
  name: String,
  color: Int,
  hoist: Boolean,
  position: Int,
  permissions: Int,
  managed: Boolean,
  mentionable: Boolean
)

case class Emoji(
  id: String,
  name: String,
  roles: Seq[Role],
  requireColons: Boolean,
  managed: Boolean
)

case class VoiceState(
  guildId: Option[Snowflake],
  channelId: Option[Snowflake],
  userId: Snowflake,
  deaf: Boolean,
  mute: Boolean,
  selfDeaf: Boolean,
  selfMute: Boolean,
  suppress: Boolean
)

object Channel {
  sealed abstract class Type(val value: Int) extends IntEnumEntry
  object Type extends IntEnum[Type] {
    val values = findValues
    case object GuildText extends Type(0)
    case object Dm extends Type(1)
    case object GuildVoice extends Type(2)
    case object GroupDm extends Type(3)
    case object GuildCategory extends Type(4)
  }
}

case class Channel(
  id: Snowflake,
  tpe: Channel.Type,
  guildId: Option[Snowflake],
  name: Option[String],
  position: Option[Int],
  permissionOverwrites: Seq[Overwrite],
  topic: Option[String],
  lastMessageId: Option[Snowflake],
  bitrate: Option[Int],
  userLimit: Option[Int],
  recipients: Seq[User],
  icon: Option[String], //icon hash they say
  ownerId: Option[Snowflake],
  applicationId: Option[Snowflake],
)

case class Overwrite(id: String, tpe: String, allow: Int, deny: Int)

case class DmChannel(
  id: String,
  isPrivate: Boolean,
  recipient: User,
  lastMessageId: String
)

case class Message(
  id: String,
  channelId: String,
  author: User,
  content: String,
  timestamp: Instant,
  editedTimestamp: Option[Instant],
  tts: Boolean,
  mentionEveryone: Boolean,
  mentions: Seq[User],
  mentionRoles: Seq[String],
  attachments: Seq[Attachment],
  embeds: Seq[Embed],
  nonce: Option[String],
  pinned: Boolean,
  webhookId: Option[String]
)
case class MessageUpdate(
  id: String,
  channelId: String,
  content: Option[String],
  editedTimestamp: Option[Instant],
  tts: Option[Boolean],
  mentionEveryone: Option[Boolean],
  mentions: Seq[User],
  mentionRoles: Seq[String],
  attachments: Seq[Attachment],
  embeds: Seq[Embed],
  nonce: Option[String],
  pinned: Option[Boolean],
  webhookId: Option[String]
)

case class Embed(
  tpe: String,
  title: Option[String] = None,
  description: Option[String] = None,
  url: Option[String] = None,
  timestamp: Option[Instant] = None,
  color: Option[Int] = None,
  footer: Option[EmbedFooter] = None,
  image: Option[EmbedImage] = None,
  thumbnail: Option[EmbedThumbnail] = None,
  video: Option[EmbedVideo] = None,
  provider: Option[EmbedProvider] = None,
  author: Option[EmbedAuthor] = None,
  fields: Seq[EmbedField] = Seq.empty
)
case class EmbedThumbnail(
  url: String,
  proxyUrl: String,
  height: Int,
  width: Int
)
case class EmbedVideo(
  url: String,
  height: Int,
  width: Int
)
case class EmbedImage(
  url: String,
  proxyUrl: String,
  height: Int,
  width: Int
)
case class EmbedProvider(name: String, url: String)
case class EmbedAuthor(
  name: String,
  url: Option[String] = None,
  iconUrl: Option[String] = None,
  proxyIconUrl: Option[String] = None
)
case class EmbedFooter(
  text: String,
  iconUrl: Option[String] = None,
  proxyIconUrl: Option[String] = None
)
case class EmbedField(
  name: String,
  value: String,
  inline: Boolean
)

case class Attachment(
  id: String,
  filename: String,
  size: Int,
  url: String,
  proxyUrl: String,
  height: Option[Int] = None,
  width: Option[Int] = None
)

sealed trait Status
object Status {
  case class PlayingGame(game: String) extends Status
  case class Streaming(name: String, url: String) extends Status
  case object Empty extends Status
}

case class AudioRtpFrame(rtpSequence: Int, rtpTimestamp: Int, rtpSsrc: Int, audio: Array[Byte])