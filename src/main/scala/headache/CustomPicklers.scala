package headache

import prickle._
import enumeratum.values.{IntEnum, IntEnumEntry}
import java.time.Instant
import java.time.format.DateTimeFormatter
import org.json4s._
import scala.util.{Failure, Success, Try}

object CustomPicklers {

  trait FullPickler[V] extends Pickler[V] with Unpickler[V]

  implicit val jvaluePickler = new FullPickler[JValue] {
    override def pickle[P](o: JValue, state)(implicit conf) = o.asInstanceOf[P]
    override def unpickle[P](v, state)(implicit conf) = Success(v.asInstanceOf[JValue])
  }
  implicit val jobjectPickler = new FullPickler[JObject] {
    override def pickle[P](o: JObject, state)(implicit conf) = o.asInstanceOf[P]
    override def unpickle[P](v, state)(implicit conf) = v match {
      case j: JObject => Success(j)
      case other => Failure(new RuntimeException(s"$other is not a JObject"))
    }
  }

  implicit val booleanPickler = new FullPickler[Boolean] {
    override def pickle[P](o: Boolean, state)(implicit conf) = JBool(o).asInstanceOf[P]
    override def unpickle[P](v, state)(implicit conf) = v match {
      case JNull => Success(false)
      case JBool(b) => Success(b)
      case other => Failure(new RuntimeException(s"$other is not a JBool"))
    }
  }

  implicit def mapPickler[V: Pickler: Unpickler] = new FullPickler[Map[String, V]] {
    override def pickle[P](v: Map[String, V], state)(implicit config: PConfig[P]): P = {
      val fields = v.toSeq.map { case (key, value) => key -> Pickle(value, state) }
      config.makeObject(fields)
    }
    override def unpickle[P](v: P, state)(implicit config: PConfig[P]) = {
      v match {
        case JObject(obj) => Try { obj.map { case (k, v) => k -> Unpickle[V].from(v, state)(config.asInstanceOf[PConfig[JValue]]).get }.toMap }
        case JNull => Success(Map.empty)
        case other => Failure(new UnsupportedOperationException(s"Can't unpickle map from value: $other"))
      }
    }
  }

  implicit def seqPickler[T: Pickler: Unpickler] = new FullPickler[Seq[T]] {
    override def pickle[P](v: Seq[T], state)(implicit config: PConfig[P]): P = {
      config.makeArray(v.map(Pickle(_, state)): _*)
    }
    override def unpickle[P](v: P, state)(implicit config: PConfig[P]) = {
      v match {
        case JArray(elems) => Try { elems.map(Unpickle[T].from(_, state)(config.asInstanceOf[PConfig[JValue]]).get) }
        case JNull => Success(Seq.empty)
        case other => Failure(new UnsupportedOperationException(s"Can't unpickle Seq from value: $other"))
      }
    }
  }

  implicit def optionPickler[T: Pickler: Unpickler] = new FullPickler[Option[T]] {
    override def pickle[P](v: Option[T], state)(implicit config: PConfig[P]): P = v.fold(config.makeNull)(Pickle(_, state))
    override def unpickle[P](v: P, state)(implicit config: PConfig[P]) = {
      if (config.isNull(v)) Success(None)
      else Unpickle[T].from(v, state).map(Some.apply)
    }
  }

  val fullDateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSXXXXX")
  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXXXX")
  implicit val instantPickler = new FullPickler[Instant] {
    override def pickle[P](v: Instant, state)(implicit conf) = Pickle(v.toString, state)
    override def unpickle[P](v, state)(implicit conf) = Unpickle[String].from(v, state).flatMap(s =>
      Try(Instant from fullDateTimeFormatter.parse(s)) orElse Try(Instant from dateTimeFormatter.parse(s)))
  }

  implicit def intEnumPickler[T <: IntEnumEntry](implicit enum: IntEnum[T]) = new FullPickler[T] {
    override def pickle[P](e: T, state)(implicit conf) = Pickle(e.value, state)
    override def unpickle[P](v, state)(implicit conf) = Unpickle[Int].from(v, state).map(enum.withValue)
  }

  implicit val userPickler = new FullPickler[User] {
    val rawPickler = Pickler.materializePickler[User]
    val rawUnpickler = Unpickler.materializeUnpickler[User]
    override def pickle[P](v: User, state)(implicit conf) = rawPickler.pickle(v, state) match {
      case jo: JObject =>
        jo.transformField { case JField("userName", value) => JField("username", value) }.asInstanceOf[P]
    }
    override def unpickle[P](v, state)(implicit conf) = v match {
      case jo: JObject => rawUnpickler.unpickle(jo.transformField { case JField("username", value) => JField("userName", value) }.asInstanceOf[P], state)
      case other => Failure(new UnsupportedOperationException(s"Can't unpickle user from value: $other"))
    }
  }

  //cached picklers
  implicit val guildPickler = implicitly[Pickler[Guild]]
  implicit val guildUnpickler = implicitly[Unpickler[Guild]]
  implicit val unavailableGuildPickler = implicitly[Pickler[UnavailableGuild]]
  implicit val unavailableGuildUnpickler = implicitly[Unpickler[UnavailableGuild]]
  implicit val guildMemberPickler = implicitly[Pickler[GuildMember]]
  implicit val guildMemberUnpickler = implicitly[Unpickler[GuildMember]]
  implicit val gameStatusPickler = implicitly[Pickler[GameStatus]]
  implicit val gameStatusUnpickler = implicitly[Unpickler[GameStatus]]
  implicit val guildPresencePickler = implicitly[Pickler[GuildPresence]]
  implicit val guildPresenceUnpickler = implicitly[Unpickler[GuildPresence]]
  implicit val guildUserPickler = implicitly[Pickler[PresenceUser]]
  implicit val guildUserUnpickler = implicitly[Unpickler[PresenceUser]]
  implicit val rolePickler = implicitly[Pickler[Role]]
  implicit val roleUnpickler = implicitly[Unpickler[Role]]
  implicit val emojiPickler = implicitly[Pickler[Emoji]]
  implicit val emojiUnpickler = implicitly[Unpickler[Emoji]]
  implicit val voiceStatePickler = implicitly[Pickler[VoiceState]]
  implicit val voiceStateUnpickler = implicitly[Unpickler[VoiceState]]
  implicit val overwritePickler = implicitly[Pickler[Overwrite]]
  implicit val overwriteUnpickler = implicitly[Unpickler[Overwrite]]
  implicit val dmChannelPickler = implicitly[Pickler[DmChannel]]
  implicit val dmChannelUnpickler = implicitly[Unpickler[DmChannel]]
  implicit val messageUpdatePickler = implicitly[Pickler[MessageUpdate]]
  implicit val messageUpdateUnpickler = implicitly[Unpickler[MessageUpdate]]
  implicit val embedThumbnailPickler = implicitly[Pickler[EmbedThumbnail]]
  implicit val embedThumbnailUnpickler = implicitly[Unpickler[EmbedThumbnail]]
  implicit val embedVideoPickler = implicitly[Pickler[EmbedVideo]]
  implicit val embedVideoUnpickler = implicitly[Unpickler[EmbedVideo]]
  implicit val embedImagePickler = implicitly[Pickler[EmbedImage]]
  implicit val embedImageUnpickler = implicitly[Unpickler[EmbedImage]]
  implicit val embedProviderPickler = implicitly[Pickler[EmbedProvider]]
  implicit val embedProviderUnpickler = implicitly[Unpickler[EmbedProvider]]
  implicit val embedAuthorPickler = implicitly[Pickler[EmbedAuthor]]
  implicit val embedAuthorUnpickler = implicitly[Unpickler[EmbedAuthor]]
  implicit val embedFooterPickler = implicitly[Pickler[EmbedFooter]]
  implicit val embedFooterUnpickler = implicitly[Unpickler[EmbedFooter]]
  implicit val embedFieldPickler = implicitly[Pickler[EmbedField]]
  implicit val embedFieldUnpickler = implicitly[Unpickler[EmbedField]]
  implicit val embedPickler = implicitly[Pickler[Embed]]
  implicit val attachmentUnpickler = implicitly[Unpickler[Attachment]]
  implicit val attachmentPickler = implicitly[Pickler[Attachment]]
  implicit val embedUnpickler = implicitly[Unpickler[Embed]]
  implicit val messagePickler = implicitly[Pickler[Message]]
  implicit val messageUnpickler = implicitly[Unpickler[Message]]
}
