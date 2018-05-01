package headache

import prickle._
import enumeratum.values.{IntEnum, IntEnumEntry}
import java.time.Instant
import java.time.format.DateTimeFormatter
import org.json4s._
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object CustomPicklers {

  trait FullPickler[V] extends Pickler[V] with Unpickler[V]

  implicit val jvaluePickler: FullPickler[JValue] = new FullPickler[JValue] {
    override def pickle[P](o: JValue, state: PickleState)(implicit conf: PConfig[P]) = o.asInstanceOf[P]
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = Success(v.asInstanceOf[JValue])
  }
  implicit val jobjectPickler: FullPickler[JObject] = new FullPickler[JObject] {
    override def pickle[P](o: JObject, state: PickleState)(implicit conf: PConfig[P]) = o.asInstanceOf[P]
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = v match {
      case j: JObject => Success(j)
      case other => Failure(new RuntimeException(s"$other is not a JObject"))
    }
  }

  implicit val booleanPickler: FullPickler[Boolean] = new FullPickler[Boolean] {
    override def pickle[P](o: Boolean, state: PickleState)(implicit conf: PConfig[P]) = JBool(o).asInstanceOf[P]
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = v match {
      case JNull => Success(false)
      case JBool(b) => Success(b)
      case other => Failure(new RuntimeException(s"$other is not a JBool"))
    }
  }
  
  implicit val snowflakePickler: FullPickler[Snowflake] = new FullPickler[Snowflake] {
    override def pickle[P](o: Snowflake, state: PickleState)(implicit conf: PConfig[P]) = JString(o.snowflakeString).asInstanceOf[P]
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = v match {
      case JString(s) => Success(Snowflake(s))
      case JLong(l) => Success(Snowflake(l))
      case other => Failure(new RuntimeException(s"$other is not a JLong"))
    }
  }

  implicit def mapPickler[V: Pickler]: Pickler[Map[String, V]] = new Pickler[Map[String, V]] {
    override def pickle[P](v: Map[String, V], state: PickleState)(implicit config: PConfig[P]): P = {
      val fields = v.toSeq.map { case (key, value) => key -> Pickle(value, state) }
      config.makeObject(fields)
    }
  }
  implicit def mapUnpickler[V: Unpickler]: Unpickler[Map[String, V]] = new Unpickler[Map[String, V]] {
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]) = {
      v match {
        case JObject(obj) => Try { obj.map { case (k, v) => k -> Unpickle[V].from(v, state)(config.asInstanceOf[PConfig[JValue]]).get }.toMap }
        case JNull => Success(Map.empty)
        case other => Failure(new UnsupportedOperationException(s"Can't unpickle map from value: $other"))
      }
    }
  }

  implicit def seqPickler[T: Pickler]: Pickler[Seq[T]] = new Pickler[Seq[T]] {
    override def pickle[P](v: Seq[T], state: PickleState)(implicit config: PConfig[P]): P = {
      config.makeArray(v.map(Pickle(_, state)): _*)
    }
  }
  implicit def seqUnpickler[T: Unpickler]: Unpickler[Seq[T]] = new Unpickler[Seq[T]] {
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]) = {
      v match {
        case JArray(elems) => Try { elems.map(Unpickle[T].from(_, state)(config.asInstanceOf[PConfig[JValue]]).get) }
        case JNull => Success(Seq.empty)
        case other => Failure(new UnsupportedOperationException(s"Can't unpickle Seq from value: $other"))
      }
    }
  }

  implicit def optionPickler[T: Pickler]: Pickler[Option[T]] = new Pickler[Option[T]] {
    override def pickle[P](v: Option[T], state: PickleState)(implicit config: PConfig[P]): P = v.fold(config.makeNull)(Pickle(_, state))
  }
  implicit def optionUnpickler[T: Unpickler]: Unpickler[Option[T]] = new Unpickler[Option[T]] {
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]) = {
      if (config.isNull(v)) Success(None)
      else Unpickle[T].from(v, state).map(Some.apply)
    }
  }

  implicit def eitherPickler[T: Pickler, U: Pickler]: Pickler[T Either U] = new Pickler[T Either U] {
    override def pickle[P](v: T Either U, state: PickleState)(implicit config: PConfig[P]) = v.fold(Pickle(_, state), Pickle(_, state))
  }
  implicit def eitherUnpickler[T: Unpickler, U: Unpickler]: Unpickler[T Either U] = new Unpickler[T Either U] {
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]) = {
      Unpickle[U].from(v, state).map(Right.apply) orElse Unpickle[T].from(v, state).map(Left.apply)
    }
  }
  
  val fullDateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSXXXXX")
  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXXXX")
  implicit val instantPickler: FullPickler[Instant] = new FullPickler[Instant] {
    override def pickle[P](v: Instant, state: PickleState)(implicit conf: PConfig[P]) = Pickle(v.toString, state)
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = Unpickle[String].from(v, state).flatMap(s =>
      Try(Instant from fullDateTimeFormatter.parse(s)) orElse Try(Instant from dateTimeFormatter.parse(s)))
  }

  implicit def intEnumPickler[T <: IntEnumEntry](implicit enum: IntEnum[T]): FullPickler[T] = new FullPickler[T] {
    override def pickle[P](e: T, state: PickleState)(implicit conf: PConfig[P]) = Pickle(e.value, state)
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = Unpickle[Int].from(v, state).map(enum.withValue)
  }

  implicit val userPickler: FullPickler[User] = new FullPickler[User] {
    val rawPickler = Pickler.materializePickler[User]
    val rawUnpickler = Unpickler.materializeUnpickler[User]
    override def pickle[P](v: User, state: PickleState)(implicit conf: PConfig[P]) = rawPickler.pickle(v, state) match {
      case jo: JObject =>
        jo.transformField { case JField("userName", value) => JField("username", value) }.asInstanceOf[P]
    }
    override def unpickle[P](v: P, state: mutable.Map[String, Any])(implicit conf: PConfig[P]) = v match {
      case jo: JObject => rawUnpickler.unpickle(jo.transformField { case JField("username", value) => JField("userName", value) }.asInstanceOf[P], state)
      case other => Failure(new UnsupportedOperationException(s"Can't unpickle user from value: $other"))
    }
  }
  
  implicit val userSettingsPickler: FullPickler[Option[UserSettings]] = new FullPickler[Option[UserSettings]] {
    val rawUserSettingsPickler = Pickler.materializePickler[UserSettings]
    val rawUserSettingsUnpickler = Unpickler.materializeUnpickler[UserSettings]
    override def pickle[P](o: Option[UserSettings], state: PickleState)(implicit conf: PConfig[P]) = o match {
      case Some(s) => rawUserSettingsPickler.pickle(s, state)
      case _ => conf.makeObjectFrom()
    }
    override def unpickle[P](v: P, state: collection.mutable.Map[String, Any])(implicit conf: PConfig[P]) = v match {
      case JNull => Success(None)
      case JObject(Nil) => Success(None)
      case jo: JObject => rawUserSettingsUnpickler.unpickle(v, state).map(Some.apply)
      case other => Failure(new RuntimeException(s"$other is not a UserSettings"))
    }
  }
  
  //cached picklers
  implicit val guildPickler: Pickler[Guild] = Pickler.materializePickler
  implicit val guildUnpickler: Unpickler[Guild] = Unpickler.materializeUnpickler
  implicit val unavailableGuildPickler: Pickler[UnavailableGuild] = Pickler.materializePickler
  implicit val unavailableGuildUnpickler: Unpickler[UnavailableGuild] = Unpickler.materializeUnpickler
  implicit val guildMemberPickler: Pickler[GuildMember] = Pickler.materializePickler
  implicit val guildMemberUnpickler: Unpickler[GuildMember] = Unpickler.materializeUnpickler
  implicit val gameStatusPickler: Pickler[GameStatus] = Pickler.materializePickler
  implicit val gameStatusUnpickler: Unpickler[GameStatus] = Unpickler.materializeUnpickler
  implicit val guildPresencePickler: Pickler[GuildPresence] = Pickler.materializePickler
  implicit val guildPresenceUnpickler: Unpickler[GuildPresence] = Unpickler.materializeUnpickler
  implicit val guildUserPickler: Pickler[PresenceUser] = Pickler.materializePickler
  implicit val guildUserUnpickler: Unpickler[PresenceUser] = Unpickler.materializeUnpickler
  implicit val rolePickler: Pickler[Role] = Pickler.materializePickler
  implicit val roleUnpickler: Unpickler[Role] = Unpickler.materializeUnpickler
  implicit val emojiPickler: Pickler[Emoji] = Pickler.materializePickler
  implicit val emojiUnpickler: Unpickler[Emoji] = Unpickler.materializeUnpickler
  implicit val voiceStatePickler: Pickler[VoiceState] = Pickler.materializePickler
  implicit val voiceStateUnpickler: Unpickler[VoiceState] = Unpickler.materializeUnpickler
  implicit val channelStatePickler: Pickler[Channel] = Pickler.materializePickler
  implicit val channelStateUnpickler: Unpickler[Channel] = Unpickler.materializeUnpickler
  implicit val overwritePickler: Pickler[Overwrite] = Pickler.materializePickler
  implicit val overwriteUnpickler: Unpickler[Overwrite] = Unpickler.materializeUnpickler
  implicit val messageUpdatePickler: Pickler[MessageUpdate] = Pickler.materializePickler
  implicit val messageUpdateUnpickler: Unpickler[MessageUpdate] = Unpickler.materializeUnpickler
  implicit val embedThumbnailPickler: Pickler[EmbedThumbnail] = Pickler.materializePickler
  implicit val embedThumbnailUnpickler: Unpickler[EmbedThumbnail] = Unpickler.materializeUnpickler
  implicit val embedVideoPickler: Pickler[EmbedVideo] = Pickler.materializePickler
  implicit val embedVideoUnpickler: Unpickler[EmbedVideo] = Unpickler.materializeUnpickler
  implicit val embedImagePickler: Pickler[EmbedImage] = Pickler.materializePickler
  implicit val embedImageUnpickler: Unpickler[EmbedImage] = Unpickler.materializeUnpickler
  implicit val embedProviderPickler: Pickler[EmbedProvider] = Pickler.materializePickler
  implicit val embedProviderUnpickler: Unpickler[EmbedProvider] = Unpickler.materializeUnpickler
  implicit val embedAuthorPickler: Pickler[EmbedAuthor] = Pickler.materializePickler
  implicit val embedAuthorUnpickler: Unpickler[EmbedAuthor] = Unpickler.materializeUnpickler
  implicit val embedFooterPickler: Pickler[EmbedFooter] = Pickler.materializePickler
  implicit val embedFooterUnpickler:Unpickler[EmbedFooter] = Unpickler.materializeUnpickler
  implicit val embedFieldPickler: Pickler[EmbedField] = Pickler.materializePickler
  implicit val embedFieldUnpickler: Unpickler[EmbedField] = Unpickler.materializeUnpickler
  implicit val embedPickler: Pickler[Embed] = Pickler.materializePickler
  implicit val attachmentUnpickler: Unpickler[Attachment] = Unpickler.materializeUnpickler
  implicit val attachmentPickler: Pickler[Attachment] = Pickler.materializePickler
  implicit val embedUnpickler: Unpickler[Embed] = Unpickler.materializeUnpickler
  implicit val messagePickler: Pickler[Message] = Pickler.materializePickler
  implicit val messageUnpickler: Unpickler[Message] = Unpickler.materializeUnpickler
  
  //gateway events
  implicit val readStatePickler: Pickler[GatewayEvents.ReadState] = Pickler.materializePickler
  implicit val readStateUnpickler: Unpickler[GatewayEvents.ReadState] = Unpickler.materializeUnpickler
  implicit val gatewayEventGuildPickle: Pickler[GatewayEvents.Guild] = Pickler.materializePickler
  implicit val gatewayEventGuildUnpickle: Unpickler[GatewayEvents.Guild] = Unpickler.materializeUnpickler
  implicit val readyGuildPickle: Pickler[UnavailableGuild Either GatewayEvents.Guild] = eitherPickler
  implicit val readyGuildUnpickle: Unpickler[UnavailableGuild Either GatewayEvents.Guild] = eitherUnpickler
  implicit val readyPickler: FullPickler[GatewayEvents.Ready] = new FullPickler[GatewayEvents.Ready] {
    override def pickle[P](o: GatewayEvents.Ready, state: PickleState)(implicit conf: PConfig[P]) = 
      conf.makeObjectFrom(("v", Pickle(o.v, state)),
                          ("user", Pickle(o.user, state)),
                          ("privateChannels", Pickle(o.privateChannels, state)),
                          ("guilds", Pickle(o.guilds, state)),
                          ("userSettings", Pickle(o.userSettings, state)),
                          ("readState", Pickle(o.readState, state)),
                          ("_trace", Pickle(o._trace, state)))
    override def unpickle[P](obj: P, state: collection.mutable.Map[String, Any])(implicit conf: PConfig[P]) = for {
      v <- conf.readObjectFieldNum(obj, "v")
      user <- conf.readObjectField(obj, "user").flatMap(Unpickle[User].from(_, state))
      privateChannels <- conf.readObjectField(obj, "privateChannels").flatMap(Unpickle[Seq[Channel]].from(_, state))
      guilds <- conf.readObjectField(obj, "guilds").flatMap(Unpickle[Seq[UnavailableGuild Either GatewayEvents.Guild]].from(_, state))
      userSettings <- conf.readObjectField(obj, "userSettings").flatMap(Unpickle[Option[UserSettings]](userSettingsPickler).from(_, state))
      readState <- conf.readObjectField(obj, "readState").flatMap(Unpickle[Seq[GatewayEvents.ReadState]].from(_, state))
      _trace <- conf.readObjectField(obj, "_trace").flatMap(Unpickle[Seq[String]].from(_, state))
    } yield GatewayEvents.Ready(v.toInt, user, privateChannels, guilds, userSettings, readState, _trace)
  }
}
