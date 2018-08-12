package discordccc
package connector

import discordccc.model._
import headache.{
  DiscordClient,
  Embed,
  EmbedAuthor,
  EmbedField,
  EmbedImage,
  EmbedFooter,
  EmbedThumbnail,
  GatewayOp,
  GatewayEvents,
  JsonUtils,
  PermissionOverwrite,
  Permissions,
  Role,
  Snowflake,
  NoSnowflake,
}, GatewayEvents._, JsonUtils.DynJValueSelector
import java.time.Instant
import java.util.Arrays
import org.agrona.BitUtil
import org.agrona.collections.{LongArrayList, Long2ObjectHashMap}
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.request.body.multipart.Part
import scala.collection.JavaConverters._
import scala.collection.mutable.{TreeSet}
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._

object DiscordConnector {
  /*custom classes for state tracking, optimized for discord*/
  private[DiscordConnector] case class DUser(name: Array[Byte], bot: Boolean, friend: Boolean, discriminator: Short, avatar: Array[Byte])
  private[DiscordConnector] case class ChannelData(name: Array[Byte], topic: Array[Byte], guildId: Snowflake, lastMessage: Snowflake,
                                                   permissionOverwrites: Long2ObjectHashMap[PermissionOverwrite],
                                                   canRead: Boolean, canWrite: Boolean, dmUser: Snowflake)
  private[DiscordConnector] case class GuildData(name: Array[Byte], lastMemberCount: Int, ownerId: Snowflake, icon: Option[String], region: Array[Byte],
                                                 myRoles: Array[Snowflake])
  
  val DiscordCdn = "https://cdn.discordapp.com"
  
  def main(args: Array[String]): Unit = {
    import better.files._
    import play.api.libs.json._
    import headache.JsonCodecs._, headache.JsonUtils._
//    val ahc = DiscordClient.newDefaultAsyncHttpClient
    val dc = new DiscordConnector(File("test-token").contentAsString(), null)
    dc.onGatewayEvent(null)(GatewayEvent(EventType.Ready, () => Json.parse(File("jmh/ready-evt.json").contentAsString()).dyn))
    
    File("jmh/allMembers.json").contentAsString().split("\n\n").zipWithIndex.foreach { case (s, i) => 
        println("procesing chunk " + i)
        try dc.onGatewayEvent(null)(GatewayEvent(EventType.GuildMemberChunk, () => Json.parse(s).dyn))
        catch { case e => println("failed index " + i + e) }
    }
    
    val members = dc.getMembers(dc.getChannel(210938552563925002l).get)
    members foreach println
  }
}
class DiscordConnector(token: String, ahc: AsyncHttpClient) extends Connector with DiscordClient.DiscordListener {
  import DiscordConnector._, Connector._
  
  // important optimization when calculating permissions
  private[this] val threadLocalRolesBuffer = ThreadLocal.withInitial(() => new collection.mutable.ArrayBuffer[PermissionOverwrite](15))
  val client = {
    val identity = DiscordClient.ClientIdentity("Windows", "strife v1.0", "strife")
    new DiscordClient(token, this, ahc, clientIdentity = identity)
  }
  implicit val discordBackpressureStrategy = headache.rest.BackPressureStrategy.Retry(3)
  private case class BotData(user: headache.User)
  private[this] var botData: BotData = _
  private[this] val guilds = new Long2ObjectHashMap[GuildData](10, 0.7f)
  private[this] val channels = new Long2ObjectHashMap[ChannelData](200, 0.5f)
  private[this] val allRoles = new Long2ObjectHashMap[Role](200, 0.5f)
  
  private[this] var allUsers: Long2ObjectHashMap[DUser] = _ //delay instantiation until we have more information on how many members there'll be
  private[this] val guildUserNickname = new Long2ObjectHashMap[Long2ObjectHashMap[Array[Byte]]](10, 0.7f)
  private[this] val guildUsersRoles = new Long2ObjectHashMap[Long2ObjectHashMap[Array[Role]]](10, 0.7f)
  private[this] val guildMembers = new Long2ObjectHashMap[LongArrayList](10, 0.7f)
  private[this] val privateChannelMembers = new Long2ObjectHashMap[Array[Long]]() //Array[Long] is being passed to generic instead of Array[Snowflake] because scala will think that Array[Snowflake] is a Array[Object] due to generics mismatch
  private[this] val emptyLongMapInstance = new Long2ObjectHashMap()
  private def emptyLongMap[T]: Long2ObjectHashMap[T] = emptyLongMapInstance.asInstanceOf[Long2ObjectHashMap[T]]
  private[this] val noRoles = Array.empty[Role]
  
  private def optSnowflake(s: Snowflake) = if (s == NoSnowflake) None else Some(s)
  
  private[this] implicit val generalExecutionContext = ExecutionContext.fromExecutorService {
    val tf: java.util.concurrent.ThreadFactory = r => {
      val t = new Thread(r)
      t.setDaemon(true)
      t
    }
    val runtimeCores = Runtime.getRuntime.availableProcessors
    val r = new java.util.concurrent.ThreadPoolExecutor(runtimeCores, runtimeCores, 30l, SECONDS, new java.util.concurrent.LinkedBlockingQueue[Runnable](), tf)
    r.allowCoreThreadTimeOut(true)
    r
  }
  private[this] val chunksProcessorExecutionContext = ExecutionContext.fromExecutorService {
    val tf: java.util.concurrent.ThreadFactory = r => {
      val t = new Thread(r)
      t.setDaemon(true)
      t
    }
    val r = new java.util.concurrent.ThreadPoolExecutor(1, 1, 30l, SECONDS, new java.util.concurrent.LinkedBlockingQueue[Runnable](), tf)
    r.allowCoreThreadTimeOut(true)
    r
  }
  
  def calculatePermission(userId: Long, memberRoles: Array[Role], channelId: Long, serverId: Long, overwrites: Long2ObjectHashMap[PermissionOverwrite]): Long =  {
    val everyoneRoles = allRoles.get(serverId)
    
    val stackOfOverwritesBuffer = threadLocalRolesBuffer.get()
    stackOfOverwritesBuffer.clear()
    Option(overwrites.get(serverId)) foreach stackOfOverwritesBuffer.+=
    memberRoles.flatMap(r => Option(overwrites.get(r.id))) foreach stackOfOverwritesBuffer.+=
    Option(overwrites.get(userId)) foreach stackOfOverwritesBuffer.+=
      
    Permissions.calculateFinalPermissions(everyoneRoles.permissions, memberRoles.map(_.permissions),
                                          stackOfOverwritesBuffer.toArray)
  }
  
  override def init() = client.login(Some(1))
  def getServer(id: Long): Option[Server] = Option(guilds.get(id)).map(mapServer(id, _))
  def getChannel(id: Long): Option[Channel] = channels.asScala.find(_._1 == id).map(entry => mapChannel(entry._1, entry._2))
  def getChannels(): Seq[Channel] = channels.asScala.map(entry => mapChannel(entry._1, entry._2)).to[Vector]
  def getMember(userId: Long, channelId: Long): Option[Member] = Option(allUsers.get(userId)).map { user =>
    val channel = channels.get(channelId)
    val serverId = channel.guildId
    val roles = guildUsersRoles.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, noRoles)
    Member(userId, serverId, new String(guildUserNickname.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, user.name)),
           roles.map(_.name), roles.sortBy(-_.position).find(_.color != 0).map(_.color).getOrElse(0),
           Option(guilds.get(serverId)).map(_.ownerId == userId).getOrElse(false), DiscordConnector.this)
  }
  def getMembers(channel: Channel): IndexedSeq[Member Either User] = {
    channel.serverId match {
      case Some(serverId) =>
        /*For detecting which users can be listed in a channel, we take two routes, first check
         if the @everyone role can access it, in which case we won't bother filtering and all we need to do is sort,
         otherwise, we only iterate the list of users with roles, beacuse only they have a chance. This *should* speed
         up filtered channels as well as unfiltered ones (because we wont calculate permissions for those*/
    
        val everyonePerms = Permissions.calculateFinalPermissions(allRoles.get(serverId).permissions, Array.empty[Long],
                                                                  Option(channels.get(channel.id).permissionOverwrites.get(serverId)).toArray)
        
        val ordering: Ordering[(Long, Int)] = {
          case ((u1Id, u1Role), (u2Id, u2Role)) => 
            val roleDiff = u2Role - u1Role
            roleDiff match {
              case 0 =>
                var u1name = guildUserNickname.getOrDefault(serverId, emptyLongMap).get(u1Id)
                if (u1name == null) u1name = allUsers.get(u1Id).name
                var u2name = guildUserNickname.getOrDefault(serverId, emptyLongMap).get(u2Id)
                if (u2name == null) u2name = allUsers.get(u2Id).name
                Arrays.compare(u1name, u2name) match {
//                  case 0 => (u1Id - u2Id).toInt
                  case 0 => 0
                  case other => other
                }
              case _ => roleDiff
            }
        }
        
        val usersInChannel = new TreeSet[(Long, Int)]()(ordering)
        
        if (Permissions.ViewChannels.existsIn(everyonePerms)) {
          guildMembers.get(serverId).forEachOrderedLong(memberId => {
              usersInChannel += memberId -> guildUsersRoles.getOrDefault(serverId, emptyLongMap).getOrDefault(memberId, noRoles).
              withFilter(_.hoist).map(_.position).foldLeft(0)((a, b) => if (a >= b) a else b)
            })
        } else {
          for {
            (userId, roles) <- guildUsersRoles.getOrDefault(serverId, emptyLongMap).asScala
            if Permissions.ViewChannels existsIn calculatePermission(userId, roles, channel.id, serverId, channels.get(channel.id).permissionOverwrites)
          } usersInChannel += userId.longValue -> roles.withFilter(_.hoist).map(_.position).foldLeft(0)((a, b) => if (a >= b) a else b)
        }

        val compactUsers = usersInChannel.iterator.map(_._1).toArray
        new IndexedSeq[Member Either User] {
          override def length = compactUsers.length
          override def apply(i: Int) = {
            val userId = compactUsers(i)
            val user = allUsers.get(userId)
            val roles = guildUsersRoles.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, noRoles)
            Left(Member(userId, serverId, new String(guildUserNickname.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, user.name)),
                        roles.map(_.name), roles.sortBy(-_.position).find(_.color != 0).map(_.color).getOrElse(0),
                        guilds.get(serverId).ownerId == userId, DiscordConnector.this))
          }
        }
      case _ =>
        /*dm channel*/
        val memberIds = privateChannelMembers.getOrDefault(channel.id, Array.empty[Long])
        memberIds.map(i => Right(getUser(i).get))
    }
  }
  def getUser(id: Long): Option[User] = Option(allUsers.get(id)).map(duser => 
    User(id, new String(duser.name), duser.bot, duser.discriminator.toString,
         Option(duser.avatar).map(a => s"$DiscordCdn/avatars/${Snowflake(id).snowflakeString}/${BitUtil.toHex(a)}").
         orElse(Some(s"$DiscordCdn/embed/avatars/${duser.discriminator % 5}.png")), duser.friend, this))
  
  
  
  def sendMessage(channel: Channel, content: Content, attachments: Seq[Part], progressListener: (Long, Long, Long) => Unit): Future[Message] = {
    content match {
      case Content.Text(t) => client.channels.createMessage(Snowflake(channel.id), t, attachments = attachments.toArray, progressListener = progressListener).map(mapMessage)
      case rl: Content.RichLayout => 
        val e = Embed(
          tpe = "rich",
          title = rl.title,
          description = Some(rl.description.map(_.originalText).mkString("\n")).filter(_.nonEmpty),
          url = rl.url,
          timestamp = Some(Instant.now),
          color = rl.color,
          footer = rl.footer.map { case footer: Content.RichLayout => EmbedFooter(footer.title.get, footer.image.map(_.url)); case footer => EmbedFooter(footer.originalText) },
          image = rl.image.map(image => EmbedImage(image.url, proxyUrl = null, height = image.height, width = image.width)),
          thumbnail = rl.thumbnail.map(image => EmbedThumbnail(image.url, proxyUrl = null, height = image.height, width = image.width)),
          author = rl.author.map { case author: Content.RichLayout => EmbedAuthor(author.title.get, author.image.map(_.url)); case author => EmbedAuthor(author.originalText) },
          fields = rl.fields.map(f => EmbedField(f.name, f.value.originalText, f.inline)).toArray
        )
        client.channels.createMessage(Snowflake(channel.id), null, embed = e, attachments = attachments.toArray, progressListener = progressListener).map(mapMessage)
    }
  }
  def getLastMessages(channel: Channel, from: Option[Long] = None, limit: Option[Int]): Future[Seq[Message]] = {
//    println("Fetching messages for channel " + channel.name + ". Before index: " + (from.map(Snowflake.apply) orElse Option(channels.get(channel.id)).map(_.lastMessage) getOrElse NoSnowflake).snowflakeString)
    from match {
      case None => 
        client.channels.getMessages(Snowflake(channel.id),
                                    around = Option(channels.get(channel.id)).map(_.lastMessage) getOrElse NoSnowflake,
                                    limit = limit.getOrElse(100)).map(_.map(mapMessage).reverse)
      case Some(idx) =>
        client.channels.getMessages(Snowflake(channel.id),
                                    before = Snowflake(idx),
                                    limit = limit.getOrElse(100)).map(_.map(mapMessage).reverse)
    }
  }
  def markAsRead(message: Message): Future[Unit] = client.channels.ack(Snowflake(message.channelId), Snowflake(message.id))
  
  def getCustomEmoji(id: Long) = Some(s"$DiscordCdn/emojis/${Snowflake(id).snowflakeString}.png")
  
  
  val logFile = better.files.File("messagesLog.json")
  logFile.clear()

//  implicit val eventsProcessor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(r => new Thread(r, "chunks processor")))
  
  override def onGatewayOp(conn: DiscordClient#GatewayConnection, op: GatewayOp, data: => DynJValueSelector): Unit = {
    logFile.append(JsonUtils.renderJson(data.jv.get, true) + "\n")
  }
  override def onUnexpectedGatewayOp(conn: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector): Unit = {
    println(s"Received unexpected $op " + JsonUtils.renderJson(data.jv.get, true) + "\n")
  }
  override def onMessageBeingSent(connection: DiscordClient#Connection, msg: String): Unit = println(s"Sending $msg")
  override def onReconnecting(conn: DiscordClient#Connection, reason: DiscordClient.ReconnectReason): Unit = println(s"Reconnecting due to $reason")
  override def onConnectionOpened(conn: DiscordClient#Connection): Unit = {}
  override def onConnectionClosed(conn: DiscordClient#Connection): Unit = println(s"Connection closed $conn")
  override def onDisconnected(conn: DiscordClient#Connection, code: Int, reason: String): Unit = {
    println(s"Connection $conn disconnected code $code: $reason")
  }
  override def onConnectionError(conn: DiscordClient#Connection, error: Throwable): Unit = println(s"Connection $conn failed $error\n" + error.getStackTrace.mkString("  ", "\n  ", ""))
  override def onGatewayEvent(conn: DiscordClient#GatewayConnection): GatewayEvent => Any = {
    case ge@ReadyEvent(evt) =>
      botData = BotData(evt.user)
//      logFile append Json4sUtils.renderJson(ge.payload().d.jv.get, true)
      val guilds = evt.guilds.collect { case Right(g) => g }
      guilds foreach (guild => println(s"Guild ${guild.name}: ${guild.memberCount}"))
      //having the guilds information, let's initialize the allUsers collection
      allUsers = new Long2ObjectHashMap((guilds.map(_.memberCount).sum * 1.1).toInt + 100, 0.9f)
      evt.privateChannels.foreach(c => addChannel(c, None))
      guilds foreach addGuild
      if (conn != null) guilds foreach (g => conn.sendRequestGuildMembers(g.id))
      
      
    case GuildCreateEvent(evt) => 
      addGuild(evt.guild)
      conn.sendRequestGuildMembers(evt.guild.id)
    case ChannelCreateEvent(evt) => 
      val guildIdAndMyMember = evt.channel.guildId.map(gid => gid -> guilds.get(gid).myRoles)
      addChannel(evt.channel, guildIdAndMyMember)
      
    case ge@GatewayEvent(EventType.GuildMemberChunk, _) =>  //parse the message in a different thread from the websocket
      Future {
        val GuildMemberChunkEvent(evt) = ge //do the json parsing in the general execution context
        Future {
          for (m <- evt.members) {
            allUsers.put(m.user.id, mapUser(m.user))
            m.nick.foreach(n => guildUserNickname.asScala.getOrElseUpdate(evt.guildId, new Long2ObjectHashMap).put(m.user.id, n.getBytes))
            if (m.roles.nonEmpty) guildUsersRoles.asScala.getOrElseUpdate(evt.guildId, new Long2ObjectHashMap).put(m.user.id, m.roles.map(allRoles.get))
            guildMembers.get(evt.guildId) add m.user.id
          }
          println(s"Processed ${guildMembers.get(evt.guildId).size} members for guild ${new String(guilds.get(evt.guildId).name)}")
        }(chunksProcessorExecutionContext) //modify always from the same thread
      }
      
      
    case ge@MessageCreateEvent(evt) =>
//      if (evt.message.embeds.nonEmpty) {
//        val json = JsonUtils.renderJson(ge.payload().jv.get, true)
//        if (json.contains("outube")) println(json)
//      }
      Option(channels.get(evt.message.channelId)).fold(println("Message arrived with no matching channel? " + evt.message)) { data =>
//        val fullChannel = mapChannel(evt.message.channelId, data)
//        println("channel " + fullChannel.name + " last message id " + evt.message.id.snowflakeString)
        channels.put(evt.message.channelId, data.copy(lastMessage = evt.message.id))
        val toNotify = MessageCreatedEvent(mapMessage(evt.message), this)
        for (l <- listeners; if l.isDefinedAt(toNotify)) l(toNotify)
      }
    case ge@MessageUpdateEvent(evt) =>
      Option(channels.get(evt.message.channelId)).fold(println("Message arrived with no matching channel? " + evt.message)) { data =>
//        val fullChannel = mapChannel(evt.message.channelId, data)
//        println("channel " + fullChannel.name + " last message id " + evt.message.id.snowflakeString)
        channels.put(evt.message.channelId, data.copy(lastMessage = evt.message.id))
        val content = Some(mapContent(evt.message.content, evt.message.embeds)).filter(_.nonEmpty)
        val update = discordccc.model.MessageUpdate(evt.message.id, content, evt.message.editedTimestamp,
                                                    Some(mapAttachments(evt.message.attachments)).filter(_.nonEmpty), evt.message.channelId, this)
        val toNotify = MessageUpdatedEvent(update, this)
        for (l <- listeners; if l.isDefinedAt(toNotify)) l(toNotify)
      }
      
//    case ge@GatewayEvent(EventType.MessageCreate, payload) =>
//      try {
//        val MessageCreateEvent(msg) = ge
//      } catch {
//        case e: NoSuchElementException => println("Failed parsing message: " + e + "\nPayload:\n" + JsonUtils.renderJson(payload().jv.get, true))
//      }
      
    case _ =>
  }
  
  private def addGuild(guild: Guild): Unit = {
    val myMember = guild.members.find(_.user.id == botData.user.id).get
    val guildData = GuildData(guild.name.getBytes, guild.memberCount, guild.ownerId, guild.icon, guild.region.getBytes, myMember.roles)
    guilds.put(guild.id, guildData)
    guildMembers.put(guild.id, new LongArrayList(guild.memberCount, Long.MinValue))
    guild.roles.foreach(r => allRoles.put(r.id, r))
    
    val server = mapServer(guild.id, guildData)
    val serverEvt = ServerEvent(server, Created, this)
    for (l <- listeners; if l.isDefinedAt(serverEvt)) l(serverEvt)
    
    guild.channels.filter(_.tpe == headache.Channel.Type.GuildText) foreach (c => addChannel(c, Some(guild.id -> guildData.myRoles)))

  }
  private def addChannel(c: headache.Channel, guildIdAndMyRoles: Option[(Snowflake, Array[Snowflake])]): Unit = {
    val channelData = guildIdAndMyRoles match {
      case Some((guildId, myRoles)) =>
        val overwrites = newLong2ObjectHashMap(c.permissionOverwrites)(_.id)
        val perms = calculatePermission(botData.user.id, myRoles.map(allRoles.get), c.id, guildId, overwrites)
        val (canRead, canWrite) = (Permissions.ViewChannels existsIn perms, Permissions.SendMessages existsIn perms)
        val channelData = ChannelData(c.name.map(_.getBytes).orNull, c.topic.map(_.getBytes).orNull, guildId,
                                      c.lastMessageId.getOrElse(NoSnowflake), overwrites,
                                      canRead, canWrite, NoSnowflake)
        channels.put(c.id, channelData)
        channelData
      case _ =>
        val dmUserId = if (c.recipients.size == 1) c.recipients.head.id else NoSnowflake
        val channelData = ChannelData(c.name.map(_.getBytes).orNull, null, NoSnowflake, c.lastMessageId.getOrElse(NoSnowflake), emptyLongMap,
                                      true, true, dmUserId)
        channels.put(c.id, channelData)
        c.recipients.foreach(u => allUsers.put(u.id, mapUser(u)))
        privateChannelMembers.put(c.id, c.recipients.map(_.id: Long))
        channelData
    }
    val mappedChannel = mapChannel(c.id, channelData)
    val channelEvt = ChannelEvent(mappedChannel, Created, this)
    for (l <- listeners; if l.isDefinedAt(channelEvt)) l(channelEvt)
  }
  
  private def newLong2ObjectHashMap[T](entries: Array[T])(id: T => Long): Long2ObjectHashMap[T] = {
    val res = new Long2ObjectHashMap[T]((entries.length * 1.1f).toInt, 0.9f)
    entries foreach (e => res.put(id(e), e))
    res
  }
 
  private def mapServer(id: Long, guild: GuildData): Server = {
    Server(id, new String(guild.name), s"guild - ${guild.lastMemberCount} members", new String(guild.region),
           guild.icon.map(icon => s"$DiscordCdn/icons/${Snowflake(id).snowflakeString}/$icon.png"), this)
  }
  
  private def mapChannel(id: Long, data: ChannelData): Channel = {
    val channelName = Option(data.name).map(new String(_)).getOrElse { 
      privateChannelMembers.get(id).map { uid =>
        Option(allUsers.get(uid)).map(u => new String(u.name)).getOrElse("Unk. user " + uid)
      }.mkString(", ")
    }
//    val permissionsOverwrites = LongMap(c.permissionOverwrites.map(o => o.id -> o).toSeq:_*)
    val dmUserId = optSnowflake(data.dmUser)
    Channel(id, optSnowflake(data.guildId), channelName, Option(data.topic).map(new String(_)).getOrElse(""), data.canRead, data.canWrite, true, dmUserId, this)
  }
  
  private def mapUser(u: headache.User): DUser = DUser(u.userName.getBytes, u.bot, true /*TODO: not everyone is a friend*/, u.discriminator.toShort,
                                                       u.avatar.map(s => BitUtil.fromHex(s.stripPrefix("a_"))).getOrElse(null))
  
  private def mapContent(content: Option[String], embeds: Array[Embed]): Seq[Content] = {
    (content.map(Content.Text.apply) ++ embeds.map { embed =>
        val colsMaxFields = embed.fields.foldLeft(Vector.empty[EmbedField] -> 0) {
          case ((acc, inlinedRun), field) if !field.inline => (acc :+ field) -> 0
          case ((acc, inlinedRun), field) if field.inline && inlinedRun < 3 => (acc :+ field) -> (inlinedRun + 1)
          case ((acc, inlinedRun), field) if field.inline && inlinedRun == 3 => (acc :+ field.copy(inline = false)) -> 0
        }._1
        Content.RichLayout(
          title = embed.title,
          description = (embed.description.map(Content.Text) ++ embed.video.map(video => Content.InlinedMedia(embed.title.getOrElse(""), video.url, "", true))).toSeq,
          url = embed.url,
          timestamp = embed.timestamp,
          color = embed.color,
          footer = embed.footer.map(f => Content.RichLayout(title = Some(f.text), image = f.iconUrl.map(u => Content.InlinedImage(u, u, "")))),
          image = embed.image.map(i => Content.InlinedImage(i.url, i.url, "", width = i.width, height = i.height)),
          thumbnail = embed.thumbnail.map(i => Content.InlinedImage(i.url, i.url, "", width = i.width, height = i.height)),
          author = embed.author.map(a => Content.RichLayout(title = Some(a.name), url = a.url, image = a.iconUrl.map(i => Content.InlinedImage(i, i, "")))).orElse(
            embed.provider.map(prov => Content.RichLayout(title = Some(prov.name), url = prov.url))),
          fields = colsMaxFields.map(f => Content.Field(f.name, Content.Text(f.value), f.inline))
        )
      }).to[Vector]
  }
  private def mapAttachments(attachments: Array[headache.Attachment]): Seq[Message.Attachment] = attachments.map(a => Message.Attachment(a.filename, a.url)) 
  private def mapMessage(m: headache.Message): Message = {
    Message(m.id, mapContent(Option(m.content), m.embeds), m.timestamp, m.editedTimestamp, mapAttachments(m.attachments), m.channelId, m.author.id, this, m.webhookId)
  }
}
