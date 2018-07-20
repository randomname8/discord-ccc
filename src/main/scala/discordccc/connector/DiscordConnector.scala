package discordccc
package connector

import discordccc.model._
import headache.{
  DiscordClient,
  GatewayOp,
  GatewayEvents,
  GuildMember,
  JsonUtils,
  PermissionOverwrite,
  Permissions,
  Role,
  Snowflake,
  NoSnowflake,
}, GatewayEvents._, JsonUtils.DynJValueSelector
import java.util.Arrays
import org.agrona.BitUtil
import org.agrona.collections.{LongArrayList, Long2ObjectHashMap, Long2LongHashMap}
import org.asynchttpclient.AsyncHttpClient
import scala.collection.JavaConverters._
import scala.collection.mutable.{TreeSet}
import scala.concurrent.{Future, ExecutionContext}, ExecutionContext.Implicits.global

object DiscordConnector {
  /*custom classes for state tracking, optimized for discord*/
  private[DiscordConnector] case class DUser(name: Array[Byte], bot: Boolean, friend: Boolean, discriminator: Short, avatar: Array[Byte])
  private[DiscordConnector] case class ChannelData(name: Array[Byte], guildId: Snowflake, lastMessage: Snowflake, permissionOverwrites: Long2ObjectHashMap[PermissionOverwrite])
  
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
  private[this] val guilds = new Long2ObjectHashMap[Guild](10, 0.7f)
  private[this] val channels = new Long2ObjectHashMap[ChannelData](200, 0.5f)
  private[this] val allRoles = new Long2ObjectHashMap[Role](200, 0.5f)
  
  private[this] var allUsers: Long2ObjectHashMap[DUser] = _ //delay instantiation until we have more information on how many members there'll be
  private[this] val guildUserNickname = new Long2ObjectHashMap[Long2ObjectHashMap[Array[Byte]]](10, 0.7f)
  private[this] val guildUsersRoles = new Long2ObjectHashMap[Long2ObjectHashMap[Array[Role]]](10, 0.7f)
  private[this] val guildMembers = new Long2ObjectHashMap[LongArrayList](10, 0.7f)
  private[this] val privateChannelMembers = new Long2ObjectHashMap[Array[Snowflake]]()
  private[this] val emptyLongMapInstance = new Long2ObjectHashMap()
  private def emptyLongMap[T]: Long2ObjectHashMap[T] = emptyLongMapInstance.asInstanceOf[Long2ObjectHashMap[T]]
  private[this] val noRoles = Array.empty[Role]
  
  def calculatePermission(userId: Long, memberRoles: Array[Role], channelId: Long, serverId: Long): Long =  {
    val overwrites = channels.get(channelId).permissionOverwrites
      
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
  def getServer(id: Long): Option[Server] = Option(guilds.get(id)).map(mapServer)
  def getChannel(id: Long): Option[Channel] = guilds.asScala.values.flatMap(s => s.channels.find(_.id == id).map(c => mapChannel(c, Some(s.id), true, true))).headOption
  def getChannels(): Seq[Channel] = guilds.asScala.values.flatMap(s => s.channels.map(c => mapChannel(c, Some(s.id), true, true))).to[Vector]
  def getMember(userId: Long, channelId: Long): Option[Member] = Option(allUsers.get(userId)).map { user =>
    val channel = channels.get(channelId)
    val serverId = channel.guildId
    val roles = guildUsersRoles.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, noRoles)
    Member(userId, serverId, new String(guildUserNickname.getOrDefault(serverId, emptyLongMap).getOrDefault(userId, user.name)),
           roles.map(_.name), roles.sortBy(_.position).find(_.color != 0).map(_.color).getOrElse(0),
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
            if Permissions.ViewChannels existsIn calculatePermission(userId, roles, channel.id, serverId)
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
                        roles.map(_.name), roles.sortBy(_.position).find(_.color != 0).map(_.color).getOrElse(0),
                        guilds.get(serverId).ownerId == userId, DiscordConnector.this))
          }
        }
      case _ =>
        /*dm channel*/
        val memberIds: Array[Snowflake] = privateChannelMembers.getOrDefault(channel.id, Array.empty[Snowflake])
        memberIds.map(i => Right(getUser(i).get))
    }
  }
  def getUser(id: Long): Option[User] = Option(allUsers.get(id)).map(duser => 
    User(id, new String(duser.name), duser.bot, duser.discriminator.toString,
         Option(duser.avatar).map(a => s"$DiscordCdn/avatars/${Snowflake(id).snowflakeString}/${BitUtil.toHex(a)}.png").
         orElse(Some(s"$DiscordCdn/embed/avatars/${duser.discriminator % 5}.png")), duser.friend, this))
  
  
  
  def sendMessage(channel: Channel, content: Content): Future[Message] = {
    client.channels.createMessage(Snowflake(channel.id), content.originalText).map(mapMessage)
  }
  def getLastMessages(channel: Channel, from: Option[Long] = None, limit: Option[Int]): Future[Seq[Message]] =
    client.channels.getMessages(Snowflake(channel.id),
                                before = from.map(Snowflake.apply) orElse Option(channels.get(channel.id)).map(_.lastMessage) getOrElse NoSnowflake,
                                limit = limit.getOrElse(100)).map(_.map(mapMessage).reverse)
  
  
  
//  val logFile = File("messagesLog.json")
//  logFile.clear()

//  implicit val eventsProcessor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(r => new Thread(r, "chunks processor")))
  
  override def onGatewayOp(conn: DiscordClient#GatewayConnection, op: GatewayOp, data: => DynJValueSelector): Unit = {
//    logFile.append(JsonUtils.renderJson(data.jv.get, true) + "\n")
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
      
      
    case GuildCreateEvent(evt) => addGuild(evt.guild)
    case ChannelCreateEvent(evt) => 
      val guildIdAndMyMember = evt.channel.guildId.map { gid => 
        val guild = guilds.get(gid)
        val myMember = guild.members.find(_.user.id == botData.user.id).get
        gid -> myMember
      }
      addChannel(evt.channel, guildIdAndMyMember)
      
    case ge@GatewayEvent(EventType.GuildMemberChunk, _) =>  //parse the message in a different thread from the websocket
      val GuildMemberChunkEvent(evt) = ge
      for (m <- evt.members) {
        allUsers.put(m.user.id, mapUser(m.user))
        m.nick.foreach(n => guildUserNickname.asScala.getOrElseUpdate(evt.guildId, new Long2ObjectHashMap).put(m.user.id, n.getBytes))
        if (m.roles.nonEmpty) guildUsersRoles.asScala.getOrElseUpdate(evt.guildId, new Long2ObjectHashMap).put(m.user.id, m.roles.map(allRoles.get))
        guildMembers.get(evt.guildId) add m.user.id
      }
      println(s"Processed ${guildMembers.get(evt.guildId).size} members for guild ${guilds.get(evt.guildId).name}")
      
      
    case ge@MessageCreateEvent(evt) =>
      if (evt.message.embeds.nonEmpty) println(JsonUtils.renderJson(ge.payload().jv.get, true))
      channels.put(evt.message.channelId, channels.get(evt.message.channelId).copy(lastMessage = evt.message.id))
      val toNotify = MessageEvent(mapMessage(evt.message), Created, this)
      for (l <- listeners; if l.isDefinedAt(toNotify)) l(toNotify)
      
      
//    case ge@GatewayEvent(EventType.MessageCreate, payload) =>
//      try {
//        val MessageCreateEvent(msg) = ge
//      } catch {
//        case e: NoSuchElementException => println("Failed parsing message: " + e + "\nPayload:\n" + JsonUtils.renderJson(payload().jv.get, true))
//      }
      
    case _ =>
  }
  
  private def addGuild(guild: Guild): Unit = {
    guilds.put(guild.id, guild)
    guildMembers.put(guild.id, new LongArrayList(guild.memberCount, Long.MinValue))
    guild.roles.foreach(r => allRoles.put(r.id, r))
    
    val server = mapServer(guild)
    val serverEvt = ServerEvent(server, Created, this)
    for (l <- listeners; if l.isDefinedAt(serverEvt)) l(serverEvt)
    
    val myMember = guild.members.find(_.user.id == botData.user.id).get
    guild.channels foreach (c => addChannel(c, Some(guild.id -> myMember)))

  }
  private def addChannel(c: headache.Channel, guildIdAndMyMember: Option[(Snowflake, GuildMember)]): Unit = {
    val (server, canRead, canWrite) = guildIdAndMyMember match {
      case Some((guildId, myMember)) =>
        channels.put(c.id, ChannelData(c.name.map(_.getBytes).orNull, guildId, c.lastMessageId.getOrElse(NoSnowflake), newLong2ObjectHashMap(c.permissionOverwrites)(_.id)))
        val perms = calculatePermission(botData.user.id, myMember.roles.map(allRoles.get), c.id, guildId)
        (Some(guildId), Permissions.ViewChannels existsIn perms, Permissions.SendMessages existsIn perms)
      case _ =>
        channels.put(c.id, ChannelData(c.name.map(_.getBytes).orNull, NoSnowflake, c.lastMessageId.getOrElse(NoSnowflake), emptyLongMap))
        c.recipients.foreach(u => allUsers.put(u.id, mapUser(u)))
        privateChannelMembers.put(c.id, c.recipients.map(_.id))
        (None, true, true) //if there is no server, then it is a group or a dm channel.
    }
    val mappedChannel = mapChannel(c, server, canRead, canWrite)
    val channelEvt = ChannelEvent(mappedChannel, Created, this)
    for (l <- listeners; if l.isDefinedAt(channelEvt)) l(channelEvt)
  }
  
  private def newLong2ObjectHashMap[T](entries: Array[T])(id: T => Long): Long2ObjectHashMap[T] = {
    val res = new Long2ObjectHashMap[T]((entries.length * 1.1f).toInt, 0.9f)
    entries foreach (e => res.put(id(e), e))
    res
  }
  
  private def mapServer(guild: Guild): Server = {
    Server(guild.id, guild.name, s"guild - ${guild.memberCount} members", guild.region,
           guild.icon.map(icon => s"$DiscordCdn/icons/${guild.id.snowflakeString}/$icon.png"), this)
  }
  
  private def mapChannel(c: headache.Channel, serverId: Option[Snowflake], canRead: Boolean, canWrite: Boolean): Channel = {
    val channelName = c.name.getOrElse(c.recipients.map(_.userName).mkString(", ")) //if it has no name, it's probably a DM or group DM channel, so use the name of the participants
//    val permissionsOverwrites = LongMap(c.permissionOverwrites.map(o => o.id -> o).toSeq:_*)
    val dmUserId = c.recipients match {
      case Array(user) => Some(user.id)
      case _ => None
    }
    Channel(c.id, serverId, channelName, c.topic.getOrElse(""), canRead, canWrite, true, dmUserId, this)
  }
  
  private def mapUser(u: headache.User): DUser = DUser(u.userName.getBytes, u.bot, true /*TODO: not everyone is a friend*/, u.discriminator.toShort,
                                                       u.avatar.map(BitUtil.fromHex).getOrElse(null))
  
  private def mapMessage(m: headache.Message): Message = {
    m.embeds.foreach(e => println("Processing embed: " + e))
    val content = Content.Text(m.content) +: m.embeds.map(embed => Content.RichLayout(
        title = embed.title,
        description = embed.description.map(Content.Text),
        url = embed.url,
        timestamp = embed.timestamp,
        color = embed.color,
        footer = embed.footer.map(f => Content.RichLayout(title = Some(f.text), image = f.iconUrl.map(u => Content.InlinedImage(u, u, "")))),
        image = embed.image.map(i => Content.InlinedImage(i.url, i.url, "", width = i.width, height = i.height)),
        thumbnail = embed.thumbnail.map(i => Content.InlinedImage(i.url, i.url, "", width = i.width, height = i.height)),
        author = embed.author.map(a => Content.RichLayout(title = Some(a.name), url = a.url, image = a.iconUrl.map(i => Content.InlinedImage(i, i, "")))),
        fields = embed.fields.map(f => Content.Field(f.name, Content.Text(f.value), f.inline))
      ))
    Message(m.id, content, m.timestamp, m.editedTimestamp, m.attachments.map(a => Message.Attachment(a.filename, a.url)), m.channelId, m.author.id, this)
  }
}
