package discordccc

import better.files._
import discordccc.model._, ConnectorRegistry.DiscordConnector
import headache.{
  DiscordClient,
  GatewayOp,
  GatewayEvents,
  JsonUtils,
  Permissions,
}, GatewayEvents._, JsonUtils.DynJValueSelector
import java.util.concurrent.Executors
import javafx.application.Platform
import scala.concurrent._, duration._, ExecutionContext.Implicits._

class DiscordEventHandler(ui: DiscordChat) extends DiscordClient.DiscordListener {

  var readyEvent: Ready = _
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
  override def onDisconnected(conn: DiscordClient#Connection, code: Int, reason: String): Unit = println(s"Connection $conn disconnected code $code: $reason")
  override def onConnectionError(conn: DiscordClient#Connection, error: Throwable): Unit = println(s"Connection $conn failed $error\n" + error.getStackTrace.mkString("  ", "\n  ", ""))
  override def onGatewayEvent(conn: DiscordClient#GatewayConnection): GatewayEvent => Any = {
    case ge@ReadyEvent(evt) =>
      readyEvent = evt
//      logFile append Json4sUtils.renderJson(ge.payload().d.jv.get, true)
      val guilds = evt.guilds.collect { case Right(g) => g }
      guilds foreach (guild => println(s"Guild ${guild.name}: ${guild.memberCount}"))
      Platform.runLater { () =>
        evt.privateChannels foreach (c => addChannel(c, None, true, true))
        guilds foreach addGuild
      }
      guilds foreach (g => conn.sendRequestGuildMembers(g.id))
      
    case GuildCreateEvent(evt) => Platform.runLater { () => addGuild(evt.guild) }
      
    case ge@GatewayEvent(EventType.GuildMemberChunk, _) => Future { //parse the message in a different thread from the websocket
        val GuildMemberChunkEvent(evt) = ge
        
        val server: Server = ui.chatModel.getServer(evt.guildId, DiscordConnector).get
        
        for (m <- evt.members) {
          ui.chatModel.putMember(DiscordConnector.mapMember(m, server))
          ui.chatModel.putUser(DiscordConnector.mapUser(m.user))
        }
        println(s"Processed ${ui.chatModel.getServerMembersCount(server.id, DiscordConnector)} members for guild ${server.name}")
      }
      
    case MessageCreateEvent(msg) => 
//      println(msg)
      
    case _ =>
  }
  
  private def addGuild(guild: Guild): Unit = {
    val server = DiscordConnector.mapServer(guild)
    ui.chatModel.putServer(server)
    ui.addServer(server)

    val myUserAsMember = DiscordConnector.mapMember(guild.members.find(_.user.id == readyEvent.user.id).get, server)
    
    guild.channels foreach { c =>
      val mappedChannel = {
        val res = DiscordConnector.mapChannel(c, Some(server), false, false)
        val perms = DiscordConnector.calculatePermission(myUserAsMember, res, server)
        res.copy(canRead = Permissions.ViewChannels existsIn perms, canTalk = Permissions.SendMessages existsIn perms)
      }
      
      ui.chatModel.putChannel(mappedChannel)
      ui.channelModifiedInServer(mappedChannel, server, true)
    }
    
    //if we have members already (for whatever reason) add them
      
    for (m <- guild.members) {
      ui.chatModel.putMember(DiscordConnector.mapMember(m, server))
      ui.chatModel.putUser(DiscordConnector.mapUser(m.user))
    }
  }
  private def addChannel(c: headache.Channel, server: Option[Server], canRead: Boolean, canWrite: Boolean): Unit = {
    val chatChannel = DiscordConnector.mapChannel(c, server, canRead, canWrite)
    ui.chatModel.putChannel(chatChannel)
    c.tpe match {
      case headache.Channel.Type.Dm => ui.dmChannelModified(chatChannel, true)
      case headache.Channel.Type.GroupDm => ui.groupChannelModified(chatChannel, true)
      case headache.Channel.Type.GuildText => ui.channelModifiedInServer(chatChannel, server.get, true)
      case headache.Channel.Type.GuildCategory =>
      case headache.Channel.Type.GuildVoice =>
    }
    val users = c.recipients.map(DiscordConnector.mapUser)
    users foreach { u =>
      ui.chatModel.putUser(u)
      ui.chatModel.registerChannelUser(c.id, u.id, DiscordConnector)
    }
  }
}
