package discordccc

import better.files._
import headache.GatewayOp
import headache.{DiscordClient, GatewayEvents, Json4sUtils}, GatewayEvents._, Json4sUtils.DynJValueSelector
import javafx.application.Platform
import scala.collection.mutable.Buffer

class DiscordEventHandler(ui: DiscordChat) extends DiscordClient.DiscordListener {

  val logFile = File("messagesLog")
  logFile.clear()
  
  override def onGatewayOp(conn: DiscordClient#GatewayConnection, op: GatewayOp, data: => DynJValueSelector): Unit = {
    logFile.append(s"Received $op " + Json4sUtils.renderJson(data.jv, true) + "\n")
  }
  override def onUnexpectedGatewayOp(conn: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector): Unit = {
    logFile.append(s"Received unexpected $op " + Json4sUtils.renderJson(data.jv, true) + "\n")
  }
  override def onMessageBeingSent(connection: DiscordClient#Connection, msg: String): Unit = println(s"Sending $msg")
  override def onReconnecting(conn: DiscordClient#Connection, reason: DiscordClient.ReconnectReason): Unit = println(s"Reconnecting due to $reason")
  override def onConnectionOpened(conn: DiscordClient#Connection): Unit = {}
  override def onConnectionClosed(conn: DiscordClient#Connection): Unit = println(s"Connection closed $conn")
  override def onDisconnected(conn: DiscordClient#Connection, code: Int, reason: String): Unit = println(s"Connection $conn disconnected code $code: $reason")
  override def onConnectionError(conn: DiscordClient#Connection, error: Throwable): Unit = println(s"Connection $conn failed $error\n" + error.getStackTrace.mkString("  ", "\n  ", ""))
  override def onGatewayEvent(conn: DiscordClient#GatewayConnection): GatewayEvent => Any = {
    case Ready(evt) =>
      println("User " + evt.user + "\nsettings " + evt.userSettings)
      Platform.runLater { () =>
        val channels = evt.privateChannels
        channels.filter(_.tpe == headache.Channel.Type.Dm) foreach (c => ui.dmChannelModified(mapChannel("dm" + c.id)(c), true))
        channels.filter(_.tpe == headache.Channel.Type.GroupDm) foreach (c => ui.dmChannelModified(mapChannel("gdm" + c.id)(c), true))
        println("DMs/GroupDms added: " + channels.mkString("\n"))
        evt.guilds foreach {
          case Right(guild) => addGuild(guild)
          case _ =>
        }
      }
      
    case GuildCreate(evt) => Platform.runLater { () => addGuild(evt.guild) }
      
    case _ =>
  }
  
  private val discordCdn = "https://cdn.discordapp.com/"
  private def addGuild(guild: Guild): Unit = {
    val serverId = guild.id.snowflakeString 
    val server = Server(serverId, guild.name, s"guild - ${guild.memberCount} members", guild.region,
                        guild.icon.map(icon => s"$discordCdn/${serverId}/$icon"), guild.channels.map(mapChannel(serverId)).toBuffer)
    ui.addServer(server)
  }
  private def mapChannel(serverId: String)(c: headache.Channel): Channel = {
    val name = c.name.getOrElse(c.recipients.map(_.userName).mkString(", ")) //if it has no name, it's probably a DM or group DM channel, so use the name of the participants
    Channel(c.id.snowflakeString, serverId, name, c.topic.getOrElse(""), Buffer.empty, true)
  }
}
