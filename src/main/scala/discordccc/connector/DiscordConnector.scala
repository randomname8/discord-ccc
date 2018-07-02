package discordccc
package connector

import discordccc.model._
import headache.{
  PermissionOverwrite,
  Permissions,
  Role,
  Snowflake
}
import headache.GatewayEvents._
import scala.collection.immutable.LongMap

object DiscordConnector {
  case class MemberMetadata(roleIds: Array[Snowflake])
  case class ChannelMetadata(permissionOverwrites: LongMap[PermissionOverwrite])
  case class ServerMetadata(ownerId: Snowflake, everyoneRole: Role, roles: LongMap[Role])
  
  val DiscordCdn = "https://cdn.discordapp.com"
}
trait DiscordConnector { self: model.Connector =>
  import DiscordConnector._
  
  // important optimization when calculating permissions
  private val threadLocalRolesBuffer = ThreadLocal.withInitial(() => new collection.mutable.ArrayBuffer[PermissionOverwrite](15))
  
  def serverMetadataClass: Class[_] = classOf[ServerMetadata]
  def channelMetadataClass: Class[_] = classOf[ChannelMetadata]
  def memberMetadataClass: Class[_] = classOf[MemberMetadata]
  def userMetadataClass: Class[_] = classOf[Unit]
  
  def calculatePermission(member: Member, channel: Channel, server: Server): Long = (member.metadata, channel.metadata, server.metadata) match {
    case (memberMetadata: MemberMetadata, channelMetadata: ChannelMetadata, serverMetadata: ServerMetadata) => 
      val memberRoles = memberMetadata.roleIds.map(serverMetadata.roles)
      val overwrites = channelMetadata.permissionOverwrites
      
      val stackOfOverwritesBuffer = threadLocalRolesBuffer.get()
      stackOfOverwritesBuffer.clear()
      overwrites.get(serverMetadata.everyoneRole.id) foreach stackOfOverwritesBuffer.+=
      memberRoles.flatMap(r => overwrites.get(r.id)) foreach stackOfOverwritesBuffer.+=
      overwrites.get(member.userId) foreach stackOfOverwritesBuffer.+=
      
      Permissions.calculateFinalPermissions(serverMetadata.everyoneRole.permissions, memberRoles.map(_.permissions),
                                            stackOfOverwritesBuffer.toArray)
    case _ => 0
  }
  def canRead(member: Member, channel: Channel, server: Server): Boolean = Permissions.ViewChannels existsIn calculatePermission(member, channel, server)
      
  
  def mapServer(guild: Guild): Server = {
    val guildRoles = LongMap(guild.roles.map(r => r.id -> r).toSeq:_*)
    Server(guild.id, guild.name, s"guild - ${guild.memberCount} members", guild.region,
           guild.icon.map(icon => s"$DiscordCdn/icons/${guild.id.snowflakeString}/$icon.png"), this,
           ServerMetadata(guild.ownerId, guildRoles(guild.id), guildRoles))
  }
  
  def mapChannel(c: headache.Channel, server: Option[Server], canRead: Boolean, canWrite: Boolean): Channel = {
    val channelName = c.name.getOrElse(c.recipients.map(_.userName).mkString(", ")) //if it has no name, it's probably a DM or group DM channel, so use the name of the participants
    val permissionsOverwrites = LongMap(c.permissionOverwrites.map(o => o.id -> o).toSeq:_*)
    val dmUserId = c.recipients match {
      case Array(user) => Some(user.id)
      case _ => None
    }
    Channel(c.id, server.map(_.id), channelName, c.topic.getOrElse(""), canRead, canWrite, true, dmUserId, this, ChannelMetadata(permissionsOverwrites))
  }
  
  def mapMember(m: headache.GuildMember, server: Server): Member = {
    val serverMetadata = server.metadata.asInstanceOf[ServerMetadata]
    val roles = m.roles.map(id => serverMetadata.roles(id))
    Member(m.user.id, server.id, m.nick.getOrElse(m.user.userName),
           roles.map(_.name), roles.find(_.color != 0).map(_.color).getOrElse(0), serverMetadata.ownerId == m.user.id,
           this, MemberMetadata(roles.map(_.id)))
  }
  
  def mapUser(u: headache.User): User = User(u.id, u.userName, u.bot, u.discriminator,
                                             u.avatar.map(a => s"$DiscordCdn/avatars/${u.id.snowflakeString}/$a.png"), true, this, null) //TODO: not everyone is a friend
}
