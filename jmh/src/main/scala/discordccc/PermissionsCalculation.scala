package discordccc

import better.files._
import headache._, JsonCodecs._
import org.openjdk.jmh.annotations._
import play.api.libs.json.Json
import scala.collection.immutable.LongMap
import scala.concurrent.duration._

@State(Scope.Thread)
class PermissionsCalculation {
  
  val readyEvent = (Json.parse(File("ready-evt.json").contentAsString())\"d").validate[GatewayEvents.Ready].get
  val guild = readyEvent.guilds.collectFirst { case Right(g) if g.name contains "PUBG" => g }.get
  
  val guildIdStr = guild.id.snowflakeString
        
  val everyoneRole = guild.roles.find(_.id == guild.id).getOrElse(throw new IllegalStateException(s"there's no permissions for everyone in guild ${guild.name}!?"))
  val guildRoles = LongMap(guild.roles.map(r => r.id -> r).toSeq:_*)
  val channelOverwrites = LongMap(guild.channels.map(c => c.id -> LongMap(c.permissionOverwrites.map(o => o.id -> o).toSeq:_*)).toSeq:_*)
  println(s"total channels ${guild.channels.length}, overrides ${channelOverwrites.size}, roles ${guildRoles.size}")
  
  val member = guild.members.head
  val stackOfOverwritesBuffer = new collection.mutable.ArrayBuffer[PermissionOverwrite](15)
  
//  @Benchmark
//  @BenchmarkMode(Array(Mode.Throughput))
//  @OutputTimeUnit(MICROSECONDS)
//  def measureMemberMapping() = {
//    val memberRoles = member.roles.map(guildRoles.apply)
//    
//    val readableChannels = guild.channels filter { c => 
//      val overwrites = channelOverwrites(c.id)
//      val overwritesForEveryone = overwrites.get(everyoneRole.id)
//      val overwritesForMemberRoles = memberRoles.flatMap(r => overwrites.get(r.id))
//      val overwritesForMember = overwrites.get(member.user.id)
//      val stackOfOverrides = overwritesForEveryone.iterator ++ overwritesForMemberRoles.iterator ++ overwritesForMember.iterator
//      
//      val permsForChat = Permissions.calculateFinalPermissions(everyoneRole.permissions, memberRoles.map(_.permissions),
//                                                               stackOfOverrides.toArray)
//      
//      Permissions.ViewChannels existsIn permsForChat
//    }
//    
//    Member(member.user.id.snowflakeString, guild.id.snowflakeString, member.nick.getOrElse(member.user.userName),
//           member.roles.map(_.snowflakeString), 0, guild.ownerId == member.user.id, readableChannels.map(_.id.snowflakeString))
//  }
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(MICROSECONDS)
  @Fork(value = 1, jvmArgsAppend = Array("-XX:+UnlockExperimentalVMOptions", "-XX:+UseJVMCICompiler"))
  def measurePermissionsCalculation() = {
    val memberRoles = member.roles.map(guildRoles.apply)
    
    guild.channels filter { c => 
      val overwrites = channelOverwrites(c.id)
      stackOfOverwritesBuffer.clear()
      overwrites.get(everyoneRole.id) foreach stackOfOverwritesBuffer.+=
      memberRoles.flatMap(r => overwrites.get(r.id)) foreach stackOfOverwritesBuffer.+=
      overwrites.get(member.user.id) foreach stackOfOverwritesBuffer.+=
      
      
      val permsForChat = Permissions.calculateFinalPermissions(everyoneRole.permissions, memberRoles.map(_.permissions),
                                                               stackOfOverwritesBuffer.toArray)
      
      Permissions.ViewChannels existsIn permsForChat
    }
  }
}
