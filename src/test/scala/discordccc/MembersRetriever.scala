package discordccc

import better.files._
import com.codahale.metrics.ConsoleReporter
import com.codahale.metrics.MetricRegistry
import headache._
import scala.io.AnsiColor
import scala.concurrent._, ExecutionContext.Implicits._
import JsonUtils._
import GatewayEvents._

/**
 * Connect to an account and retrieve all memebers.
 */
object MembersRetriever {
  def main(args: Array[String]): Unit = {
    val token = file"test-token".contentAsString()
    
    val model = new CompressingChatModel()
    val metrics = new MetricRegistry()
    val storeTimer = metrics.timer("store operation time")
    val usageMetric = metrics.histogram(s"bucket usage% ${model.statistics.members.usage.length} buckets")
    val entriesMetric = metrics.histogram(s"entries per bucket")
    val reporter = ConsoleReporter.forRegistry(metrics).build()
    reporter.start(5, duration.SECONDS)
    
    val client = new DiscordClient(token, new DiscordClient.DiscordListener {
        def prettyPrint(js: DynJValueSelector) = JsonUtils.renderJson(js.jv.result.get, true)
        override def onUnexpectedGatewayOp(connection: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector): Unit =
          println(AnsiColor.RED + s"unexpected op: $op data: ${prettyPrint(data)}" + AnsiColor.RESET)
        override def onUnexpectedVoiceOp(connection: DiscordClient#VoiceConnection, op: Int, data: => DynJValueSelector): Unit =
          println(AnsiColor.RED + s"unexpected voice op: $op data: ${prettyPrint(data)}" + AnsiColor.RESET)
//        override def onMessageBeingSent(connection: DiscordClient#Connection, msg: String): Unit =
//          println(AnsiColor.CYAN + s"sending: $msg" + AnsiColor.RESET)

        override def onReconnecting(connection: DiscordClient#Connection, reason: DiscordClient.ReconnectReason): Unit =
          println(AnsiColor.RED + s"reconnecting $connection due to $reason" + AnsiColor.RESET)
        override def onConnectionOpened(connection: DiscordClient#Connection): Unit =
          println(s"connected $connection")
        override def onConnectionClosed(connection: DiscordClient#Connection): Unit =
          println(s"connected closed $connection")
        override def onDisconnected(connection: DiscordClient#Connection, code: Int, reason: String): Unit =
          println(AnsiColor.RED + s"disconnected $connection code: $code reason: $reason" + AnsiColor.RESET)
        override def onConnectionError(connection: DiscordClient#Connection, error: Throwable): Unit = {
          val stacktrace = Iterator.iterate(error)(_.getCause).takeWhile(_ != null).foldLeft(new StringBuilder){ (sb, ex) =>
            sb.append("caused by: ").append(ex)
            ex.getStackTrace foreach (t => sb.append("\n  ").append(t))
            sb
          }.result
          println(AnsiColor.RED_B + AnsiColor.WHITE + s"connection error $connection $stacktrace" + AnsiColor.RESET)
        }
      
        val totalUsers = new java.util.concurrent.atomic.AtomicInteger
        var ready: Ready = _
        override def onGatewayEvent(connection: DiscordClient#GatewayConnection) = {
          case ReadyEvent(evt) =>
            ready = evt
            println(AnsiColor.YELLOW + evt + AnsiColor.RESET)
            val guilds = evt.guilds.collect { case Right(g) => g }
            println(System.currentTimeMillis + " requesting guilds " + guilds.map(_.name).mkString(", "))
            guilds foreach (g => connection.sendRequestGuildMembers(g.id, "", 0))
            
          case GuildCreateEvent(evt) => 
            println(AnsiColor.YELLOW + evt + AnsiColor.RESET)
            println("requesting guilds " + evt.guild.name)
            connection.sendRequestGuildMembers(evt.guild.id, "", 0)
            
          case GuildMemberChunkEvent(evt) =>
            val totalAsOfNow = totalUsers.addAndGet(evt.members.size)
            println(System.currentTimeMillis + " " + evt.members.size + " members received for guild " + evt.guildId.snowflakeString + " total " + totalAsOfNow)
            val members = evt.members.map(m => Member(m.user.id.snowflakeString, evt.guildId.snowflakeString, m.nick.getOrElse(""), m.roles.map(_.snowflakeString), 0, false))
            
            val guildName = ready.guilds.collectFirst { case Right(g) if g.id == evt.guildId => g.name.replace("/", "⁄") }.head
            
            members foreach (m => storeTimer.time(() => model.putMember(m)))
            println(s"Processed ${members.length} members from guild $guildName")
            
            val stats = model.statistics()
            stats.members.usage foreach (u => usageMetric.update((u * 100).toInt))
            stats.members.entriesPerBucket foreach entriesMetric.update
            println("Overrun buckets: " + stats.members.overrunBuckets)
            println(f"Cache hits: ${stats.members.cacheHits}, misses: ${stats.members.cacheMisses}, ratio ${stats.members.cacheHits.toDouble / stats.members.cacheMisses}")
            
          case _ =>
        }
      })
    client.login().onComplete(println)
  }
}
