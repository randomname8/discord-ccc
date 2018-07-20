package discordccc

import better.files._
import discordccc.model._
import headache.Snowflake
import com.codahale.metrics.ConsoleReporter
import com.codahale.metrics.MetricRegistry

object CompressingChatModelTest extends App {

  val model = new CompressingChatModel()
  
  val metrics = new MetricRegistry()
  val storeTimer = metrics.timer("store operation time")
  val usageMetric = metrics.histogram(s"bucket usage% ${model.statistics.members.usage.length} buckets")
  val entriesMetric = metrics.histogram(s"entries per bucket")
  
  val members = 
    for (_ <- 0 until 1500) yield {
      val member = Member(scala.util.Random.nextLong,
                          scala.util.Random.nextLong,
                          scala.util.Random.alphanumeric.take(10).mkString,
                          Vector.fill(3)(scala.util.Random.alphanumeric.take(5).mkString),
                          0,
                          false,
                          ConnectorRegistry.DiscordConnector,
                          connector.DiscordConnector.MemberMetadata(Array(Snowflake(1), Snowflake(2), Snowflake(3))))
      storeTimer.time(() => model.putMember(member))
      member
    }

//  println(org.openjdk.jol.info.GraphLayout.parseInstance(members).toFootprint)
  println("Size of one member" + org.openjdk.jol.info.GraphLayout.parseInstance(members.head).toFootprint)
  
  val stats = model.statistics()
  stats.members.usage.iterator foreach (u => usageMetric.update((u * 100).toInt))
  stats.members.entriesPerBucket.iterator foreach (u => entriesMetric.update((u).toInt))
  ConsoleReporter.forRegistry(metrics).build().report()
  println("Overrun buckets: " + stats.members.overrunBuckets)
  
  val users = Vector.fill(1500)(User(
      scala.util.Random.nextLong,
      scala.util.Random.alphanumeric.take(10).mkString,
      scala.util.Random.nextBoolean,
      scala.util.Random.alphanumeric.take(10).mkString,
      Some("http://" + scala.util.Random.alphanumeric.take(40).mkString),
      scala.util.Random.nextBoolean,
      ConnectorRegistry.DiscordConnector,
      null))
  
  users foreach model.putUser
}
