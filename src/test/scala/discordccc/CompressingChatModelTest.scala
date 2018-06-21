package discordccc

import better.files._
import com.codahale.metrics.ConsoleReporter
import com.codahale.metrics.MetricRegistry
import com.esotericsoftware.kryo.io.Input

object CompressingChatModelTest extends App {

  val model = new CompressingChatModel()
  
  val metrics = new MetricRegistry()
  val storeTimer = metrics.timer("store operation time")
  val usageMetric = metrics.histogram(s"bucket usage% ${model.statistics.members.usage.length} buckets")
  
//  val trueMembers = for {
//    file <- Seq(File("⁄r⁄Fortnite.gz"), File("A.I. Channel Unofficial.gz"), File("Discord API.gz"), File("Discord4J.gz"), File("g2.gz"), File("PUBG Official.gz"))
//    member <- file.inputStream().map { in =>
//      val input = new Input(in)
//      Iterator.continually(model.kryo.readObject(input, classOf[Vector[Member]])).takeWhile(_ => in.available > 0).flatten.to[Vector]
//    }.get
//  } yield member
  
  val members = 
  for (_ <- 0 until 150000) yield {
    val member = Member(scala.util.Random.alphanumeric.take(10).mkString,
                        scala.util.Random.alphanumeric.take(10).mkString,
                        scala.util.Random.alphanumeric.take(10).mkString,
                        Vector.fill(3)(scala.util.Random.alphanumeric.take(10).mkString),
                        0,
                        false)
    storeTimer.time(() => model.putMember(member))
    member
  }

  println(org.openjdk.jol.info.GraphLayout.parseInstance(members).toFootprint)
  
  val stats = model.statistics()
  stats.members.usage.iterator foreach (u => usageMetric.update((u * 100).toInt))
//  val (nonEmpty, addedRatio) = stats.membersUsage.foldLeft((0, 0.0)) {
//    case (t@(nonEmpty, addedRatio), 0) => t
//    case (t@(nonEmpty, addedRatio), ratio) => (nonEmpty + 1, addedRatio + ratio)
//  }
//  val avgRatio = addedRatio / nonEmpty
//  println(f"Unused buckets ${stats.membersUsage.length - nonEmpty}, Avg ratio: $avgRatio%.02f")
  ConsoleReporter.forRegistry(metrics).build().report()
  println("Overrun buckets: " + stats.members.overrunBuckets)
}
