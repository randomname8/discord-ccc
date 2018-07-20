package discordccc

import better.files._
import com.codahale.metrics.ConsoleReporter
import com.codahale.metrics.MetricRegistry
import headache._, JsonCodecs._
import org.openjdk.jmh.annotations._
import play.api.libs.json.{Json}
import scala.concurrent.duration._

object RadixTreeBenchmark {

  def main(args: Array[String]): Unit = {
    
    val allMembers = File("allMembers.json").contentAsString().split("\n\n").zipWithIndex.flatMap { case (s, i) => 
        println("procesing chunk " + i)
        try Json.parse(s).\("d").validate[GatewayEvents.GuildMemberChunk].get.members
        catch { 
          case _ => 
            println("failed index " + i)
            Array.empty[GuildMember]
        }
    }
    
    println(s"read ${allMembers.length} members")
    val allNames = allMembers.map(m => m.nick.getOrElse(m.user.userName).getBytes())
    println("calculating all names total size")
    println(org.openjdk.jol.info.GraphLayout.parseInstance(allNames).toFootprint)
    
    val metricReg = new MetricRegistry
    val appendTimer = metricReg.timer("append")
    val reporter = ConsoleReporter.forRegistry(metricReg).convertDurationsTo(MICROSECONDS).build
    reporter.start(3, SECONDS)
    
    val root = new RadixTree.Root()
    allNames foreach (n => appendTimer.time(() => root.append(n)))
//    allUrls foreach (n => appendTimer.time(() => root.append(n)))
    reporter.stop()
    println("Completed")
    reporter.report()
    println("radix trie size")
    println(org.openjdk.jol.info.GraphLayout.parseInstance(root).toFootprint)
    
    val nodeStats = new Array[Int](21)
    root.elementsIterator.foreach { n => 
      nodeStats((n.length - 1) % nodeStats.length) += 1
      }
    println("nodes stats " + nodeStats.deep)
//    File("out.dot").write(root.toDotString)
  }
}
