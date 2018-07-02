package discordccc

import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import com.twitter.chill.ScalaKryoInstantiator
import discordccc.model._
import java.io.ByteArrayOutputStream
import java.util.zip.GZIPOutputStream
import org.scalatest.Assertions._

object CompressionTest {
  
  def main(args: Array[String]): Unit = {
    scala.util.Random.alphanumeric.take(10).mkString
    val users = Vector.fill(150000)(User(
        scala.util.Random.nextLong,
        scala.util.Random.alphanumeric.take(10).mkString,
        scala.util.Random.nextBoolean,
        scala.util.Random.alphanumeric.take(10).mkString,
        Some("http://" + scala.util.Random.alphanumeric.take(40).mkString),
        scala.util.Random.nextBoolean,
        ConnectorRegistry.DiscordConnector,
        ()))
    
    val totalSizeJol = org.openjdk.jol.info.GraphLayout.parseInstance(users).toFootprint
    println(totalSizeJol)
    
    val instantiator = new ScalaKryoInstantiator()
    val kryo = instantiator.newKryo()
    val baos = new ByteArrayOutputStream()
    
    {
      kryo.writeObject(new Output(baos), users)
      val raw = baos.toByteArray
      println("raw size " + raw.length)
      baos.reset()
      val zout = new GZIPOutputStream(baos, 1024*1024)
      zout.write(raw)
      zout.finish()
      val zipped = baos.toByteArray()
      println("compressed " + zipped.length)
      val users2 = kryo.readObject(new Input(raw), classOf[Vector[User]])
      assert(users == users2)
    }
    
//    {
//      //json test
//      baos.reset()
//      val zout = new GZIPOutputStream(baos, 1024 * 1024)
//      zout write Json.stringify(Json.toJson(users)).getBytes()
//      zout.finish()
//      val zipped = baos.toByteArray()
//      println("compressed " + zipped.length)
//    }
    
    
    
    
  }
  
}
