package discordccc.util

import better.files._
import java.util.Collections
import java.net.URI
import java.nio.file.FileSystems
import scala.collection.JavaConverters._
import scala.collection.immutable.IntMap
import scala.reflect.{ClassTag, classTag}

/**
 * Storage that maps each instance to an entry in a zip file.
 * Parallelism level controls how the domain is partitioned to enable concurrent access.
 */
class FileBackedBucketStorage[T: ClassTag](val parallelismLevel: Int, val storageName: String, val kryo: KryoPool, val idFunction: T => String) {

  val bucketsDirectory = file"${File.temp}/$storageName/"
  bucketsDirectory.createDirectories()
  private class Bucket(val file: File) {
    FileSystems.newFileSystem(URI.create(s"jar:file:$file"), Map("create" -> "true").asJava).close //touch the file so that it is created
    def managed[R](f: File => R): R = synchronized {
      FileSystems.newFileSystem(URI.create(s"jar:file:$file"), Collections.emptyMap[String, String]).autoClosed(fs => f(fs.getPath("/")))
    }
  }
  private val buckets = IntMap((0 until parallelismLevel).map(i => i -> new Bucket(file"$bucketsDirectory/$i.zip")):_*)
  
  def put(t: T): Unit = {
    val id = idFunction(t)
    val bucket = buckets((id.hashCode % parallelismLevel).abs)
    val bytes = kryo.toBytesWithoutClass(t)
    bucket.managed { root => (root/id).writeByteArray(bytes) }
  }
  def get(id: String): Option[T] = {
    val bucket = buckets((id.hashCode % parallelismLevel).abs)
    bucket.managed { root =>
      val targetFile = root/id
      if (targetFile.exists) Some(kryo.fromBytes(targetFile.byteArray, classTag[T].runtimeClass).asInstanceOf[T])
      else None
    }
  }
  def remove(id: String): Unit = buckets((id.hashCode % parallelismLevel).abs).managed(root => (root/id).delete())
  
//  def iterator: Iterator[T] = buckets.values.iterator.flatMap(_.children).flatMap(f => get(f.name))
}
