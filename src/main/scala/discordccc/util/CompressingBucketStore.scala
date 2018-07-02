package discordccc.util

import better.files.InputStreamOps
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.Arrays
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.ReentrantLock
import java.util.zip.{GZIPOutputStream}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class CompressingBucketStore[T: ClassTag, Key](val kryo: KryoPool, val metricsPrefix: String, val numberOfBuckets: Int, val maxBucketSize: Int,
                                               val cachedBuckets: Int, val parallelismLevel: Int,
                                               val key: T => Key) {
  
  private val memory = new Array[Byte](maxBucketSize * numberOfBuckets)
  val bucketsUsage = new Array[Float](numberOfBuckets)
  val bucketEntries = new Array[Int](numberOfBuckets)
  private val locks = Array.fill(parallelismLevel)(new ReentrantLock)
  private val bucketsPerLock = (numberOfBuckets / parallelismLevel).max(1)
  var overrunBuckets = scala.collection.immutable.IntMap[Array[Byte]]()
    
  val uncompressedBucketsCache = new LruMap[Int, ArrayBuffer[T]](cachedBuckets)
  val cacheHits = new AtomicLong(0)
  val cacheMisses = new AtomicLong(0)
    
  private val writerSerializer = new FilteredTraversable[T, ArrayBuffer](_ => true)
  def store(t: T): Unit = {
    val bucket = bucketOffset(key(t))
    
    val lock = locks((bucket / bucketsPerLock).min(locks.length - 1))
    lock.lock()
    try {
      val elements = readBucket(bucket, t.!=) += t
//      val elements = t +: readBucket(bucket, t.!=).filterNot(t.==)
//      val elements = readBucket(bucket, t.!=).filterNot(t.==) :+ t
      
      val baos = new ByteArrayOutputStream()
      val gzipOutput = new GZIPOutputStream(baos)
      val bytes = kryo.withInstance { serde =>
        serde.kryo.writeObject(serde.output, elements, writerSerializer)
        serde.output.flush()
        val res = serde.output.toBytes()
        serde.clear()
        res
      }
      gzipOutput.write(bytes)
      gzipOutput.close()
      
      Arrays.fill(memory, bucket * maxBucketSize, bucket * maxBucketSize + maxBucketSize, 0.toByte)
      val resByteArray = baos.toByteArray
//      println(s"${elements.size} elements have size ${resByteArray.length} and raw ${bytes.length}")
      
      if (resByteArray.length > maxBucketSize) {
        //on overrun, store to the overrun buckets
        overrunBuckets = overrunBuckets.updated(bucket, resByteArray)
      } else {
        System.arraycopy(resByteArray, 0, memory, bucket * maxBucketSize, resByteArray.length)
      }
      if (cachedBuckets > 0) uncompressedBucketsCache(bucket) = elements
      bucketsUsage(bucket) = resByteArray.length.toFloat / maxBucketSize
      bucketEntries(bucket) = elements.size
    } finally lock.unlock()
  }
    
  def retrieve(id: Key): Option[T] = readBucket(bucketOffset(id)).find(key(_) == id)
    
  private def readBucket(bucket: Int, readPredicate: T => Boolean = _ => true): ArrayBuffer[T] = {
    uncompressedBucketsCache.get(bucket) match {
      case Some(cached) => 
        cacheHits.incrementAndGet()
        cached
      case _ =>
        cacheMisses.incrementAndGet() 
        
        def readElements(bytes: Array[Byte]): ArrayBuffer[T] = kryo.withInstance { serde =>
          serde.input.setBuffer(bytes)
          val res = serde.kryo.readObject(serde.input, classOf[ArrayBuffer[T]], new FilteredTraversable[T, ArrayBuffer](readPredicate))
//          val res = serde.kryo.readObject(serde.input, classOf[ArrayBuffer[T]])
          serde.clear()
          res
        }
        
        val res: ArrayBuffer[T] = if (overrunBuckets.contains(bucket)) {
          val bytes = new ByteArrayInputStream(overrunBuckets(bucket)).asGzipInputStream().readAllBytes()
          readElements(bytes)
        } else if (memory(bucket * maxBucketSize) == 0) ArrayBuffer.empty
        else {
          val bytes = new ByteArrayInputStream(memory, bucket * maxBucketSize, maxBucketSize).asGzipInputStream().readAllBytes()
          readElements(bytes)
        }
        if (cachedBuckets > 0) uncompressedBucketsCache(bucket) = res
        res
    }
  }
  private def bucketOffset(id: Any): Int = (id.hashCode % numberOfBuckets).abs
    
  def iterator: Iterator[T] = (0 until memory.length).iterator.flatMap(b => readBucket(b))
  def clear(): Unit = {
    locks foreach (_.lock()) //must ensure noone is modifying anything
    Arrays.fill(memory, 0, memory.length, 0.toByte)
    locks foreach (_.unlock())
  }
  
  import language.higherKinds
  private class FilteredTraversable[T, C[X] <: Traversable[X]](pred: T => Boolean)(implicit ct: ClassTag[T], cbf: CanBuildFrom[C[T], T, C[T]]) extends Serializer[C[T]] {
    def write(kser: Kryo, out: Output, obj: C[T]) {
      //Write the size:
      out.writeInt(obj.size, true)
      obj.foreach { t =>
        if (pred(t)) {
          kser.writeObject(out, t)
        }
      }
    }

    def read(kser: Kryo, in: Input, cls: Class[C[T]]): C[T] = {
      val size = in.readInt(true)
      var idx = 0
      val builder = cbf()
      builder.sizeHint(size + 1)

      val rc = ct.runtimeClass
      while (idx < size) {
        val item = kser.readObject(in, rc).asInstanceOf[T]
        builder += item
        idx += 1
      }
      builder.result()
    }
  }
}