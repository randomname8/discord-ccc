package discordccc.util

import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.KryoBase
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.Arrays
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

class CompressingBucketStore[T](val kryo: KryoBase, val metricsPrefix: String, val numberOfBuckets: Int, val maxBucketSize: Int, val cachedBuckets: Int, val key: T => String) {
  val memory = new Array[Byte](maxBucketSize * numberOfBuckets)
  val bucketsUsage = new Array[Float](numberOfBuckets)
  val bucketEntries = new Array[Int](numberOfBuckets)
  var overrunBuckets = scala.collection.immutable.IntMap[Array[Byte]]()
    
  val uncompressedBucketsCache = new LruMap[Int, List[T]](cachedBuckets)
  var cacheHits = 0l
  var cacheMisses = 0l
    
  def store(t: T): Unit = {
    val bucket = bucketOffset(key(t))
      
    val elements = t :: readBucket(bucket).filterNot(t.==)
      
    val baos = new ByteArrayOutputStream()
    val gzipOutput = new GZIPOutputStream(baos)
    val output = new Output(gzipOutput)
    kryo.writeObject(output, elements)
    output.flush()
    gzipOutput.close()
      
    Arrays.fill(memory, bucket * maxBucketSize, bucket * maxBucketSize + maxBucketSize, 0.toByte)
    val resByteArray = baos.toByteArray
      
    if (resByteArray.length > maxBucketSize) {
      //on overrun, store to the overrun buckets
      overrunBuckets = overrunBuckets.updated(bucket, resByteArray)
    } else {
      System.arraycopy(resByteArray, 0, memory, bucket * maxBucketSize, resByteArray.length)
    }
    uncompressedBucketsCache(bucket) = elements
    bucketsUsage(bucket) = resByteArray.length.toFloat / maxBucketSize
    bucketEntries(bucket) = elements.size
  }
    
  def retrieve(id: String): Option[T] = readBucket(bucketOffset(id)).find(key(_) == id)
    
  private def readBucket(bucket: Int): List[T] = {
    uncompressedBucketsCache.get(bucket) match {
      case Some(cached) => 
        cacheHits += 1
        cached
      case _ =>
        cacheMisses += 1
        val res = if (overrunBuckets.contains(bucket)) {
          val bais = new ByteArrayInputStream(overrunBuckets(bucket))
          val input = new Input(new GZIPInputStream(bais))
          kryo.readObject(input, classOf[List[T]])
        } else if (memory(bucket * maxBucketSize) == 0) List.empty
        else {
          val bais = new ByteArrayInputStream(memory, bucket * maxBucketSize, maxBucketSize)
          val input = new Input(new GZIPInputStream(bais))
          kryo.readObject(input, classOf[List[T]])
        }
        uncompressedBucketsCache(bucket) = res
        res
    }
  }
  private def bucketOffset(id: String): Int = (id.hashCode % numberOfBuckets).abs
    
  def iterator: Iterator[T] = (0 until memory.length).iterator.flatMap(readBucket)
  def clear(): Unit = Arrays.fill(memory, 0, memory.length, 0.toByte)
}