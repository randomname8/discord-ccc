package discordccc

import discordccc.model._
import headache.Snowflake
import org.cliffc.high_scale_lib.NonBlockingHashMapLong
import org.openjdk.jmh.annotations._
import scala.concurrent.duration._

object ConcurrentMapsTest {
  val Members = 15000
  
  val members = for (_ <- 0 until Members) yield 
    Member(scala.util.Random.nextLong,
           scala.util.Random.nextLong,
           scala.util.Random.alphanumeric.take(10).mkString,
           Vector.fill(3)(scala.util.Random.alphanumeric.take(5).mkString),
           0,
           false,
           null)
  
  @State(Scope.Thread)
  class Pointers {
    var read: Long = 0
    var toWrite: Member = members.head
    
    @Setup(Level.Iteration)
    def next() = {
      read = members(scala.util.Random.nextInt(Members)).userId
      toWrite = members(scala.util.Random.nextInt(Members))
    }
  }
  
  @State(Scope.Benchmark)
  class ConcurrentHashMapState {
    val map = new java.util.concurrent.ConcurrentHashMap[Long, Member]
  }
  @State(Scope.Benchmark)
  class StmMapState {
    val map = scala.concurrent.stm.TMap.empty[Long, Member]
  }
  @State(Scope.Benchmark)
  class TrieMapState {
    val map = scala.collection.concurrent.TrieMap[Long, Member]()
  }
  @State(Scope.Benchmark)
  class SimpleConcurrentLongMapState {
    val map = new SimpleConcurrentLongMap[Member](32)
  }
  @State(Scope.Benchmark)
  class NonBlockingLongMapState {
    val map = new NonBlockingHashMapLong[Member]()
  }
  
  class SimpleConcurrentLongMap[T](val parallelismLevel: Int) {
    val maps = Array.fill(parallelismLevel)(scala.collection.mutable.LongMap[T]())
    val locks = Array.fill(parallelismLevel)(new java.util.concurrent.locks.ReentrantLock())
    
    def put(key: Long, value: T): Unit = {
      val bucket = (key.abs % parallelismLevel).toInt
      val l = locks(bucket)
      l.lock()
      try maps(bucket).put(key, value)
      finally l.unlock()
    }
    def get(key: Long): T = {
      val bucket = (key.abs % parallelismLevel).toInt
//      val l = locks(bucket).readLock
//      while (!l.tryLock()) Thread.onSpinWait()
      try maps(bucket).getOrElse(key, null.asInstanceOf[T])
//      finally l.unlock()
    }
  }
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(MICROSECONDS)
@Fork(jvmArgsAppend = Array("-XX:+UnlockExperimentalVMOptions", "-XX:+EnableJVMCI", "-XX:+UseJVMCICompiler", "-Djvmci.Compiler=graal"))
class ConcurrentMapsTest {
  import ConcurrentMapsTest._
  
  @Benchmark
  def cmh(state: ConcurrentHashMapState, pointers: Pointers) = {
    state.map.put(pointers.toWrite.userId, pointers.toWrite)
    state.map.get(pointers.read)
  }
  
  @Benchmark
  def highscale(state: NonBlockingLongMapState, pointers: Pointers) = {
    state.map.put(pointers.toWrite.userId, pointers.toWrite)
    state.map.get(pointers.read)
  }
  
  @Benchmark
  def stm(state: StmMapState, pointers: Pointers) = {
    state.map.single.put(pointers.toWrite.userId, pointers.toWrite)
    state.map.single.get(pointers.read)
  }
  
  @Benchmark
  def trie(state: TrieMapState, pointers: Pointers) = {
    state.map.put(pointers.toWrite.userId, pointers.toWrite)
    state.map.get(pointers.read)
  }
  
  @Benchmark
  def concurrentLongMap(state: SimpleConcurrentLongMapState, pointers: Pointers) = {
    state.map.put(pointers.toWrite.userId, pointers.toWrite)
    state.map.get(pointers.read)
  }
}
