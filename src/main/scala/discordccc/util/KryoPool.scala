package discordccc.util


import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.{KryoInstantiator, ResourcePool}
import KryoPool._
import java.io.ByteArrayOutputStream

final class KryoPool(poolSize: Int, val kryoInstantiator: KryoInstantiator,
                     val outBufferMin: Int, val outBufferMax: Int) extends ResourcePool[SerDeState](poolSize) {
//  protected def newInstance() = SerDeState(kryoInstantiator.newKryo(), new Input(), new Output(new ByteArrayOutputStream()))
  protected def newInstance() = SerDeState(kryoInstantiator.newKryo(), new Input(), new Output(outBufferMin, outBufferMax))
  
  @inline def withInstance[R](f: SerDeState => R): R = {
    val serde = borrow()
    try f(serde)
    finally release(serde)
  }
  
  def fromBytes(bytes: Array[Byte]): Any = withInstance { serde => 
    serde.input.setBuffer(bytes)
    val res = serde.kryo.readClassAndObject(serde.input)
    serde.clear()
    res
  }
  
  def fromBytes[T](bytes: Array[Byte], cls: Class[T]): T = withInstance { serde => 
    serde.input.setBuffer(bytes)
    val res = serde.kryo.readObject(serde.input, cls)
    serde.clear()
    res
  }
  
  def toBytesWithClass(obj: Any): Array[Byte] = withInstance { serde =>
    serde.kryo.writeClassAndObject(serde.output, obj)
    serde.output.flush()
    val res = serde.output.toBytes()
    serde.clear()
    res
  }
  
  def toBytesWithoutClass(obj: Any): Array[Byte] = withInstance { serde =>
    serde.kryo.writeObject(serde.output, obj)
    serde.output.flush()
    val res = serde.output.toBytes()
    serde.clear()
    res
  }
  
}
object KryoPool {
  private val noInput: Array[Byte] = Array.empty
  final case class SerDeState(kryo: Kryo, input: Input, output: Output) {
    def clear(): Unit = {
      input.setBuffer(noInput)
      output.clear()
    }
  }
}
