package discordccc.util

import discordccc.model._
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.ScalaKryoInstantiator
import scala.reflect.{ClassTag, classTag}

object CustomKryoInstantiator extends ScalaKryoInstantiator {
  override def newKryo = {
    val k = super.newKryo
    k.register(classOf[Connector], ConnectorSerializer)
    k.register(classOf[Member], MemberSerializer)
    k.register(classOf[User], UserSerializer)
    k
  }
  
  /*
   * define some custom serializers for the classes that are more abundant and big to serialize.
   */
  
  object ConnectorSerializer extends Serializer[Connector] {
    def read(kryo: Kryo, input: Input, c: Class[Connector]): Connector = ConnectorRegistry withValue input.readByte()
    def write(kryo: Kryo, output: Output, c: Connector): Unit = output writeByte c.value
  }
  
  object MemberSerializer extends Serializer[Member] {
    def read(kryo: Kryo, input: Input, c: Class[Member]): Member = {
      val userId = input.readLong()
      val serverId = input.readLong()
      val nickname = input.readString()
      val roles = readSeq[String](kryo, input)
      val color = input.readInt()
      val isOwner = input.readBoolean()
      val connector = kryo.readObject(input, classOf[Connector])
      val metadata = kryo.readObjectOrNull(input, connector.memberMetadataClass)
      Member(userId, serverId, nickname, roles, color, isOwner, connector, metadata)
    }
    def write(kryo: Kryo, output: Output, m: Member): Unit = {
      output.writeLong(m.userId)
      output.writeLong(m.serverId)
      output.writeString(m.nickname)
      writeSeq(kryo, output, m.roles)
      output.writeInt(m.color)
      output.writeBoolean(m.isOwner)
      kryo.writeObject(output, m.connector, ConnectorSerializer)
      kryo.writeObjectOrNull(output, m.metadata, m.connector.memberMetadataClass)
      output.flush()
    }
  }
  object UserSerializer extends Serializer[User] {
    def read(kryo: Kryo, input: Input, c: Class[User]): User = {
      val id = input.readLong()
      val name = input.readString()
      val bot = input.readBoolean()
      val extra = input.readString()
      val imageUrl = Option(kryo.readObjectOrNull(input, classOf[String]))
      val friend = input.readBoolean()
      val connector = kryo.readObject(input, classOf[Connector])
      val metadata = kryo.readObjectOrNull(input, connector.userMetadataClass)
      User(id, name, bot, extra, imageUrl, friend, connector, metadata)
    }
    def write(kryo: Kryo, output: Output, m: User): Unit = {
      output.writeLong(m.id)
      output.writeString(m.name)
      output.writeBoolean(m.bot)
      output.writeString(m.extra)
      kryo.writeObjectOrNull(output, m.imageUrl.orNull, classOf[String])
      output.writeBoolean(m.friend)
      kryo.writeObject(output, m.connector, ConnectorSerializer)
      kryo.writeObjectOrNull(output, m.metadata, m.connector.userMetadataClass)
      output.flush()
    }
  }
  
  private def readSeq[T: ClassTag](kryo: Kryo, input: Input): Seq[T] = {
    val elements = input.readInt(true)
    val clazz = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    Seq.fill(elements)(kryo.readObject(input, clazz))
  }
  private def writeSeq[T](kryo: Kryo, output: Output, seq: Seq[T]): Unit = {
    output.writeInt(seq.size, true)
    seq foreach (e => kryo.writeObject(output, e))
  }
}
