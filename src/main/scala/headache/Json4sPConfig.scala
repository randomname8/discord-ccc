package headache

import org.json4s._
import prickle.PConfig
import scala.util.control.NoStackTrace
import scala.util.{Try, Success, Failure}

object Json4sPConfig extends PConfig[JValue] {
  implicit val conf = this
  def areSharedObjectsSupported: Boolean = false
  def prefix: String = "Json4sPConfig#"

  def makeNull() = JNull
  def makeBoolean(b: Boolean) = JBool(b)
  def makeNumber(x: Double) = JDouble(x)
  def makeString(s: String) = JString(s)
  def makeArray(elems: JValue*) = JArray(elems.asInstanceOf[List[JValue]])
  def makeObject(fields: Seq[(String, JValue)]) = JObject(fields: _*).transformField { case JField("tpe", v) => JField("type", v) }

  def isNull(x: JValue): Boolean = x match {
    case JNull => true
    case _ => false
  }
  def readBoolean(x: JValue): Try[Boolean] = x match {
    case JBool(b) => Success(b)
    case other => error("boolean", s"$other")
  }
  def readNumber(x: JValue): Try[Double] = x match {
    case JDouble(d) => Try(d)
    case JInt(i) => Try(i.toDouble)
    case JLong(l) => Try(l.toDouble)
    case JDecimal(d) => Try(d.toDouble)
    case other => error("number", s"$other")
  }
  def readString(x: JValue): Try[String] = x match {
    case JString(s) => Success(s)
    case other => error("string", s"$other")
  }
  def readArrayLength(x: JValue): Try[Int] = x match {
    case x: JArray => Success(x.arr.size)
    case other => error("array length", s"$other")
  }
  def readArrayElem(x: JValue, index: Int): Try[JValue] = x match {
    case JArray(arr) if index < arr.size => Success(arr(index))
    case other => error(s"array($index)", s"$other")
  }

  private val IgnoreRefField = "Json4sPConfig#ref"
  private val IgnoredFieldException = new RuntimeException("no ref field") with NoStackTrace
  def readObjectField(x: JValue, field: String): Try[JValue] = {
//    println(s"getting field $field")
    x match {
      case any if field == IgnoreRefField => Failure(IgnoredFieldException)
      case JObject(fields) =>
        val f = if (field == "tpe") "type" else field
        val r = fields.find(_._1 == f).map(_._2).getOrElse(JNull)
//        if (!r.isInstanceOf[JObject]) println("  = " + r)
        Success(r) //missing fields are the same the field being present and null
        //      Try(x.value(field)).orElse(fail(
        //      s"Cannot read field '$field' of '$x', available fields: ${x.value.values.mkString(", ")}"))
      case other => error(s"field \'$field\' to be an object", s"$other")
    }
  }

  def error(exp: String, actual: String) = Failure(new RuntimeException(s"Expected: $exp  Actual: $actual"))

  def fail(msg: String) = Failure(new RuntimeException(msg))
}
