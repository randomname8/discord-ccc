package headache

import language.dynamics

import org.json4s._
import org.json4s.native.JsonMethods
import prickle._
import scala.annotation.unchecked.uncheckedVariance

object Json4sUtils {

  type Unpickler[+T] = prickle.Unpickler[T @uncheckedVariance]

  implicit class jValue2Dyn(val jv: JValue) extends AnyVal {
    def dyn = new DynJValueSelector(jv)
  }
  class DynJValueSelector(val jv: JValue) extends AnyVal with Dynamic {
    def selectDynamic(field: String) = new DynJValueSelector(jv \ field)
    def extract[T](implicit unpickler: Unpickler[T], conf: PConfig[JValue]): T = Unpickle[T].from(jv)(conf).get
    override def toString = jv.toString
  }

  def parseJson(s: String): JValue = JsonMethods.parse(s)
  def renderJson(jv: JValue, pretty: Boolean = false): String =
    if (pretty) JsonMethods.pretty(JsonMethods.render(jv))
    else JsonMethods.compact(JsonMethods.render(jv))
  def toJson[T](t: T)(implicit p: Pickler[T], c: PConfig[JValue]): JValue = Pickle[T, JValue](t)
}