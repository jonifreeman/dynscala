package dynscala

object DynScala extends DynScala

trait DynScala {
  type Trap = (AnyRef, CallSite) => Any

  implicit def any2dyn(receiver: AnyRef) = new DynCall(receiver)
  implicit def sym2method(s: Symbol) = (params: (Any*)) => CallSite(s.name, params.toArray)
  implicit def any2trap(receiverType: Class[_]) = new {
    def trap(f: Trap) = Meta.add(receiverType, f)
  }

  case class CallSite(name: String, params: Array[Any]) {
    override def toString = name + "(" + params.mkString(", ") + ")"
  }

  class DynCall(receiver: AnyRef) {
    def -->(site: CallSite) = {
      try {
        val paramTypes = site.params.map(Reflection.getType(_))
        val method = receiver.getClass.getMethod(site.name, paramTypes: _*)
        method.invoke(receiver, site.params.map(Reflection.toAnyRef(_)): _*)
      } catch {
        case e: NoSuchMethodException => Meta.trap(receiver, site)
      }
    }
  }

  private object Meta {
    import scala.collection.jcl.Conversions._

    val traps = new java.util.concurrent.ConcurrentHashMap[Class[_], Trap]()

    // FIXME: why on earth does this not compile without an explicit cast?
    def add(receiverType: Class[_], trap: Trap) = 
      traps += (receiverType.asInstanceOf[Class[_]] -> trap)

    def trap(receiver: AnyRef, site: CallSite) = {
      val trap = findTrap(receiver).getOrElse(fail(site))
      trap(receiver, site)
    }

    private final def findTrap(receiver: AnyRef): Option[Trap] = {
      // FIXME should search in linearized order
      val matches = for ((k, v) <- traps if k.isAssignableFrom(receiver.getClass)) yield v
      matches.toList.firstOption
    }

    private def fail(site: CallSite) = throw new MethodMissingError("method missing " + site)
  }

  class MethodMissingError(msg: String) extends Exception(msg)
}

object Reflection {
  def getType(a: Any): Class[_] = a match {
    case _: Byte => java.lang.Byte.TYPE
    case _: Short => java.lang.Short.TYPE
    case _: Int => java.lang.Integer.TYPE
    case _: Long => java.lang.Long.TYPE
    case _: Float => java.lang.Float.TYPE
    case _: Double => java.lang.Double.TYPE
    case _: Char => java.lang.Character.TYPE
    case _: Boolean => java.lang.Boolean.TYPE
    case _: Unit => java.lang.Void.TYPE
    case a: AnyRef => a.getClass
  }

 def toAnyRef(a: Any): AnyRef = a match {
    case a: Byte => Byte.box(a)
    case a: Short => Short.box(a)
    case a: Int => Int.box(a)
    case a: Long => Long.box(a)
    case a: Float => Float.box(a)
    case a: Double => Double.box(a)
    case a: Char => Char.box(a)
    case a: Boolean => Boolean.box(a)
    case a: Unit => ()
    case a: AnyRef => a
  }
}
