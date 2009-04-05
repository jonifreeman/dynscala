package dynscala

object DynScala extends DynScala

trait DynScala {
  type Trap[A] = (A, CallSite) => Any

  implicit def any2dyn[A](receiver: A)(implicit t: Trap[A]): DynCall[A] = new DynCall(receiver)
  implicit def sym2method(s: Symbol) = (params: (Any*)) => CallSite(s.name, params.toArray)

  case class CallSite(name: String, params: Array[Any]) {
    override def toString = name + "(" + params.mkString(", ") + ")"
  }

  class DynCall[A](receiver: A)(implicit t: Trap[A]) {
    def -->(site: CallSite) = {
      try {
        val paramTypes = site.params.map(Reflection.getType)
        val method = receiver.asInstanceOf[AnyRef].getClass.getMethod(site.name, paramTypes: _*)
        method.invoke(receiver, site.params.map(Reflection.toAnyRef): _*)
      } catch {
        case e: NoSuchMethodException => trap(receiver, site)
      }
    }

    private def trap[A](receiver: A, site: CallSite)(implicit t: Trap[A]) = t(receiver, site)
  }

  def fail[A] = new Trap[A] {
    def apply(a: A, site: CallSite) = throw new MethodMissingError("method missing " + site)
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
