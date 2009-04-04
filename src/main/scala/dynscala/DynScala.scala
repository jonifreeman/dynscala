package dynscala

object DynScala extends DynScala

trait DynScala {
  type Trap = (AnyRef, CallSite) => Any

  implicit def any2dyn(receiver: AnyRef) = new DynCall(receiver)
  // FIXME should be Any*
  implicit def sym2method(s: Symbol) = (params: (AnyRef*)) => CallSite(s.name, params.toArray)
  implicit def any2trap(receiverType: Class[_]) = new {
    def trap(f: Trap) = Meta.add(receiverType, f)
  }

  case class CallSite(name: String, params: Array[AnyRef]) {
    override def toString = name + "(" + params.mkString(", ") + ")"
  }

  class DynCall(receiver: AnyRef) {
    def -->(site: CallSite) = {
      try {
        val paramTypes = site.params.toArray.map((p: AnyRef) => p.getClass)
        val method = receiver.getClass.getMethod(site.name, paramTypes: _*)
        method.invoke(receiver, site.params.toArray: _*)
      } catch {
        case e: NoSuchMethodException => Meta.trap(receiver, site)
      }
    }
  }

  private object Meta {
    import scala.collection.jcl.Conversions._

    val traps = new java.util.concurrent.ConcurrentHashMap[Class[_], Trap]()

    // FIXME: why on earth this does not compile without explicit cast?
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
