package dynscala

import org.scalacheck._

object DynCallSpecification extends Properties("DynCall") {
  import DynScala._

  implicit def trap[A] = new Trap[A] {
    def apply(a: A, site: CallSite) = Reflection.getType(a).getName + "." + site
  }

  specify("call0", (any: String, method: Symbol) => any-->method() == sig(any, method))
  specify("call1", (any: String, method: Symbol, p1: Int) => any-->method(p1) == sig(any, method, p1))
  specify("call2", (any: String, method: Symbol, p1: Int, p2: Double) => any-->method(p1, p2) == sig(any, method, p1, p2))
  specify("call3", (any: String, method: Symbol, p1: Int, p2: Double, p3: Boolean) => any-->method(p1, p2, p3) == sig(any, method, p1, p2, p3))

  def sig(any: AnyRef, method: Symbol, params: Any*) = 
    any.getClass.getName + "." + method.name + "(" + params.mkString(", ") + ")"

  implicit def arbSymbol: Arbitrary[Symbol] = Arbitrary(Gen.identifier map (Symbol(_)))
}
