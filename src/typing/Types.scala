package typing

import front._

trait Types {
self: Pgrms =>
  
  val showTypes = false
//  val showTypes = true
  
  case class Typd[T](obj: T, typ: Type) {
    
    override def toString =
      if (showTypes) s"$obj: $typ"
      else s"$obj"
  }
  
}







