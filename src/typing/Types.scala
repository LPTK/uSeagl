package typing

import front._

trait Types {
self: Pgrms =>
  
  val showTypes = false
  
  //trait Type
  
  case class Typd[T](obj: T, typ: Type) {
    override def toString =
      if (showTypes) s"$obj: $typ"
      else s"$obj"
  }
  object Typd {
    
    
    
  }

}







