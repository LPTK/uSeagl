package typing

import front._

trait Types {
self: Pgrms =>
  
  
  //trait Type
  
  case class Typd[T](obj: T, typ: Type) {
    override def toString = s"$obj: $typ"
  }
  object Typd {
    
    
    
  }

}







