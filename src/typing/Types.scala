package typing

import front._
import common.Stage
import common.Stages._
import Regions._

trait Types {
//self: Pgrms =>
self: Stage =>
  
//  val showTypes = false
  val showTypes = true
  
  case class Typd[T](obj: T, typ: Type, reg: Reg) { //extends Stmt {
    
    override def toString =
      if (showTypes) s"$obj: $typ"
      else s"$obj"
  }
  
}



object Types {
   val singleStaged = SingleStaged(Typed)
}




