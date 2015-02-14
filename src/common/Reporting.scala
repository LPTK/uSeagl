package common

import utils._
import front._
import Stages._

object Reporting {
  
  class CompileError(val msg: Str) extends Exception(msg)
  object CompileError {
    def apply(msg: Str) = new CompileError(msg)
    def unapply(ce: CompileError) = Some(ce.msg)
  }
  
  case class IdentifierNotFound(id: Id) extends CompileError(s"identifier not found: ${id.fullStr}")
  
//  case class UnificationError(t1: Typed.Type, t2: Typed.Type)
  case class UnificationError(t1: Any, t2: Any)
    extends CompileError(s"cannot unify $t1 with $t2")
  
}


