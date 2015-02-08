package common

import util._
import front._

object Reporting {
  
  class CompileError(val msg: Str) extends Exception(msg)
  object CompileError {
    def apply(msg: Str) = new CompileError(msg)
    def unapply(ce: CompileError) = Some(ce.msg)
  }
  
  case class IdentifierNotFound(id: Id) extends CompileError(s"identifier not found: ${id.fullStr}")
  
}


