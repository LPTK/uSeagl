package front

import util._

object Reporting {
  
  class CompileError(val msg: Str) extends Exception(msg)
  object CompileError {
    def unapply(ce: CompileError) = Some(ce.msg)
  }
  
  case class IdentifierNotFound(id: Id) extends CompileError(s"identifier not found: ${id.fullStr}")
  
  
  
}


