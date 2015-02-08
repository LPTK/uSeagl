package exec

object Exceptions {
  import util._
  import common._
  import front._
  
  case class ExecException(msg: Str) extends Exception
  
  def check(e: => Bool) {
    if (!e) throw new ExecException("Invariant checking error")
  }
  
  class AbsTypeBuildEE[S <: Stage](typ: S# Typ) extends ExecException(s"Trying to build an abstract type: $typ")
  
  class IfCondEE(p: Memory.Ptr) extends ExecException(s"If expression with non-integer condition: $p")
  
  class FieldAccessEE(p: Memory.Ptr) extends ExecException(s"Wrongful field access/assign on value: $p")
  
}



