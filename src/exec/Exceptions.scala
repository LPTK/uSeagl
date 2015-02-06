package exec

object Exceptions {
  import util._
  
  case class ExecException(msg: Str) extends Exception
  
  def check(e: => Bool) {
    if (!e) throw new ExecException("Invariant checking error")
  }
  
  
}