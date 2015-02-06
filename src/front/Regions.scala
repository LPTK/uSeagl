package front

object Regions {
  import regex.RegexRegions._
  
  trait Reg {
    def exp: Regex
  }
  val Reg = XReg
  object XReg {
    val empty = XReg(Empty)
  }
  
  /** Exact region */
  case class XReg(exp: Regex) extends Reg
  
  /** Full region */
  case class FReg(fexp: Full) extends Reg {
    def exp = fexp.r
  }
  
  
  
}





