package front

object Regions {
  import regex.RegexRegions._
  
  trait Reg {
    def exp: Regex
  }
  val Reg = XReg
  object XReg {
    val empty = XReg(Empty)
    def apply(id: VId): XReg = XReg(Sym(id.sym))
  }
  
  /** Exact region */
  case class XReg(exp: Regex) extends Reg {
    override def toString = exp.toString
  }
  
  /** Full region */
  case class FReg(fexp: Full) extends Reg {
    def exp = fexp.r
  }
  
  
  
}





