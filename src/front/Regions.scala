package front

object Regions {
  import regex.RegexRegions._
  
//  trait Reg {
//    def exp: Regex
//  }
//  val Reg = XReg
//  object XReg {
//    val empty = XReg(Empty)
//    def apply(id: VId): XReg = XReg(Sym(id.sym))
//  }
//  
//  /** Exact region */
//  case class XReg(exp: Regex) extends Reg {
//    
//    def ~ (r: Reg) = Reg(exp ~ r.exp)
//    
//    override def toString = exp.toString
//  }
//  
//  /** Full region */
//  case class FReg(fexp: Full) extends Reg {
//    def exp = fexp.r
//  }
  
  
  case class Reg(exp: Regex) {
    
    def ~ (r: Reg) = Reg(exp ~ r.exp)
    
    def transHead(subs: collection.Map[Symbol, Reg]) = Reg{
      (exp.consume map {
        case Some((Sym(s), r)) if subs isDefinedAt s => subs(s).exp ~ r
        case Some((s, r)) => s ~ r
        case None => Empty
      } foldLeft (Empty: Regex)) (Regex.union)
    }
    
    override def equals(that: Any) = that match {
      case that: Reg => that.exp equiv exp
      case _ => false
    }
    override def toString = exp.toString
  }
  object Reg {
    val empty = Reg(Empty)
    def apply(id: VId): Reg = Reg(Sym(id.sym))
  }
  
  
  
}





