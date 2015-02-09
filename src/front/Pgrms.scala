package front

import util._
import common._
import Specs._
import Regions._

abstract class Id(name: Str) {
  def sym: Symbol
  def fullStr = s"$name "+toString
  override def toString = sym match { case Symbol(str) => str }
}
object Id {
  implicit def toStr(id: Id) = id.toString
}
case class TId(sym: Sym) extends Id("Type")
object TId { def apply(str: Str) = new TId(Sym(str)) }
case class FId(sym: Sym) extends Id("Function")
object FId { def apply(str: Str) = new FId(Sym(str)) }
case class VId(sym: Sym) extends Id("Local")
object VId { def apply(str: Str) = new VId(Sym(str)) }

trait Pgrms {
self: Stage =>
  
//  type Id = Sym
//  case class Id(sym: Sym) {
//    override def toString = sym match { case Symbol(str) => str }
//  }
//  object Id {
//    val empty = Id(Symbol("")) // but seriously, why would you do that?
//    def apply(str: Str): Id = Id(Symbol(str))
//  }
//  class TId(sym: Sym) extends Id(sym)
//  object TId { def apply(str: Str): TId = new TId(Symbol(str))
//  class FId(sym: Sym) extends Id(sym)
  
  
  case class Pgrm(typs: Map[Id,ConcTyp], funs: Map[Id,Fun])
  
  trait Decl
  
  trait Parmzd {
    def typs: Seq[TypeParam]
    def regs: Seq[VId]
  }
  trait Inst {
    def parmzd: Parmzd
    def targs: Seq[Type]
    def rargs: Seq[Reg]
    
    def transType(t: Type): Type = t match { // TODO: careful with cycles!
      case Type(Cyclic(at: AbsTyp), _, _) if parmzd.typs contains at =>
        transType(targs(parmzd.typs indexOf at))
      case Type(c@Cyclic(ct: ConcTyp), targs, rargs) =>
        Type(c, targs map transType, rargs)
      case _ => t // unknown abs type
    }
    
  }
  
  sealed trait Typ extends Unique with Parmzd {
    val nam: TId
    val typs: Seq[TypeParam] // Seq[TId]
    val regs: Seq[VId]
    
    override def toString = this match {
      case ConcTyp(nam, typs, regs, params) =>
//        s"$nam[${typs mkString ", "}]{${regs mkString ", "}}(${params mkString ", "})"
        "typ " + nam + mkTyps(typs) + mkRegs(regs) + mkArgs(params)
      case AbsTyp(nam, typs, regs, ud) =>
//        s"$nam[${typs mkString ", "}]{${regs mkString ", "}}=?"
        "" + nam + mkTyps(typs) + mkRegs(regs) + "=?"
    }
  }
  
  case class ConcTyp(nam: TId, typs: Seq[TypeParam], regs: Seq[VId], params: Seq[Local]) extends Decl with Typ {
    def getField(id: VId) = params.find (_.nam === id)
//    def fieldType(id: VId) = (params.find (_.nam === id) get).typ // TODOne handle not here
  }

  case class AbsTyp(nam: TId, typs: Seq[TypeParam], regs: Seq[VId], userDefined: Bool) extends Typ
  
  
//  case class Type(t: TypSym, targs: Opt[Seq[Type]], rargs: Opt[Seq[Reg]]) { // TODOne rm Opt
  case class Type(t: TypSym, targs: Seq[Type], rargs: Seq[Reg]) {
//    def parmzd = t
    
    override def toString = tname(t) + mkTyps(targs) + mkArgs(rargs)
  }
//  object Type {
//    def unapply()
//  }
  
  case class Fun (
      nam: FId,
      typs: Seq[TypeParam], // Seq[TId],
      regs: Seq[VId],
      params: Seq[Local],
      ret: TypeSpec,
      spec: Spec,
      body: Term  // Cyclic[Term]
  ) extends Decl with Parmzd with Unique {
    override def toString = "fun " + nam + mkStr(typs,"[",", ","]",false) +
      mkStr(regs,"{",", ","}",false) + mkStr(params,"(",", ",")",true) + s": $ret = $body"
  }
  
  
  /** will incorporate more complex info, eg: cross-scoping info (?) */
  case class Local(nam: VId, typ: TypeSpec) extends Unique {
    override def toString = s"$nam: $typ"
  }
  
  
  def mkStr(xs: Traversable[_], start: Str, sep: Str, end: Str, showEmpty: Bool = true) =
    if (!showEmpty && xs.isEmpty) ""
    else xs.mkString(start, sep, end)
  
  def mkArgs(xs: Traversable[_]) = mkStr(xs,"(",", ",")",false)
  def mkTyps(xs: Traversable[_]) = mkStr(xs,"[",", ","]",false)
  def mkRegs(xs: Traversable[_]) = mkStr(xs,"{",", ","}",false)
  
}















