package front

import utils._
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

trait Uid extends Unique {
  override def toString = s"[$id]"
}
class FUid extends Uid
class TUid extends Uid
class VUid extends Uid


trait Pgrms {
self: Stage =>
  
  val dispIds = false
//  val dispIds = true
  
  case class Pgrm(typs: Map[Id,ConcTyp], funs: Map[Id,Fun])
  
  trait Decl
  
//  class FUid
  
//  sealed trait Typ extends Unique with Parmzd {
  sealed trait Typ extends Parmzd {
    val uid: TUid
    val nam: TId
    val typs: Seq[TypeParam] // Seq[TId]
    val regs: Seq[VId]
    
    def namStr = if (dispIds) s"$uid$nam" else nam.toString
    
    override def toString =
    (if (dispIds) s"$uid" else "") +
    (this match {
      case ConcTyp(uid, nam, typs, regs, params) =>
        "typ " + nam + mkTyps(typs map tpname) + mkRegs(regs) + mkArgs(params)
      case AbsTyp(uid, nam, typs, regs, ud) =>
        "" + nam + mkTyps(typs) + mkRegs(regs) + "=?"
    })
  }
  
  case class ConcTyp(uid: TUid, nam: TId, typs: Seq[TypeParam], regs: Seq[VId], params: Seq[Local]) extends Decl with Typ {
    def getField(id: VId) = params.find (_.nam === id)
    
    /** TODO: check if always okay to do this: this is assuming a ConcTyp in the same stage with the same id should be the same
    * (note: otherwise equality of recursive types diverges) */
    override def equals(that: Any) = that match {
      case that: ConcTyp => that.uid === uid // Note: this is because transformations can duplicate Cyclic[Type] objects, which use unique
      case _ => false
    }
  }

  case class AbsTyp(uid: TUid, nam: TId, typs: Seq[TypeParam], regs: Seq[VId], userDefined: Bool) extends Typ {
    def params = Seq()
  }
  object AbsTyp {
//    def apply(): AbsTyp = { val id = new TUid; AbsTyp(id, TId(id toString), Seq(), Seq(), false) }
    def apply(): AbsTyp = new TUid in { id => AbsTyp(id, TId(id toString), Seq(), Seq(), false) }
  }
  
  
  case class Type(t: TypSym, targs: Seq[Type], rargs: Seq[Reg]) {
    override def toString = tname(t) + mkTyps(targs) + mkRegs(rargs)
  }
  
  case class Fun (
      uid: FUid,
      nam: FId,
      typs: Seq[TypeParam],
      regs: Seq[VId],
      params: Seq[Local],
      ret: TypeSpec,
      spec: Spec,
      body: Term
//  ) extends Decl with Parmzd with Unique {
  ) extends Decl with Parmzd {
    
    def mkNew = Fun(new FUid, nam, typs, regs, params, ret, spec, body)
    
    override def toString = "fun " + nam + mkTyps(typs map tpname) +
      mkRegs(regs) + mkArgs(params,true) + s": $ret = $body"
  }
  
  
  /** will incorporate more complex info, eg: cross-scoping info (?) */
  case class Local(uid: VUid, nam: VId, typ: TypeSpec) { //extends Unique {
    override def toString = s"$nam: $typ"
  }
  
  
  trait Parmzd {
    def typs: Seq[TypeParam]
    def regs: Seq[VId]
    def params: Seq[Local]
  }
  
  trait Inst {
    def parmzd: Parmzd
    def targs: Seq[Type]
    def rargs: Seq[Reg]
    def args: Seq[Term]
    
    def reg(t: Term): Reg
    
    lazy val regSubs =
      (parmzd.regs map (_ sym) zip rargs).toMap ++ //and {print(">>"); println}
      (parmzd.params map (_.nam sym) zip (args map reg)).toMap
    
    def transType(t: Type): Type = t match { // TODO: careful with cycles! => inf.loops?
      case Type(Cyclic(at: AbsTyp), _, _) if parmzd.typs contains at =>
        transType(targs(parmzd.typs indexOf at))
      case Type(c@Cyclic(ct: ConcTyp), targs, rargs) =>
        Type(c, targs map transType, rargs map transReg)
      case _ => t // unknown abs type
    }
    
    def transReg(r: Reg) = //rargs(parmzd.regs indexOf r)
      r.transHead(regSubs)
    
  }
  
  
  
  def mkStr(xs: Traversable[_], start: Str, sep: Str, end: Str, showEmpty: Bool = true) =
    if (!showEmpty && xs.isEmpty) ""
    else xs.mkString(start, sep, end)
  
  def mkArgs(xs: Traversable[_],se:Bool=false) = mkStr(xs,"(",", ",")",se)
  def mkTyps(xs: Traversable[_]) = mkStr(xs,"[",", ","]",false)
  def mkRegs(xs: Traversable[_]) = mkStr(xs,"{",", ","}",false)
  
}















