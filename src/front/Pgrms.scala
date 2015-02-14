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
  }

  case class AbsTyp(uid: TUid, nam: TId, typs: Seq[TypeParam], regs: Seq[VId], userDefined: Bool) extends Typ
  
  
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
  case class Local(uid: VUid, nam: VId, typ: TypeSpec) extends Unique {
    override def toString = s"$nam: $typ"
  }
  
  
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
  
  
  
  def mkStr(xs: Traversable[_], start: Str, sep: Str, end: Str, showEmpty: Bool = true) =
    if (!showEmpty && xs.isEmpty) ""
    else xs.mkString(start, sep, end)
  
  def mkArgs(xs: Traversable[_],se:Bool=false) = mkStr(xs,"(",", ",")",se)
  def mkTyps(xs: Traversable[_]) = mkStr(xs,"[",", ","]",false)
  def mkRegs(xs: Traversable[_]) = mkStr(xs,"{",", ","}",false)
  
}















