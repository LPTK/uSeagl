package front

import util._
import common._
import Specs._
import Regions._

trait Id {
  def sym: Symbol
  override def toString = sym match { case Symbol(str) => str }
}
case class TId(sym: Sym) extends Id
object TId { def apply(str: Str) = new TId(Sym(str)) }
case class FId(sym: Sym) extends Id
object FId { def apply(str: Str) = new FId(Sym(str)) }
case class VId(sym: Sym) extends Id
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
  
  
  trait Decl
  
  case class Typ(nam: TId, typs: Seq[TId], regs: Seq[VId], params: Seq[Local]) extends Decl
  
  case class Type(t: TypSym, targs: Opt[Seq[Type]], rargs: Opt[Seq[Reg]])
  
  case class Fun (
      nam: FId,
      typs: Seq[TId],
      regs: Seq[VId],
      params: Seq[Local],
      ret: Opt[Type],
      spec: Spec,
      body: Term  // Cyclic[Term]
  ) extends Decl
  
  case class Pgrm(typs: Map[Id,Typ], funs: Map[Id,Fun])
  
  
  /** will incorporate more complex info, eg: cross-scoping info */
  case class Local(nam: VId, typ: Type)
  
  
  
}















