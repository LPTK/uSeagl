package front

import util._
import common._
//import Pgrms._
import Regions._

trait Exprs {
self: Stage =>
  
  trait Stmt
  trait Expr extends Stmt
  
  trait BasicExpr extends Expr
  
//  case class Var(nam: VId) extends BasicExpr
  case class Var(sym: VarSym) extends BasicExpr
  case class FCall(f: FunSym, targs: Opt[Seq[Type]], rargs: Opt[Seq[Reg]], args: Seq[Term]) extends BasicExpr {
    override def toString = s"CALL ${fname(f)} ${args mkString ","}"
  }
  case class Build(typ: Type, args: Seq[Term]) extends BasicExpr
  
  case class Binding(nam: VId, value: Expr) extends Stmt
  case class Block(smts: Seq[Stmt]) extends BasicExpr
  
  
  case class FieldAccess(obj: Term, id: VId) extends BasicExpr
  
  case class FieldAssign(obj: Term, id: VId, value: Term) extends BasicExpr
  
  
//  /**
//   * takes at the given adress
//   * unlike in the paper, does not put nil instead (!); can be done on a simple ref
//   * effect system should ensure the old owner ptr is not used/deallocated
//  */
//  case class Take(obj: Term) extends BasicExpr
  
  case class Take(obj: Term, id: VId) extends BasicExpr
  
  
  
  
}










