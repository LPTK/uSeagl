package front

import util._
import common._
//import Pgrms._
import Regions._

trait Exprs {
self: Stage =>
  
  trait Stmt {
    
    override def toString = this match {
      case Binding(nam, valu) => s"$nam = $valu"
      case Var(s) => s"${vname(s)}"
      case FCall(f, targs, rargs, args) =>
        fname(f) + mkTyps(targs) + mkRegs(rargs) + mkArgs(args) //s"${fname(f)}${mkArgs}"
      case Build(typ, args) =>
        "new " + typ + mkArgs(args) //s"${fname(f)}${mkArgs}"
      case Block(stmts, ret) => s"{${stmts map (_.toString+"; ")}$ret}"
      case IntLit(n) => n.toString
      case Ite(c,t,e) => s"if $c then $t else $e"
      case NilExpr => s"nil"
      case FieldAccess(obj, id) => s"$obj.$id"
      case FieldAssign(obj, id, value) => s"$obj.$id <- $value"
      case Take(obj, id) => s"take $obj.$id"
    }
    
  }
  
  trait Expr extends Stmt
  
  trait BasicExpr extends Expr
  
//  case class Var(nam: VId) extends BasicExpr
  case class Var(sym: VarSym) extends BasicExpr
  case class FCall(f: FunSym, targs: Seq[Type], rargs: Seq[Reg], args: Seq[Term]) extends BasicExpr {
//    override def toString = s"CALL ${fname(f)} ${args mkString ","}"
  }
  case class Build(typ: Type, args: Seq[Term]) extends BasicExpr
  
  case class Binding(nam: VId, value: Term) extends Stmt
  case class Block(stmts: Seq[Stmt], ret: Term) extends BasicExpr
  
  case class IntLit(value: Int) extends BasicExpr
  
//  case class BoolLit(value: Bool) extends BasicExpr
  
  case class Ite(cond: Term, thn: Term, els: Term) extends BasicExpr
  
  case object NilExpr extends BasicExpr
  
  
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










