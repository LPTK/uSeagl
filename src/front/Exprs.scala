package front

import util._
import common._
//import Pgrms._
import Regions._

trait Exprs {
self: Stage =>
  
  trait Expr extends Stmt
  
  trait BasicExpr extends Expr
  
  
  case class Var(sym: VarSym) extends BasicExpr
  
  case class FCall(f: FunSym, targs: Seq[Type], rargs: Seq[Reg], args: Seq[Term]) extends BasicExpr
  
  case class Build(typ: Type, args: Seq[Term]) extends BasicExpr
  
//  case class Binding(nam: VId, value: Term) extends Stmt
  case class Binding(loc: Local, value: Term) extends Stmt
  case class Block(stmts: Seq[Eit[Term,Binding]], ret: Term) extends BasicExpr
  
  case class IntLit(value: Int) extends BasicExpr
  case class IntOp(lhs: Term, rhs: Term, op: (Int, Int) => Int) extends BasicExpr
  
  case class Ite(cond: Term, thn: Term, els: Term) extends BasicExpr
  
  case object NilExpr extends BasicExpr
  
  case class FieldAccess(obj: Term, id: VId) extends BasicExpr
  case class FieldAssign(obj: Term, id: VId, value: Term) extends BasicExpr
  
  case class Take(obj: Term, id: VId) extends BasicExpr
  
  
  trait Stmt {
    
    override def toString = this match {
      case Binding(nam, valu) => s"$nam = $valu"
      case Var(s) => s"${vname(s)}"
      case FCall(f, targs, rargs, args) =>
        fname(f) + mkTyps(targs) + mkRegs(rargs) + mkArgs(args,true) //s"${fname(f)}${mkArgs}"
      case Build(typ, args) =>
        "new " + typ + mkArgs(args) //s"${fname(f)}${mkArgs}"
//      case Block(stmts, ret) => s"{${stmts map (_.toString+"; ") mkString}$ret}"
      case Block(stmts, ret) => s"{${stmts map {
        case Left(t) => t.toString+"; "
        case Right(b) => b.toString+"; "
      } mkString}$ret}"
      case IntLit(n) => n.toString
      case IntOp(a, b, op) => s"[iop]($a,$b)"
      case Ite(c,t,e) => s"if $c then $t else $e"
      case NilExpr => s"nil"
      case FieldAccess(obj, id) => s"$obj.$id"
      case FieldAssign(obj, id, value) => s"$obj.$id <- $value"
      case Take(obj, id) => s"take $obj.$id"
    }
    
  }
  
}










