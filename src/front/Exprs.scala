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
  case class FCall(fid: FunSym, targs: Opt[Seq[Type]], rargs: Opt[Seq[Reg]], args: Opt[Seq[Expr]]) extends BasicExpr
  case class Build(typ: Type, args: Seq[Expr]) extends BasicExpr
  
  case class Binding(nam: VId, value: Expr) extends Stmt
  case class Block(smts: Seq[Stmt]) extends BasicExpr
  
  
}










