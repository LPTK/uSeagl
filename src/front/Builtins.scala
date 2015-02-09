package front

import util._
import common.Stage
import common.Stages._
import Specs._

//case class Builtins[S <: Stage](s: S) {
//case class Builtins[S <: Stage](s: S) {
//self: Pgrms =>
object Builtins {
//  import s._
  import Ast._
  
  val btyps = Seq(
      ConcTyp(TId("Ref"), Seq(TId("T")), Seq(VId("Pte")), Seq()),
      ConcTyp(TId("Unit"), Seq(), Seq(), Seq()),
//      ConcTyp(TId("Bool"), Seq(), Seq(), Seq()),
      ConcTyp(TId("Int"), Seq(), Seq(), Seq())
  )
  
  val IntType = Type(TId("Int"), Seq(), Seq())
  
  def binIntOp(nam: Str)(op: (Int,Int) => Int) =
      Fun(FId(nam), Seq(), Seq(), Seq(Local(VId("a"), None), Local(VId("b"), None)), None, Spec.empty,
          IntOp(Var(VId("a")),Var(VId("b")),op))
  
  val bfuns = Seq(
//      Fun(FId("add"), Seq(), Seq(), Seq(Local(VId("a"), IntType)), None, Spec.empty, ???)
//      Fun(FId("add"), Seq(), Seq(), Seq(Local(VId("a"), None), Local(VId("b"), None)), None, Spec.empty,
//          IntOp(Var(VId("a")),Var(VId("b")),{_ + _}))
      binIntOp("add"){_ + _},
      binIntOp("eq"){(a,b) => if (a == b) 1 else 0}
  )

}



