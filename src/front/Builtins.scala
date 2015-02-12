package front

import util._
import common.Stage
import common.Stages._
import Specs._

object Builtins {
  import Ast._
  
  val btyps = Seq(
      ConcTyp(TId("Ref"), Seq(TId("T")), Seq(VId("Pte")), Seq()),
      ConcTyp(TId("Unit"), Seq(), Seq(), Seq()),
      ConcTyp(TId("Int"), Seq(), Seq(), Seq())
  )
  
  val IntType = Type(TId("Int"), Seq(), Seq())
  
  def binIntOp(nam: Str)(op: (Int,Int) => Int) =
      Fun(FId(nam), Seq(), Seq(), Seq(Local(VId("a"), None), Local(VId("b"), None)), None, Spec.empty,
          IntOp(Var(VId("a")),Var(VId("b")),op))
  
  val bfuns = Seq(
      binIntOp("add"){_ + _},
      binIntOp("eq"){(a,b) => if (a == b) 1 else 0}
  )

}



