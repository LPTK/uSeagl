package front

import utils._
import common.Stage
import common.Stages._
import Specs._
import Parser._

object Builtins {
  import Ast._
  
  def typtxt(txt: Str): ConcTyp = phrase(typ)(new lexical.Scanner(txt)).get
  
  val btyps = Seq(
      ConcTyp(new TUid, TId("Ref"), Seq(TId("T")), Seq(VId("Pte")), Seq()),
      ConcTyp(new TUid, TId("Unit"), Seq(), Seq(), Seq()),
      ConcTyp(new TUid, TId("Int"), Seq(), Seq(), Seq()),
      typtxt("typ Pair(fst,snd)"),
      typtxt("typ Couple[T](fst:T,snd:T)"),
      typtxt("typ Cell(val)"),
      typtxt("typ RefCell(r: Ref)")
  )
  
  val IntType = Type(TId("Int"), Seq(), Seq())
  
  def binIntOp(nam: Str)(op: (Int,Int) => Int) =
      Fun(new FUid, FId(nam), Seq(), Seq(),
          Seq(Local(new VUid, VId("a"), None), Local(new VUid, VId("b"), None)),
          None, Spec.empty,
          IntOp(Var(VId("a")),Var(VId("b")),op));
  
  def funtxt(txt: Str): Fun = phrase(fun)(new lexical.Scanner(txt)).get
  
  val bfuns = Seq(
      binIntOp("add"){_ + _},
      binIntOp("eq"){(a,b) => if (a == b) 1 else 0},
      funtxt("fun id(x) = x"),
      funtxt("fun refto{r}(x: Ref{r}) = x  // useful to require a particular region for a ref"),
      funtxt("fun coe[T](x:T,y:T) = ()  // coercition of two values' types")
  )

}



