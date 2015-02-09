package typing

import util._
import common._
import front._
import Stages._
import Reporting._
import Regions._
import Typed._
import Unify._
import collection._

object Unify {
   val singleStaged = SingleStaged(Typed)
}
class Unify(unifs: Map[AbsTyp,Type]) extends singleStaged.Identity {
  
//  println(unifs)
  
  var u = unifs
  
//  val unifsTrans = mutable.Map(unifs.toSeq: _*)
  val unifsTrans = mutable.Map[AbsTyp,Type]()
//  for ((at,t) <- unifs)
  for (k <- unifs.keys) {
    unifsTrans += (k -> u(k))
//    println(k, u(k))
    u = u mapValues apply
//    println(u)
  }
//  println
//  println(unifsTrans)
  
  /** Note: does not handle abs types with typ args */
  override def apply(x: Type) = (x.t.value match {
    case at: AbsTyp if unifsTrans isDefinedAt at => unifsTrans(at)
    case _ => super.apply(x)
  }) //and println
  override def tspec(x: TypeSpec) = apply(x)
  override def tparam(x: TypeParam) = apply(x)
  
  override def typs(x: TypSym) = mkUnique(x)
  override def funs(x: FunSym) = mkUnique(x)
  override def vars(x: a.VarSym) = apply(x)
  override def terms(x: Term) = Typd(apply(x.obj), apply(x.typ))
  
  
////  override def typs(x: TypSym) = mkCycle(x) //x oh_and println(x)
//  override def typs(x: TypSym) = x.value match {
//    case at: AbsTyp => mkCycle(unifs.getOrElse(at, at))
//    case ct: ConcTyp => mkCycle(ct)
//  }
//  override def tspec(x: TypeSpec) = apply(x) // oh_and println(x)
////  override def apply(x: Fun) = super.apply(x) oh_and println(x)
//  
//  override def tparam(x: TypeParam) = apply(x)
//  
////  override def apply(x: Typ) = (x match {
//////    case at: AbsTyp if unifs isDefinedAt at => unifs(at)
////    case at: AbsTyp => unifs.getOrElse(at, at)
////    case ct: ConcTyp => ct
////  }) oh_and println(x)
//  
////  val btyps = Seq() //wtf
}










