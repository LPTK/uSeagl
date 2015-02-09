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
class Unify(unifs: Map[AbsTyp,Typ]) extends singleStaged.Identity {
  
  println(unifs)
  
//  override def typs(x: TypSym) = mkCycle(x) //x oh_and println(x)
  override def typs(x: TypSym) = x.value match {
    case at: AbsTyp => mkCycle(unifs.getOrElse(at, at))
    case ct: ConcTyp => mkCycle(ct)
  }
  override def tspec(x: TypeSpec) = apply(x) // oh_and println(x)
//  override def apply(x: Fun) = super.apply(x) oh_and println(x)
  
  override def tparam(x: TypeParam) = apply(x)
  
//  override def apply(x: Typ) = (x match {
////    case at: AbsTyp if unifs isDefinedAt at => unifs(at)
//    case at: AbsTyp => unifs.getOrElse(at, at)
//    case ct: ConcTyp => ct
//  }) oh_and println(x)
  
//  val btyps = Seq() //wtf
}










