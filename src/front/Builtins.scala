package front

import common.Stage
import common.Stages._

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
  
  

}



