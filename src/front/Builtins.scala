package front

import common.Stage

case class Builtins[S <: Stage](s: S) {
//case class Builtins[S <: Stage](s: S) {
//self: Pgrms =>
  import s._
  
  val btyps = Seq(
      ConcTyp(TId("Ref"), Seq(TId("T")), Seq(VId("Pte")), Seq()),
      ConcTyp(TId("Unit"), Seq(), Seq(), Seq())
  )
  
  

}



