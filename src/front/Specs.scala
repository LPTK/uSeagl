package front

object Specs {
  import utils._
  import Regions._
  
  case class Spec(read: Reg, inval: Reg, transfers: Seq[Trans])
  object Spec {
    val empty = Spec(Reg.empty, Reg.empty, Seq())
  }
  
  case class Trans(from: Reg, to: Reg, total: Bool)
  
  
}