package exec

//import collection.mutable._
import Exceptions._
import Memory._
import common._
import util._
import front._
  
//object Exec {
//  import collection.mutable._
//  import Exceptions._
//  import util._
//  import front._
//  
//}

object Exec {
  import Stages.Resolved._
  import collection.mutable.HashMap
//  import collection.mutable._
  
  type Gamma = Map[VId,Ptr]
  
  def exec(e: Expr) = {
    val h: Heap = new Heap
    
    def ref(p: Ptr) = p match {
      case OwnPtr(a) => RefPtr(a)
      case _ => p
    }
    
    def rec(e: Expr)(implicit g: Gamma): Ptr = e match {
      case Var(s) => ref(g(s.nam))
//      case Build(typ, args) => h.alloc(Obj(
//          typ.t.params.z map {p => }
//        ))
      case Build(typ, args) =>
        val par = typ.t.params
//        h.alloc(Obj(HashMap(
//          { for (i <- 0 until par.size; id = par(i).nam; valu = rec(args(i)))
//            yield (id -> valu) } : _*
//        )))
        
        val fs = for (i <- 0 until par.size; id = par(i).nam; valu = rec(args(i)))
          yield id -> valu;
        
//        h.alloc(Obj(HashMap(fs.toArray: _*)))
//        h.alloc(Obj(HashMap(fs.toMap)))
        h.alloc(Obj(HashMap(fs:_*)))
      case FCall(f, _, _, args) =>
        val par = f.params
        val g2 = for (i <- 0 until par.size)
          yield par(i).nam -> rec(args(i));
        rec(f.body)(g2 toMap)
    }
    
  }
  
  
  
}

















