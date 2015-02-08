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
    def mkUnit = ???
    
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
        
//        val fs = for (i <- 0 until par.size; id = par(i).nam; valu = rec(args(i)))
//          yield id -> valu;
        val fs = for (i <- 0 until par.size)
          yield par(i).nam -> rec(args(i));
        
//        h.alloc(Obj(HashMap(fs.toArray: _*)))
//        h.alloc(Obj(HashMap(fs.toMap)))
        h.alloc(Obj(HashMap(fs:_*)))
      case FCall(f, _, _, args) =>
        val par = f.params
        val g2 = for (i <- 0 until par.size)
          yield par(i).nam -> rec(args(i));
        rec(f.body)(g2 toMap)
      
      case FieldAccess(obj, id) =>
        val ptr = rec(obj)
        ptr match {
          case Ptr(a) => ref(h(a)(id))
          case Nil => Nil
        }
        
      case FieldAssign(obj, id, value) =>
        val ptr = rec(obj)
        val v = rec(value)
        ptr match {
          case Ptr(a) =>
            h(a)(id) = v
            mkUnit
          case Nil => mkUnit
        }
        
      case Take(obj, id) =>
        val ptr = rec(obj)
        ptr match {
          case Ptr(a) =>
            val r = h(a)(id) // TODO warn if taking a ref?
            h(a)(id) = Nil
            r
          case Nil => Nil
        }
        
        
    }
    
  }
  
  
  
}

















