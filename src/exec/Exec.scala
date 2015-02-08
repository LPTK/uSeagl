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

class Exec {
  import Stages.Resolved._
  import collection.mutable.HashMap
//  import collection.mutable._
  
  type Gamma = Map[VId,Ptr]
  
  val h: Heap = new Heap
  
  def apply(e: Expr, g: Gamma = Map()) = {
    
    def ref(p: Ptr) = p match {
      case OwnPtr(a) => RefPtr(a)
      case _ => p
    }
    def mkUnit = h.alloc(Obj.empty)
    
    def rec(e: Expr)(implicit g: Gamma): Ptr = e match {
      
      case NilExpr => Nil
      
      case IntLit(n) => IntVal(n)
//      case BoolLit(b) => BoolVal(b)
      
      case Ite(c,t,e) =>
        val cp = rec(c)
        cp match {
          case IntVal(0) => rec(e)
          case IntVal(_) => rec(t)
          case _ => throw new IfCondEE(cp)
        }
      
      case Var(s) => ref(g(s.nam)) // TODO handle not in ctx
//      case Build(typ, args) => h.alloc(Obj(
//          typ.t.params.z map {p => }
//        ))
      
      case Build(typ, args) => typ.t.value match {
        case t: ConcTyp =>
          val par = t.params
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
        case t: AbsTyp => throw new AbsTypeBuildEE(t)
      }
      
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
        
      case Block(stmts, e) => stmts match {
        case Seq(Binding(id,e), rest @ _*) =>
          val p = rec(e)
          rec(Block(rest, e))(g + (id -> p))
        case Seq(ex: Expr, rest @ _*) =>
          rec(ex)
          rec(Block(rest, e))
        case Seq() => rec(e)
      }
      
    }
    
//    println(g)
    rec(e)(g)
  }
  
  def dispVal(v: Ptr, done: Set[Ptr] = Set()): Str = {
//  if (done(v)) "..." else {
    def dispAddr(a: Addr) = h.store get a match {
      case _ if (done(v)) => "..."
      case Some(obj) => obj.fields map {
        case (id,p) => s"$id: ${dispVal(p, done + v)}"
      } mkString ("{", ", ", "}")
      case None => "[deallocated]"
    }
    v match {
      case OwnPtr(a) => s"=> ${dispAddr(a)}"
      case RefPtr(a) => s"-> ${dispAddr(a)} @ ${a.value}"
      case Nil => "Nil"
      case IntVal(n) => s"$n"
    }
  }
  
}

























