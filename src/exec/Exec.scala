package exec

//import collection.mutable._
import utils._
import Exceptions._
import Memory._
import common._
import front._
//import collection._
import Predef.{any2stringadd => _, _}
  
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
  import collection.mutable.ArrayBuffer
//  import collection.mutable._
  import collection.Set
  
  type Gamma = Map[VId,Value]
  
  val h: Heap = new Heap
  import h.dispVal
  
  def apply(e: Expr, g: Gamma = Map()) = {
    
//    def E[T] = Set[T]()
    val E = Seq()
    
    def ref(p: Value) = (p match {
      case OwnPtr(a) => RefPtr(a)
      case _ => p
    }) //and println
    def mkUnit = (h.alloc(Obj.empty), E)
    
    type Tmps = Seq[Ptr]
    
    implicit class TmpHelp(val x: (Value,Tmps)) {
      def + (tmp: Tmps) = (x._1, x._2 ++ tmp)
    }
    
    /** returns the result value and temporaries to be deallocated */
    def rec(e: Expr)(implicit g: Gamma): (Value,Tmps) = e match {
      
      case NilExpr => (Nil,E)
      
      case IntLit(n) => (IntVal(n),E)
//      case BoolLit(b) => BoolVal(b)
      
      case Ascribe(e, _) => rec(e)
      
      case IntOp(lhs,rhs,op) =>
        val (v1,t1) = rec(lhs)
        val (v2,t2) = rec(rhs)
        (v1,v2) match {
          case (IntVal(l),IntVal(r)) => (IntVal(op(l,r)), t1 ++ t2)
          case _ => throw ExecException(s"Illegal operands for integer peration: $v1, $v2")
        }
      
      case Ite(c,t,e) =>
        val (cp,tmp) = rec(c)
        cp match {
          case IntVal(0) => rec(e) + tmp
          case IntVal(_) => rec(t) + tmp
          case Nil => rec(e) + tmp
          case _ => throw new IfCondEE(cp)
        }
      
      case Var(s) => (ref(g(s.nam)), E) // TODO handle not in ctx
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
          val tmps = ArrayBuffer[Ptr]()
          val fs = for (i <- 0 until par.size; (valu,tmp) = rec(args(i)))
            yield par(i).nam -> valu oh_and (tmps ++= tmp)
          
  //        h.alloc(Obj(HashMap(fs.toArray: _*)))
  //        h.alloc(Obj(HashMap(fs.toMap)))
          (h.alloc(Obj(HashMap(fs:_*))), tmps)
        case t: AbsTyp => throw new AbsTypeBuildEE(t)
      }
      
      case FCall(f, _, _, args) =>
        val par = f.params
        val tmps = ArrayBuffer[Ptr]()
        val g2 = for (i <- 0 until par.size; (valu,tmp) = rec(args(i)))
          yield par(i).nam -> valu oh_and (tmps ++= tmp)
        rec(f.body)(g2 toMap) + tmps + g2.collect{case(id,ptr:Ptr) => ptr}
      
      case FieldAccess(obj, id) =>
        val (ptr,tmp) = rec(obj)
        ptr match {
          case Ptr(a) => (ref(h(a)(id)), tmp)
          case Nil => (Nil, tmp)
          case _ => throw new FieldAccessEE(ptr)
        }
        
      case FieldAssign(obj, id, value) =>
        val (ptr,tmp) = rec(obj)
        val (v,tmp2) = rec(value)
        ptr match {
          case Ptr(a) =>
//            println(dispVal(h(a)(id)))
            h.dealloc(h(a)(id))
//            println(a,id,h(a)(id))
//            println(dispVal(h(a)(id)))
            h(a)(id) = v
            mkUnit + tmp + tmp2
          case Nil => mkUnit + tmp + tmp2
          case _ => throw new FieldAccessEE(ptr)
        }
        
      case Take(obj, id) =>
        val (ptr,tmp) = rec(obj)
        ptr match {
          case Ptr(a) =>
            val r = h(a)(id) // TODO warn if taking a ref?
            h(a)(id) = Nil
            (r,tmp)
          case Nil => (Nil, tmp)
          case _ => throw new FieldAccessEE(ptr)
        }
        
      /** At block exit, every (local) temporary is deallocated, as well as intermediate exprs */
      case Block(stmts, e) => stmts match {
        case Seq(Right(Binding(Local(_,id,_),e2)), rest @ _*) =>
          val (p,t) = rec(e2)
          val (p2,t2) = rec(Block(rest, e))(g + (id -> p)) + t
//          h.dealloc(p)
//          t2 foreach h.dealloc
          h.dealloc(t2 :+ p)
          (p2,E)
        case Seq(Left(ex: Expr), rest @ _*) =>
          val (p,t) = rec(ex)
          val (p2,t2) = rec(Block(rest, e))
//          h.dealloc(p)
//          t2 foreach h.dealloc
          h.dealloc(t2 :+ p)
          (p2,E)
        case Seq() =>
          val (p,t) = rec(e)
          t foreach h.dealloc
//          println(g,e)
          (p,E)
      }
      
    }
    
//    println(e)
    val (ptr,tmp) = rec(e)(g)
    h.dealloc(tmp)
    ptr
  }
  
}

























