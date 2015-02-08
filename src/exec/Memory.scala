package exec

object Memory {
  import collection.mutable._
  import util._
  import common._
  import Stages.Resolved._
//  import Exec._
  import Exceptions._
  import front._
  
  type Addr = Nat
  
//  type Heap = Map[Addr,Obj]
  
  sealed trait Ptr
  object Ptr {
    def unapply(p: Ptr) = p match {
      case OwnPtr(a) => Some(a)
      case RefPtr(a) => Some(a)
      case Nil => None
      case IntVal(_) => None
    }
  }
  case class OwnPtr(a: Addr) extends Ptr
  case class RefPtr(a: Addr) extends Ptr
  case object Nil extends Ptr
  case class IntVal(n: Int) extends Ptr
  
  case class Obj(fields: Map[VId, Ptr]) {
    def apply(id: VId) = fields(id)  // TODO handle field missing
    def update(id: VId, v: Ptr) = fields(id) = v // idem
  }
  object Obj {
    val empty = Obj(Map())
  }
  
  class Heap {
    private var nextAddr = Nat(0): Addr
    val store = Map[Addr,Obj]()
    
    private def freshAddr = nextAddr oh_and (nextAddr += 1)
    
    def alloc(obj: Obj) = {
      val addr = freshAddr
      store += (addr -> obj)
      OwnPtr(addr)
    }
    
    def dealloc(ptr: Ptr): Unit = ptr match {
      case OwnPtr(a) => dealloc(a)
      case _ =>
    }
    def dealloc(a: Addr) {
      check(store isDefinedAt a)
      store(a).fields mapValues dealloc
      check(store isDefinedAt a)
      store -= a
    }
    
//    def apply(p: Ptr) = p match {
//      case Nil =>
//    }
    def apply(a: Addr) = store(a)
    
  }
  
  
  
  
}



