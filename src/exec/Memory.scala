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
  
  sealed trait Value {
    
    override def toString = this match {
      case OwnPtr(a) => s"Own @$a"
      case RefPtr(a) => s"Ref @$a"
      case Nil => "Nil"
      case IntVal(n) => s"$n"
    }
//    override def toString = toString(Set())
//    def toString(done: Set[Ptr]) = this match {
//      case OwnPtr(a) => s"Own @$a = ${dispObj(a)}"
//      case RefPtr(a) => s"Ref @$a = ${dispObj(a)}"
//      case Nil => "Nil"
//      case IntVal(n) => s"$n"
//    }
    
  }
  
  sealed trait Ptr extends Value
  
  object Ptr {
    def unapply(p: Value) = p match {
      case OwnPtr(a) => Some(a)
      case RefPtr(a) => Some(a)
      case Nil => None
      case IntVal(_) => None
    }
  }
  case class OwnPtr(a: Addr) extends Ptr
  case class RefPtr(a: Addr) extends Ptr
  case object Nil extends Value
  case class IntVal(n: Int) extends Value
  
  case class Obj(fields: Map[VId, Value]) {
    def apply(id: VId) = fields(id)  // TODO handle field missing
    def update(id: VId, v: Value) = fields(id) = v // idem
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
    
    def dealloc(ptrs: collection.Traversable[Value]) {
      ptrs foreach dealloc
    }
    def dealloc(ptr: Value): Unit = ptr match {
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



