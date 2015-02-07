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
  case class OwnPtr(a: Addr) extends Ptr
  case class RefPtr(a: Addr) extends Ptr
  case object Nil extends Ptr
  
  case class Obj(fields: Map[VId, Ptr])
  
  class Heap {
    private var nextAddr = Nat(0): Addr
    val objs = Map[Addr,Obj]()
    
    private def freshAddr = nextAddr oh_and (nextAddr += 1)
    
    def alloc(obj: Obj) = {
      val addr = freshAddr
      objs += (addr -> obj)
      OwnPtr(addr)
    }
    
    def dealloc(ptr: Ptr): Unit = ptr match {
      case OwnPtr(a) => dealloc(a)
      case _ =>
    }
    def dealloc(a: Addr) {
      check(objs isDefinedAt a)
      objs(a).fields mapValues dealloc
      check(objs isDefinedAt a)
      objs -= a
    }
    
  }
  
  
  
  
}



