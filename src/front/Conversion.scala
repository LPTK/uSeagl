package front

import util._
import common._
import Stages._
import Specs._
import Proxy._
//import scala.util.parsing.input.Positional
import scala.util.{Try, Success, Failure}


abstract case class StageConverter[A <: Stage, B <: Stage](a: A, b: B) extends Converter {
  import collection.mutable.HashMap
  import b._
  
  /** Polymorphic types */
  
  def typs(x: a.TypSym): Result[b.TypSym]
  def funs(x: a.FunSym): Result[b.FunSym]
  def vars(x: a.VarSym): Result[b.VarSym]
  def terms(x: a.Term): Result[b.Term]
  
  
  /** Trees */
  
  def apply(x: a.Expr) = x match {
    case a.Var(vs) => vars(vs) map { Var }
//    case a.FCall(fs,ta,ra,a) => b.FCall(fun(fs), )
    case a.FCall(fs,ta,ra,a) => for {
      f <- funs(fs)
    } yield FCall(f, None, None, None)
  }
  
  def opt[T,U](x: Opt[T], f: T => Result[U]): Result[Opt[U]] =
//    x flatMap (y => f(y) map (Some.apply))
//    x flatMap (y => f(y) map (Some.apply))
//    x flatMap {
//      y =>
//        val z = f(y)
//        val a = z map (u => Some(u): Opt[U])
//        a
//    }
    x match {
      case Some(y) => f(y) map (Some.apply)
      case None => unit(None)
    }
  
//  def apply(x: a.Fun): Result[Fun] =
//  if (funs isDefinedAt x) funs(x)
//  else {
//    val r = new Cyclic[Result[Fun]]({
//      cf =>
//        funs += ((x -> cf))
//        for {
//          r <- opt[a.Type,Type](x.ret, apply)  // apply(x.ret)
//      //    r <- (opt(x.ret, apply):Result[Type])
//      //    r <- opt(x.ret, apply[a.Type](_))//:Result[Type])
//          b <- terms(x.body)
//        } yield Fun(x.nam, Seq(), Seq(), Seq(), r, Spec.empty, b)
//    })
//    r.value
//  }
  def apply(x: a.Fun): Result[Fun] = for {
    r <- opt[a.Type,Type](x.ret, apply)  // apply(x.ret)
//    r <- (opt(x.ret, apply):Result[Type])
//    r <- opt(x.ret, apply[a.Type](_))//:Result[Type])
    b <- terms(x.body)
  } yield Fun(x.nam, Seq(), Seq(), Seq(), r, Spec.empty, b)
  
  def apply(x: a.Type): Result[Type] = for {
    t <- typs(x.t)
  } yield Type(t, None, None)
  
  def apply(x: a.Typ): Result[Typ] = ???
  
  def apply(x: a.Var): Result[Var] = ???
  
  def apply(x: a.Local): Result[Local] = ???
  
  
  /** Cycle handling */
  
//  val funs = HashSet[Cyclic[Fun]]()
//  val funs = HashMap[Cyclic[a.Fun], Cyclic[Result[Fun]]]()
  val funs = HashMap[a.Fun, Cyclic[Result[Fun]]]()
//  val funs = HashMap[a.Fun, Result[Fun]]()
//  val funs = HashMap[a.Fun, Cyclic[Result[Fun]]]()
  
  def apply(x: Cyclic[a.Fun]): Cyclic[Result[Fun]] = {
    val k = x.value
    println(s"cf ${funs isDefinedAt k}  ${k}")
    if (funs isDefinedAt k) funs(k)
    else new Cyclic[Result[Fun]]({
      cf =>
        funs += ((k -> cf))
        apply(k)
    })
  }
}

case class SingleStaged(s: Stage) {
  abstract class Identity extends front.StageConverter[s.type,s.type](s, s) {
    
    def typs(x: a.TypSym) = unit(x)
    def funs(x: a.FunSym) = unit(x)
    def vars(x: a.VarSym) = unit(x)
    
  }
}

//class Read extends StageConverter[Ast.type, Resolving.type](Ast, Resolving) with DirectConverter {
class Read extends StageConverter(Ast, Resolving) with DirectConverter {
  import scala.util.Success
  import collection.mutable._
  import Resolving._
  import b._
  
//  Predef
  scala.util.control.NonFatal /** if not init here SOF will not be handled correctly; cf https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM */
  
  case class Ctx (
      typTable: Map[TId, Typ],
      funTable: Map[FId, Fun],
      locTable: Map[VId, Local],
      parent: Option[Ctx] )
  {
    def apply(x: TId) = typTable(x)
    def update(x: TId, y: Typ) = typTable(x) = y
    
    def apply(x: FId) = funTable(x)
    def update(x: FId, y: Fun) = funTable(x) = y
    
    def apply(x: VId) = locTable(x)
    def update(x: VId, y: Local) = locTable(x) = y
  }
  private var ctx = Ctx(Map(), Map(), Map(), None)
  
  def ult[T](e: => T) = unit(Lazy(e)) //unit(Lazy(Try(e)))
  
  def typs(x: a.TypSym) = ult(ctx(x))
  def funs(x: a.FunSym) = ult(ctx(x))
  def vars(x: a.VarSym) = ult(ctx(x))
  def terms(x: a.Term)  = apply(x)
  
  override def apply(x: a.Fun) = { // TODO use oh_and
    val r = super.apply(x)
    ctx(x.nam) = r.get
    r
  }
}

//class Resolve extends StageConverter[Resolving.type, Resolved.type](Resolving, Resolved) with FallibleConverter {
class Resolve extends StageConverter(Resolving, Resolved) with FallibleConverter {
  import b._
  
  type Result[+T] = Try[T]
  def succeed[T](a: => T) = Try(a)
  def fail[T](e: Throwable) = Failure(e)
  
  
  def typs(x: a.TypSym) = apply(x.get) //x.get flatMap apply
  def funs(x: a.FunSym) =
//  def funs(x: Lazy[Try[a.Fun]]) =
//    x.get flatMap (x => apply(new Cyclic[Resolving.Fun](_ => x)))
//    apply(new Cyclic[Resolving.Fun](_ => apply(x.get)))
//    Try(apply(new Cyclic[Resolving.Fun](_ => x.get.get)) map (_ get))
//    Try(apply(new Cyclic[Resolving.Fun](_ => x.get)))
//    apply(new Cyclic[Resolving.Fun](_ => x.get))
  {
//    val c = 
//    Try(new Cyclic[Fun](_ => apply(x.get)))
//    val ct = apply(x.get)
    
//    val ct = apply(new Cyclic[a.Fun](_ => x.get))
////    ct.value map (Try.apply)
////    Lazy(ct.value)
////    ct.value map (Lazy)
//    succeed(Lazy(ct.value.get)) // won't get the exception...
    
    succeed(Lazy(apply(x.get).get)) // won't get the exception...
  }
  def vars(x: a.VarSym) = apply(x.get) //x.get flatMap apply
  def terms(x: a.Term)  = apply(x)
  
  
  
  
  
//  def trans(x: a.TypSym) = ???
//  def trans(x: a.FunSym) = ???
//  def trans(x: a.VarSym) = ???
  
  
}






//class StageIdentity(val s: Stage) extends StageConverter(s, s) {
//class StageIdentity[S <: Stage](val s: S) extends StageConverter[S,S](s, s) {

//trait Lol[A <: Stage, B <: Stage] extends Converter {
//  
//  def typs(x: A# TypSym): Result[B# TypSym]
//  def funs(x: A# FunSym): Result[B# FunSym]
//  def vars(x: A# VarSym): Result[B# VarSym]
//
//}
//class StageIdentity[S <: Stage](val s: S) extends Lol[S,S] {
//  
////  def typs(x: s.TypSym) = unit(x)
////  def funs(x: s.FunSym) = unit(x)
////  def vars(x: s.VarSym) = unit(x)
//  def typs(x: S# TypSym) = unit(x)
//  def funs(x: S# FunSym) = unit(x)
//  def vars(x: S# VarSym) = unit(x)
//  
//  def unit[T](a: => T): Result[T] = ???
//  
//}





















