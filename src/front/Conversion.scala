package front

import util._
import common._
import Stages._
import Specs._
import Proxy._
//import scala.util.parsing.input.Positional
import scala.util.{Try, Success, Failure}


abstract case class StageConverter[A <: Stage, B <: Stage](a: A, b: B) {
  import collection.mutable.HashMap
  import b._
  
  /** Polymorphic types */
  
  def typs(x: a.TypSym): b.TypSym
  def funs(x: a.FunSym): b.FunSym
  def vars(x: a.VarSym): b.VarSym
  def terms(x: a.Term): b.Term
  
  
  /** Trees */
  
  def apply(x: a.Expr) = x match {
    case a.Var(vs) => Var(vars(vs))
//    case a.FCall(fs,ta,ra,a) => b.FCall(fun(fs), )
    case a.FCall(fs,ta,ra,a) => FCall(funs(fs), None, None, Seq())
  }
  
  def apply(x: a.Fun): Fun = Fun(x.nam, x.typs, x.regs, x.params map apply, x.ret map apply, Spec.empty, terms(x.body)) 
  
  def apply(x: a.Type): Type = Type(typs(x.t), None, None)
  
  def apply(x: a.Typ): Typ = Typ(x.nam, x.typs, x.regs, x.params map apply)
  
  def apply(x: a.Var): Var = ???
  
  def apply(x: a.Local): Local = Local(x.nam, apply(x.typ))
  
  
  /** Cycle handling */
  
//  val funs = HashSet[Cyclic[Fun]]()
//  val funs = HashMap[Cyclic[a.Fun], Cyclic[Result[Fun]]]()
  val funs = HashMap[a.Fun, Cyclic[Fun]]()
//  val funs = HashMap[a.Fun, Result[Fun]]()
//  val funs = HashMap[a.Fun, Cyclic[Result[Fun]]]()
  val typs = HashMap[a.Typ, Cyclic[Typ]]()
  
//  def apply(x: Cyclic[a.Fun]): Cyclic[Fun] = mkCycle(x.value)
//  def apply(x: Cyclic[a.Typ]): Cyclic[Typ] = mkCycle(x.value)
  
  def mkCycle(k: a.Fun): Cyclic[Fun] = {
//    println(s"cf ${funs isDefinedAt k}  ${k}")
    if (funs isDefinedAt k) funs(k)
    else new Cyclic[Fun]({
      cf =>
        funs += ((k -> cf))
        apply(k)
    })
  }
  def mkCycle(k: a.Typ): Cyclic[Typ] = {
//    println(s"cf ${typs isDefinedAt k}  ${k}")
    if (typs isDefinedAt k) typs(k)
    else new Cyclic[Typ]({
      cf =>
        typs += ((k -> cf))
        apply(k)
    })
  }
}

case class SingleStaged(s: Stage) {
  abstract class Identity extends front.StageConverter[s.type,s.type](s, s) {
    
    def typs(x: a.TypSym) = x
    def funs(x: a.FunSym) = x
    def vars(x: a.VarSym) = x
    
  }
}

//class Read extends StageConverter[Ast.type, Resolving.type](Ast, Resolving) with DirectConverter {
class Read extends StageConverter(Ast, Resolving) {
  import scala.util.Success
  import Reporting.IdentifierNotFound
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
//    def check(id: Id, m: Map[Id, _]) = ???
    def get[K<:Id,V](id: K, m: Map[K, V]) =
      if (m isDefinedAt id) m(id)
      else throw IdentifierNotFound(id)
    
    def apply(x: TId) = get(x, typTable)//typTable(x)
    def update(x: TId, y: Typ) = typTable(x) = y
    
    def apply(x: FId) = get(x, funTable)
    def update(x: FId, y: Fun) = funTable(x) = y
    
    def apply(x: VId) = get(x, locTable)
    def update(x: VId, y: Local) = locTable(x) = y
  }
  private var ctx = Ctx(Map(), Map(), Map(), None)
  
  val builtins = Builtins(Ast)  // Builtins[Ast.type](Ast)
//  builtins.btyps foreach (t => ctx(t.nam) = apply(t))
  builtins.btyps foreach apply
    
//  def ult[T](e: => T) = Lazy(Try(e).transform(x=>Try(x), {
//    case e: java.util.NoSuchElementException => Failure(IdentifierNotFound(null))
//  })) // Lazy(e) //unit(Lazy(Try(e)))
  def ult[T](e: => T) = Lazy(e)
  
  def typs(x: a.TypSym) = ult(ctx(x))
  def funs(x: a.FunSym) = ult(ctx(x))
  def vars(x: a.VarSym) = ult(ctx(x))
  def terms(x: a.Term)  = apply(x)
  
  override def apply(x: a.Fun) = { // TODO use and
    val r = super.apply(x)
    ctx(x.nam) = r
    r
  }
  override def apply(x: a.Typ) =
    super.apply(x) and (ctx(x.nam) = _)
  
}

//class Resolve extends StageConverter[Resolving.type, Resolved.type](Resolving, Resolved) with FallibleConverter {
class Resolve extends StageConverter(Resolving, Resolved) {
  import b._
  
  def typs(x: a.TypSym) = mkCycle(x.get) //apply(x.get) //x.get flatMap apply
  def funs(x: a.FunSym) = mkCycle(x.get)
//    Lazy(apply(x.get))
//    apply(new Cyclic[a.Fun](_ => x.get))
  def vars(x: a.VarSym) = apply(x.get) //x.get flatMap apply
  def terms(x: a.Term)  = apply(x)
  
  
  
  
  
//  def trans(x: a.TypSym) = ???
//  def trans(x: a.FunSym) = ???
//  def trans(x: a.VarSym) = ???
  
  
}

























