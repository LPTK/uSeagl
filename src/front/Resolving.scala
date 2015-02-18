package front

import utils._
import common._
import Stages._
import Specs._
import Proxy._
import scala.util.{Try, Success, Failure}
import common.Reporting
import collection.mutable.ArrayBuffer


class Presolve extends StageConverter(Ast, Resolving) {
  import scala.util.Success
  import Reporting.IdentifierNotFound
  import collection.mutable._
  import Resolving._
  import b._
  
//  val state = new StageState(Resolving) {
//    val funTable = HashMap[FUid, Cyclic[s.Fun]]()
//    val typTable = HashMap[TUid, Cyclic[s.Typ]]()
//    val varTable = HashMap[VUid, s.Local]()
//  }
  
//  Predef
  scala.util.control.NonFatal /** if not init here SOF will not be handled correctly; cf https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM */
  
  case class Ctx (
      typTable: Map[TId, Typ],
      funTable: Map[FId, Fun],
      locTable: Map[VId, Local],
      parent: Option[Ctx] )
  {
    def apply(x: TId): Typ = (typTable get x, parent) match {
      case (Some(v), _) => v
      case (_, Some(p)) => p(x)
      case _ => throw IdentifierNotFound(x)
    }
    def update(x: TId, y: Typ) = typTable(x) = y
    
    def apply(x: FId): Fun = (funTable get x, parent) match {
      case (Some(v), _) => v
      case (_, Some(p)) => p(x)
      case _ => throw IdentifierNotFound(x)
    }
    def update(x: FId, y: Fun) = funTable(x) = y
    
    def apply(x: VId): Local = (locTable get x, parent) match {
      case (Some(v), _) => v
      case (_, Some(p)) => p(x)
      case _ => throw IdentifierNotFound(x)
    }
    def update(x: VId, y: Local) = locTable(x) = y
    
  }
  private var ctx = Ctx(Map(), Map(), Map(), None)
  def pushCtx = ctx = Ctx(Map(), Map(), Map(), Some(ctx))
  def popCtx = ctx = ctx.parent.get
  
  def getCtx = ctx
  
//  Builtins.btyps foreach apply
  
  val btyps = Builtins.btyps map apply
  val bfuns = Builtins.bfuns map apply
  
//  def ult[T](e: => T) = Lazy(Try(e).transform(x=>Try(x), {
//    case e: java.util.NoSuchElementException => Failure(IdentifierNotFound(null))
//  })) // Lazy(e) //unit(Lazy(Try(e)))
  def ult[T](e: => T) = Lazy(e)
  
  def typs(x: a.TypSym) = {val c = ctx; Lazy(c(x))} // ult(ctx(x))
  def funs(x: a.FunSym) = {val c = ctx; Lazy(c(x))} // ult(ctx(x))
  def vars(x: a.VarSym) =
//    ult(ctx(x))
//    Lazy{println(ctx);ctx(x)}
    {val c = ctx; Lazy(c(x))}
  def terms(x: a.Term)  = apply(x)
  
  def tspec(x: a.TypeSpec) = x map apply
  def tparam(x: a.TypeParam) = AbsTyp(new TUid, x, Seq(), Seq(), false, true) and (ctx(x) = _)
  
  
  override def delegate(x: a.Fun) = { // TODO use and
    pushCtx
//    x.typs foreach (p => ctx(p) = AbsTyp(p, Seq(), Seq()))
    x.params foreach (p => ctx(p.nam) = apply(p))
////    println(ctx.locTable)
//    val r = super.apply(x)
//    popCtx
//    ctx(x.nam) = r
//    r
    super.delegate(x) and {popCtx; ctx(x.nam) = _}
  }
  override def apply(x: a.ConcTyp) =
    { pushCtx; super.apply(x) } and { popCtx; ctx(x.nam) = _ }
//  override def apply(x: a.ConcTyp) = {
//    pushCtx
//    x.typs foreach (p => ctx(p) = AbsTyp(p, Seq(), Seq()))
//    super.apply(x)
//  } and { popCtx; ctx(x.nam) = _ }
  
//  override def apply(x: a.Binding) =
////    super.apply(x) and (ctx(x.nam) = (_:Binding).value)
////    super.apply(x) and ((b:Binding) => ctx(x.nam) = Local(x.nam, b.value))
//    super.apply(x) oh_and (ctx(x.nam) = Local(new VUid, x.nam, None))
  override def apply(x: a.Binding) =
    super.apply(x) and (b => ctx(x.loc.nam) = b.loc) // should probably do this ctx update in apply Local
  
  override def apply(x: a.Expr): Expr = x match {
    case a.Block(s,e) =>
      pushCtx; super.apply(x) oh_and popCtx
    case _ => super.apply(x)
  }
  
}

//class Resolve extends StageConverter[Resolving.type, Resolved.type](Resolving, Resolved) with FallibleConverter {
class Resolve(ps: Presolve) extends StageConverter(Resolving, Resolved) {
  import b._
  
  val btyps = ps.btyps map apply //ps.btyps map getUnique // apply
  val bfuns = ps.bfuns map (_ value) map apply //getUnique
  
  def typs(x: a.TypSym) = apply(x.get)//getUnique(x.get) //apply(x.get) //x.get flatMap apply
  def funs(x: a.FunSym) = apply(x.get) //getUnique(x.get)
//    Lazy(apply(x.get))
//    apply(new Cyclic[a.Fun](_ => x.get))
  def vars(x: a.VarSym) = apply(x.get) //apply(x.get) //x.get flatMap apply
  def terms(x: a.Term)  = apply(x)
  
  def tspec(x: a.TypeSpec) = x map apply
//  def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp] // TODO make cleaner // apply(x)
  def tparam(x: a.TypeParam) = apply(x:a.Typ).value.asInstanceOf[AbsTyp] // TODO make cleaner // apply(x)
  
  
  
  
  
//  def trans(x: a.TypSym) = ???
//  def trans(x: a.FunSym) = ???
//  def trans(x: a.VarSym) = ???
  
  
}






























