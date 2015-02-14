package front

import util._
import common._
import Stages._
import Specs._
import Proxy._
import scala.util.{Try, Success, Failure}
import common.Reporting
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection._

abstract class StageState[S <: Stage](val s: S) {
  val funTable: HashMap[FUid, Cyclic[s.Fun]]
  val typTable: HashMap[TUid, Cyclic[s.Typ]]
  val varTable: HashMap[VUid, s.Local]
  
  var renewed = None: Opt[mutable.Set[Any]]
}

abstract case class StageConverter[A <: Stage, B <: Stage](a: A, b: B) {
  import b._
  
//  val state: StageState[b.type]
  val state = new StageState[b.type](b) {
    val funTable = HashMap[FUid, Cyclic[s.Fun]]()
    val typTable = HashMap[TUid, Cyclic[s.Typ]]()
    val varTable = HashMap[VUid, s.Local]()
  }
  import state._
  
  
  /** Delayed checking useful for avoiding illegal cyclic dependencies */
  
  val delayedChecks = ArrayBuffer[()=>Unit]()
  def delayCheck(ch: => Unit) = delayedChecks += (() => ch)
  def flushChecks {
//    println("flush...")
    val chks = delayedChecks.clone
    delayedChecks.clear
    chks foreach (_ apply)
  }
  
//  private var renewing = false
////  def renew[T](f: => T) = { renewing = true; f } oh_and (renewing = false)
//  def renewTree[T](f: => T) = try { renewing = true; f }
////  catch {
////    case _: Throwable => renewing = false
////  }
//  finally { renewing = false }
  
//  private var renewed = None: Opt[mutable.Set[Any]]
  def isRenewed(x: Any): Bool = renewed map (_ apply x) getOrElse true
  def renewTree[T](f: => T) = try { renewed = Some(mutable.HashSet()); f }
  finally { renewed = None }
  
  
  /** Polymorphic definitions */
  
  def typs(x: a.TypSym): b.TypSym
  def funs(x: a.FunSym): b.FunSym
  def vars(x: a.VarSym): b.VarSym
  def terms(x: a.Term):  b.Term
  
  def tspec(x: a.TypeSpec):  b.TypeSpec
  def tparam(x: a.TypeParam):  b.TypeParam
  
  
  /** Trees */
  
  def apply(x: a.Expr): Expr = x match {
    case a.NilExpr => NilExpr
    case a.IntLit(n) => IntLit(n)
    case a.IntOp(a,b,o) => IntOp(terms(a),terms(b),o)
    case a.Var(vs) => Var(vars(vs))
//    case a.Block(s,e) => Block(s map apply, terms(e))
    case a.Block(s,e) => Block(s map { case Left(t) => Left(terms(t)) case Right(b) => Right(apply(b)) }, terms(e))
    case a.Ite(c,t,e) => Ite(terms(c),terms(t),terms(e))
    case a.Build(t,a) => Build(apply(t), a map terms)
    case a.FCall(fs,ta,ra,a) => FCall(funs(fs), ta map apply, ra, a map terms)
    case a.FieldAccess(e, id) => FieldAccess(terms(e), id)
    case a.FieldAssign(e, id, v) => FieldAssign(terms(e), id, terms(v))
  }
  
  final
//  def apply(x: a.Fun): Fun =
  def apply(x: a.Fun): Cyclic[Fun] =
//    funTable getOrElse (x.uid, getUnique(x)) value
    if ((funTable isDefinedAt x.uid) && isRenewed(x)) funTable(x.uid)
    else { renewed map (_ += x); getUnique(x) }
  
  def delegate(x: a.Fun): Fun =
    Fun(x.uid, x.nam, x.typs map tparam, x.regs, x.params map apply, tspec(x.ret), Spec.empty, terms(x.body))
  
  def apply(x: a.Type): Type = Type(typs(x.t), x.targs map apply, x.rargs)
  
  final def apply(x: a.Typ): Cyclic[Typ] =
//    typTable getOrElse (x.uid, getUnique(x)) value
    if ((typTable isDefinedAt x.uid) && isRenewed(x)) typTable(x.uid)
    else { renewed map (_ += x); getUnique(x) }
  
  def delegate(x: a.Typ): Typ = x match {
    case t: a.ConcTyp => apply(t)
    case t: a.AbsTyp => apply(t)
    case _ => wtf // cf rm warning
  }
  /** The following are error prone since they don't check typTable! */
  def apply(x: a.ConcTyp): ConcTyp = ConcTyp(x.uid, x.nam, x.typs map tparam, x.regs, x.params map apply)
  def apply(x: a.AbsTyp): AbsTyp = AbsTyp(x.uid, x.nam, x.typs map tparam, x.regs, x.userDefined)
  
  def apply(x: a.Stmt): Stmt = x match {
    case x: a.Expr => apply(x)
    case x: a.Binding => apply(x)
  }
  
  def apply(x: a.Local): Local =
//    if (varTable isDefinedAt x.uid) varTable(x.uid)
//    else Local(x.uid, x.nam, tspec(x.typ)) and (varTable(x.uid) = _)
    if ((varTable isDefinedAt x.uid) && isRenewed(x)) varTable(x.uid)
//    else Local(x.uid, x.nam, tspec(x.typ)) and (varTable(x.uid) = _)
////    oh_and (renewed map (_ += x)) // not actually necessary
    else { renewed map (_ += x); Local(x.uid, x.nam, tspec(x.typ)) } and (varTable(x.uid) = _)
  
//  def apply(x: a.Binding): Binding =
//    Binding(x.nam, terms(x.value))
  def apply(x: a.Binding): Binding =
    Binding(apply(x.loc), terms(x.value))
  
  
  /** Cycle handling */
  
//  def apply(x: Cyclic[a.Fun]): Cyclic[Fun] = mkCycle(x.value)
//  def apply(x: Cyclic[a.Typ]): Cyclic[Typ] = mkCycle(x.value)
  
  def getUnique(k: a.Fun): Cyclic[Fun] = {
////    println(s"cf ${funs isDefinedAt k}  ${k}")
//    if (funTable isDefinedAt k.uid) funTable(k.uid)
//    else
    fctComputed(k, new Cyclic[Fun]({
      cf =>
        funTable += ((k.uid -> cf))
        delegate(k)
    }, Some(_ == _))) //oh_and flushChecks
  }
  def getUnique(k: a.Typ): Cyclic[Typ] = {
////    println(s"cf ${typTable isDefinedAt k.uid}  ${k}")
//    if (typTable isDefinedAt k.uid) typTable(k.uid)
//    else
    new Cyclic[Typ]({
      cf =>
//        println(s"making $k")
        typTable += ((k.uid -> cf))
        delegate(k) //oh_and println(s"made $k")
    }, Some(_ == _)) //oh_and flushChecks
  } //oh_and println(s"$b >> ${k.id} $k")
//  def mkVar(k: a.Local): Local =
//    if (vars isDefinedAt k) vars(k)
//    else apply(k)
  def fctComputed(k: a.Fun, f: Cyclic[Fun]) = f // TODO rm param k
  
  /** Builtins */
  
//  val btyps: Seq[Typ]
}

//case class SingleStaged(s: Stage) {
case class SingleStaged[S <: Stage](s: S) {
  abstract class Identity extends front.StageConverter[s.type,s.type](s, s) {
    
    def typs(x: a.TypSym) = x
    def funs(x: a.FunSym) = x
    def vars(x: a.VarSym) = x
    def terms(x: a.Term) = x
    
    def tspec(x: a.TypeSpec) = x
    def tparam(x: a.TypeParam) = x
    
  }
}

























