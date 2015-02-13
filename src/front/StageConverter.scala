package front

import util._
import common._
import Stages._
import Specs._
import Proxy._
import scala.util.{Try, Success, Failure}
import common.Reporting
import collection.mutable.ArrayBuffer


abstract case class StageConverter[A <: Stage, B <: Stage](a: A, b: B) {
  import collection.mutable.HashMap
  import b._
  
  
  /** Delayed checking useful for avoiding illegal cyclic dependencies */
  
  val delayedChecks = ArrayBuffer[()=>Unit]()
  def delayCheck(ch: => Unit) = delayedChecks += (() => ch)
  def flushChecks {
//    println("flush...")
    val chks = delayedChecks.clone
    delayedChecks.clear
    chks foreach (_ apply)
  }
  
  
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
    case a.Block(s,e) => Block(s map apply, terms(e))
    case a.Ite(c,t,e) => Ite(terms(c),terms(t),terms(e))
    case a.Build(t,a) => Build(apply(t), a map terms)
    case a.FCall(fs,ta,ra,a) => FCall(funs(fs), ta map apply, ra, a map terms)
    case a.FieldAccess(e, id) => FieldAccess(terms(e), id)
    case a.FieldAssign(e, id, v) => FieldAssign(terms(e), id, terms(v))
  }
  
  final def apply(x: a.Fun): Fun =
    funTable getOrElse (x.uid, getUnique(x)) value
  
  def delegate(x: a.Fun): Fun =
    Fun(x.uid, x.nam, x.typs map tparam, x.regs, x.params map apply, tspec(x.ret), Spec.empty, terms(x.body))
  
  def apply(x: a.Type): Type = Type(typs(x.t), x.targs map apply, x.rargs)
  
  final def apply(x: a.Typ): Typ =
    typTable getOrElse (x.uid, getUnique(x)) value
  
  def delegate(x: a.Typ): Typ = x match {
    case t: a.ConcTyp => apply(t)
    case t: a.AbsTyp => apply(t)
    case _ => wtf
  }
  def apply(x: a.ConcTyp): ConcTyp = ConcTyp(x.uid, x.nam, x.typs map tparam, x.regs, x.params map apply)
  def apply(x: a.AbsTyp): AbsTyp = AbsTyp(x.uid, x.nam, x.typs map tparam, x.regs, x.userDefined)
  
  def apply(x: a.Stmt): Stmt = x match {
    case x: a.Expr => apply(x)
    case x: a.Binding => apply(x)
  }
  
  def apply(x: a.Local): Local =
    if (varTable isDefinedAt x.uid) varTable(x.uid)
    else Local(x.uid, x.nam, tspec(x.typ)) and (varTable(x.uid) = _)
  
  def apply(x: a.Binding): Binding = Binding(x.nam, terms(x.value))
  
  
  /** Cycle handling */
  
//  val funs = HashSet[Cyclic[Fun]]()
//  val funs = HashMap[Cyclic[a.Fun], Cyclic[Result[Fun]]]()
  val funTable = HashMap[FUid, Cyclic[Fun]]()
//  val funs = HashMap[a.Fun, Result[Fun]]()
//  val funs = HashMap[a.Fun, Cyclic[Result[Fun]]]()
  val typTable = HashMap[TUid, Cyclic[Typ]]()
  val varTable = HashMap[VUid, Local]()
  
//  def apply(x: Cyclic[a.Fun]): Cyclic[Fun] = mkCycle(x.value)
//  def apply(x: Cyclic[a.Typ]): Cyclic[Typ] = mkCycle(x.value)
  
  def getUnique(k: a.Fun): Cyclic[Fun] = {
//    println(s"cf ${funs isDefinedAt k}  ${k}")
    if (funTable isDefinedAt k.uid) funTable(k.uid)
    else fctComputed(k, new Cyclic[Fun]({
      cf =>
        funTable += ((k.uid -> cf))
        delegate(k)
    })) //oh_and flushChecks
  }
  def getUnique(k: a.Typ): Cyclic[Typ] = {
//    println(s"cf ${typs isDefinedAt k}  ${k}")
    if (typTable isDefinedAt k.uid) typTable(k.uid)
    else new Cyclic[Typ]({
      cf =>
//        println(s"making $k")
        typTable += ((k.uid -> cf))
        delegate(k) //oh_and println(s"made $k")
    }) //oh_and flushChecks
  } //oh_and println(s"$b >> ${k.id} $k")
//  def mkVar(k: a.Local): Local =
//    if (vars isDefinedAt k) vars(k)
//    else apply(k)
  def fctComputed(k: a.Fun, f: Cyclic[Fun]) = f
  
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

























