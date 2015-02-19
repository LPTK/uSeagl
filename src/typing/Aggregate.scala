package typing

import utils._
import common._
import front._
import Stages._
import Reporting._
import Regions._

trait StageIdentDefs {
self: Types.singleStaged.Identity =>
  import b._
  
  override def typs(x: a.TypSym) = apply(x) // getUnique(x.value)
  override def funs(x: a.FunSym) = apply(x) // getUnique(x.value)
  override def vars(x: a.VarSym) = apply(x)
  
  override def tspec(x: a.TypeSpec) = apply(x)
  override def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp] // TODO make cleaner
  
}

/**
 * TODO:
 *  handle explicit fun typ params, eg
 *    fun f[T](x:T) = { f(0); f(()); x }
 *  should not type to
 *    fun f[T](x: T): Ref[T]{x} = {f[Int](0: Int): Ref[T]{x}; f[Unit](new Unit: Unit): Ref[T]{x}; x: Ref[T]{x}}: Ref[T]{x}
 *  "polymorphic recursive function needs explicit complete return type"
 * 
 * 
 * TODO: there should be a bunch of "currently typed decls" that would be the only renewed entities
 *   -- actually, remove this renewing thing
 *   
 *   
 * TODO: allow nested functions (use levels for sound generalization) and treat mutrec funs correctly
 * 
 * 
 */
//class Aggregate(override val state: StageState[Typed.type]) extends Types.singleStaged.Identity {
class Aggregate(val pt: Pretype) extends Types.singleStaged.Identity with StageIdentDefs {
  import b._
  import state._
  import collection._
  import mutable.ArrayBuffer
  import pt._
  
  override val state = pt.state
  
  val IntlFId = FId("[internal]")
  
  // "import" functions so they appear in the :c context command of the REPL before using them
  val bfuns = pt.bfuns map (f => renew(f))
  
//  override def typs(x: a.TypSym) = getUnique(x.value)
//  override def funs(x: a.FunSym) = getUnique(x.value)
//  override def vars(x: a.VarSym) = apply(x)
//  
//  override def tspec(x: a.TypeSpec) = apply(x)
//  override def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp] // TODO make cleaner
  
  override def terms(x: a.Term) = {
    x.obj match {
      case NilExpr => 
      case IntLit(n) => 
      case IntOp(a,b,o) =>
        ctx.soft_cstrs += (a.typ -> IntType) // (a.typ.valType -> IntType)
        ctx.soft_cstrs += (b.typ -> IntType)
        IntType
      case Var(vs) =>
        ctx.soft_cstrs += (x.typ -> vs.typ)
      case Ascribe(e,typ) =>
        ctx.hard_cstrs += (e.typ -> typ)
//        ctx.soft_cstrs += (e.typ -> typ)
      case Block(s,e) => //c[Block](r).ret.typ //terms(e).typ
        s.foreach {
          case Right(Binding(loc, valu)) =>
            ctx.hard_cstrs += (loc.typ -> valu.typ)
          case _ => 
        }
      case Ite(c,t,e) =>
        ctx.soft_cstrs += (c.typ -> IntType)
        ctx.hard_cstrs += (t.typ -> e.typ)
        ctx.hard_cstrs += (t.typ -> x.typ)
//        ctx.leastUpperBound(t.typ, e.typ)
//      case b @ Build(Type(Cyclic(t: ConcTyp), _, _), args) =>
      case b @ Build(TType(t: ConcTyp, _, _), args) =>
        for (i <- 0 until args.size)
          ctx.hard_cstrs += (args(i).typ -> b.transType(t.params(i).typ))
      case b: Build => wtf
      case fc: FCall =>
//        println("FCALL ", x.typ, fc.retType)
//        fc.args map (_.typ.valType) zip (fc.paramTypes) foreach (ctx.soft_cstrs += _)
        
        println(s"Call to ${fc.f.nam}")
        
        implicit val subs = mutable.HashMap[AbsTyp,Type]()
        
        fc.args map (_.typ) zip (fc.paramTypes map inst) foreach (ctx.soft_cstrs += _)
        ctx.hard_cstrs += (x.typ -> inst(fc.retType))
        fc.retType match {
//          case TType(RefTyp, _, Seq(r)) =>
          case TRef(_, r) =>
            ctx.reg_cstrs += (x.reg -> r)
          case _ =>
        }
      case FieldAccess(e, id) => // TODO handle refs
//        val typ = e.typ.valType
//        typ.t.value match {
//          case ct @ ConcTyp(_, _, typs, regs, params) => RefType(typ.fieldType(id), Reg.empty)
//          case at : AbsTyp => throw CompileError(s"Unknown field access _.$id on abstract type $at")
//        }
        ctx.field_cstrs += ((e.typ, id, x.typ))
  //    case a.FieldAssign(e, id, v) => FieldAssign(terms(e), id, terms(v))
    }
    Typd(super.apply(x.obj), x.typ, x.reg)
  }
  
  
  override def apply(x: a.Type) = { // not called
    val r = super.apply(x)
    
//    println(x.targs, x.t.typs)
    
//    delayCheck {
      if (x.targs.size != x.t.typs.size)
        throw CompileError(s"Wrong number of type arguments in $x of type ${x.t}")
    
    r
  }
  
//  override def apply(x: a.Fun) =
//    super.apply(x mkNew) // TODO this is bad; fix it
//  def renew(x: Fun) = { funTable -= x.uid; apply(x) }
//  def renew(x: Typ) = { typTable -= x.uid; apply(x) }
  def renew(x: Fun) = renewTree{ apply(x) }
  def renew(x: Typ) = renewTree{ apply(x) }
  
  override def delegate(x: a.Fun) = {
    pushCtx
    val r = super.delegate(x)
    ctx.hard_cstrs += (r.ret -> r.body.typ)
    r
//    new Unify2(this).apply(x) oh_and popCtx
  }
//  override def fctComputed(k: a.Fun, x: Cyclic[Fun]) = {
////    println(s"comp ${new Unify(ctx.cstrs toMap).mkUnique(x)}")
////    new Unify(ctx.cstrs toMap).mkUnique(x) oh_and {popCtx; flushChecks}
//    new Unify(state, ctx.cstrs toMap).getUnique(x) and {popCtx; flushChecks; funTable(k.uid) = _}
//  }
  override def fctComputed(k: Fun, x: Cyclic[Fun]) = {
//    new Unify2(this).getUnique(x) oh_and popCtx
    println(x)
//    new Unify(this)(x) oh_and popCtx
//    val r = try new Unify(this)(x) finally popCtx
    val u = try new Unify(this) finally popCtx
    val r = u(x)
    
    def hasRegs(typ: Type): Bool =
      (typ.t.regs.size != 0) || (typ.targs exists hasRegs)
    
    def hasAbsTypWithRegs(typ: Type): Bool = typ match {
      case TType(at: AbsTyp, _, _) if u.subs isDefinedAt at => hasRegs(u.subs(at)) // TODO assert no targs/rargs
      case typ @ TType(ct: ConcTyp, targs, rargs) =>
//        hasRegs(typ) || (targs exists hasAbsTypWithRegs)
        (targs exists hasAbsTypWithRegs)
      case _ => false
    }
    
    for ((Local(_,_,t1), l2 @ Local(_,_,t2)) <- x.params zip r.params) {
//    }
//    for (Local(_,_,t1) <- x.params) {
      if (hasAbsTypWithRegs(t1))
        throw CompileError(s"Inferred parameter $l2 has unspecified region arguments, in: $r")
    }
    
//    u(x)
    r.value.params map (_ typ) foreach gen
    gen(r.value.ret) // TODO: use levels to allow sound nested functions gen; TODO rm mutation
    r
  }
  
  
  
//  def inst(f: Fun) = 
//  def inst(s: Sign) = 
//  def inst(t: Type)(implicit subs: mutable.Map[AbsTyp, Type] = mutable.HashMap()): Type = t match {
  def inst(t: Type)(implicit subs: mutable.Map[AbsTyp, Type]): Type = t match {
    case TType(at: AbsTyp, _, _) if subs isDefinedAt at => subs(at)
    case TType(at: AbsTyp, _, _) if at.quantified =>
      Type(new Cyclic(AbsTyp()), Seq(), Seq()) and (subs += at -> _) and (x => println(s"inst $at -> $x"))
    case Type(c@Cyclic(ct: ConcTyp), targs, rargs) =>
      Type(c, targs map inst, rargs)
    case _ => t
  }
  
  def gen(t: Type): Unit = t match {
    case TType(at: AbsTyp, _, _) if !at.userDefined => at.quantified = true
    case Type(c@Cyclic(ct: ConcTyp), targs, rargs) =>
      targs foreach gen
    case _ =>
  }
  
  
  
  
  
  case class Ctx (
      parent: Option[Ctx],
      hard_cstrs: ArrayBuffer[(Type,Type)] = ArrayBuffer(),
      soft_cstrs: ArrayBuffer[(Type,Type)] = ArrayBuffer(), // only the lhs is "soft" (references will be coerced)
      field_cstrs: ArrayBuffer[(Type,VId,Type)] = ArrayBuffer(),
//      reg_cstrs: ArrayBuffer[(VId,Reg)] = ArrayBuffer()
      reg_cstrs: ArrayBuffer[(Reg,Reg)] = ArrayBuffer()
  ) {
    
  }
  private var ctx = Ctx(None)
  def pushCtx = ctx = Ctx(Some(ctx))
  def popCtx = {
//    ctx.delayedChecks foreach (_ apply)
    ctx = ctx.parent.get
  }
  
  def cstrs = ctx
  
  
  
  
  def typeUnify(e: pt.a.Expr) = {
    val es = Seq()
    val Cyclic(Fun(uid, nam, typs, regs, params, ret, spec, body)) =
      renew(pt.apply(pt.a.Fun(new FUid, IntlFId, es, es, es, None, Specs.Spec.empty, e)))
    body
  }
  def typeUnify(b: pt.a.Binding) = {
    val es = Seq()
    val Cyclic(Fun(uid, nam, typs, regs, params, ret, spec, body)) =
      renew(pt.apply(pt.a.Fun(new FUid, IntlFId, es, es, es, b.loc.typ, Specs.Spec.empty, b.value)))
    state.varTable += (b.loc.uid -> Local(b.loc.uid, b.loc.nam, ret)) // we need to leak the typed result; (pretty ugly) 
    body
  }
  
  
  
  
  
  implicit class TFCall(self: FCall) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def args = self.args
    
    def parmzd = f.value
    
    def reg(t: Term) = t.reg
    
    def retType = transType(f.ret)
    
    def paramTypes = f.params map (_ typ) map transType
    
  }
  
  implicit class TBuild(self: Build) extends Inst {
    import self._
    
    def targs = typ.targs
    def rargs = typ.rargs
    def args = self.args // TODO: is this correct? does Build have inferred args?
    
    def parmzd = typ.t.value
    
    def reg(t: Term) = t.reg
    
  }
  
  // not used (yet?):
  case class TypeMismatch(found: Type, req: Type) extends Reporting.CompileError(s"type mismatch: found $found, expected $req")
  
}















