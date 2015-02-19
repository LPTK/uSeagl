package typing

import utils._
import common._
import front._
import Stages._
import Reporting._
import Regions._

/**
 * TODO:
 *  - before typing, put in flat form; easier to deal with tempos' regions
 *  - prevent region name clashes (args, locs, etc)
 *  
 */
class Pretype(rs: Resolve) extends StageConverter(Resolved, Typed) {
  import b._
  import state._
  import collection._
  import mutable.ArrayBuffer
  import mutable.HashMap
  
  
//  val btyps = rs.btyps map (t => getUnique(t.value))
  val btyps = rs.btyps map (t => apply(t:a.Typ))
  
  def btByName(nam: Sym) = (btyps.find { _.nam == TId(nam) } get)
  
  val UnitType = Type(btByName('Unit), Seq(), Seq())
  val IntType = Type(btByName('Int), Seq(), Seq())
  val RefTyp = btByName('Ref)
  def RefType(typ: Type, reg: Reg) = Type(RefTyp, Seq(typ), Seq(reg))
  
  val bfuns = rs.bfuns map (t => apply(t))
  
//  case class FunInfo(argTyps: Seq[AbsTyp])
//  case class FunInfo(argTypes: Seq[Type], retType: Type)
  case class FunInfo(params: Seq[Local], retType: Type)
//  val funInfo = HashMap[Cyclic[Fun],FunInfo]()
  val funInfo = HashMap[a.Fun,FunInfo]()
  
  
  def typs(x: a.TypSym) = apply(x value) // getUnique(x.value)
  def funs(x: a.FunSym) = apply(x value) // getUnique(x.value)
  def vars(x: a.VarSym) = apply(x)
  
  def tspec(x: a.TypeSpec) = x map apply getOrElse ctx.mkAbsType
  def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp]
  
  def terms(x: a.Term) = {
    var r = super.apply(x)
    val (t, reg) = x match {
//    val (t: Type, reg: Reg) = x match {
      case a.NilExpr => (ctx.mkAbsType, Reg.empty)
      case a.IntLit(n) => (IntType, ctx.mkTmpReg)
      case a.IntOp(a,b,o) => (IntType, ctx.mkTmpReg)
      case a.Ascribe(e,typ) =>
        val Ascribe(e,typ) = r
        (typ, e.reg)
      case a.Var(vs) => //ctx.mkAbsType //ref(apply(vs).typ, vs.nam)
//        ref(ctx.mkAbsType, vs.nam)
        ref(r.asInstanceOf[Var].sym.typ, vs.nam)
      case a.Block(s,e) => //c[Block](r).ret.typ //terms(e).typ
        val Block(s,e) = r
        // TODO check no regions escape!
        (e.typ, e.reg)
      case a.Ite(c,t,e) => (ctx.mkAbsType, ctx.mkTmpReg)
      case b: a.Build =>
        b.typ.t.value match {
          case a.ConcTyp(_, _, _, _, fs, _) if b.args.size != fs.size =>
            throw CompileError(s"Wrong number of arguments in object construction $b")
          case at: a.AbsTyp => 
            throw CompileError(s"Cannot construct abstract type $at")
          case _ =>
        }
        (r.asInstanceOf[Build].typ, ctx.mkTmpReg)
//      case fc: a.FCall =>
      case fc: a.FCall =>
//      case fc @ a.FCall(f, targs, rargs, args) =>
        if (fc.args.size != fc.f.value.params.size)
//        if (args.size != f.value.params.size)
          throw CompileError(s"Wrong number of arguments in function call $fc")
        val FCall(f, targs, rargs, args) = r
        if (targs.size > fc.f.typs.size)
          throw CompileError(s"Too many type arguments in call $fc")
        val fc2 = FCall(f, targs ++ (for (i <- targs.size until fc.f.typs.size) yield ctx.mkAbsType), rargs, args)
        r = fc2
        if (!f.wasComputerYet && fc.f.value.typs.size > 0 && !fc.f.value.ret.isComplete)
//          throw CompileError(s"Recursive-polymorphic function needs complete return type specified: ${fc.f.nam}")
          throw CompileError(s"Function '${fc.f.nam}' with polymorphic recursion (explicit type parameters) needs return type completely specified")
        if (!f.wasComputerYet) ctx.recCalls += fc2
        (ctx.mkAbsType, ctx.mkAbsReg) // can't know in advance the type of the return; add cstr later
      case a.FieldAccess(e, id) => // TODO handle refs
//        val FieldAccess(e, id) = r
//        val typ = e.typ.valType
//        typ.t.value match {
//          case ct @ ConcTyp(_, _, typs, regs, params) => RefType(typ.fieldType(id), Reg.empty)
//          case at : AbsTyp => throw CompileError(s"Unknown field access _.$id on abstract type $at")
//        }
        val FieldAccess(e, id) = r
        ref(ctx.mkAbsType, e.reg ~ Reg(id)) // TODO handle region
      case a.FieldAssign(e, id, v) => (UnitType, ctx.mkTmpReg)
    }
    Typd(r.desugar, t, reg)
  }
  
  /** this override reduces the number of abs types to unify by inlining the type of the value in an untyped binding */
  override def apply(x: a.Binding): Binding =
  if (x.loc.typ.isDefined) super.apply(x) else {
    val value = terms(x.value)
    val loc = Local(x.loc.uid, x.loc.nam, value.typ)
    state.varTable += (loc.uid -> loc)
    Binding(loc, value)
  }
  
  override def apply(x: a.ConcTyp) = x match {
    case a.ConcTyp(uid, nam, typs, regs, params, prim) =>
      pushCtx
      val ps = params map apply
      val newAbsTyps = ctx.absTyps
      newAbsTyps foreach (_.quantified = true)
      val newAbsRegs = ctx.absRegs
      popCtx
      ConcTyp(uid, nam, (typs map tparam) ++ newAbsTyps, regs ++ newAbsRegs, ps, prim)
  }
  
  override def apply(x: a.Type) = {
    val t = typs(x.t)
    
    val targs = (x.targs map apply) ++ {
      if (t.wasComputerYet)
        for{i <- 0 until (t.typs.size - x.targs.size)} yield ctx.mkAbsType
      else Seq()
    }
    
    val rargs = (x.rargs map apply) ++ {
      if (t.wasComputerYet)
        for{i <- 0 until (t.regs.size - x.rargs.size)} yield ctx.mkAbsReg
      else Seq()
    }
    
    // TODOne check will be done in next phase!:
//    delayCheck {
//      if (targs.size != t.typs.size)
//        throw CompileError(s"Wrong number of type arguments in $x of type $t")
//    }
    
    Type(t, targs, rargs)
  }
  
  override def delegate(x: a.Fun) = {
    pushCtx
    try super.delegate(x) catch { case t: Throwable => popCtx; throw t }
  }
  override def fctComputed(k: a.Fun, x: Cyclic[Fun]) = try { // TODO rm k param
//    x oh_and { popCtx }
//    if (x.value.params.)
//    if (!(x.params map (_ typ) forall (_ isRegComplete)))
    def regCompl = k.params map (_.typ.isRegComplete) forall (identity _)
    if (!regCompl) // other part of the check in Aggregate's fctComputed
      throw CompileError(s"Incomplete region arguments specification in parameters of $x")
    x
  } finally popCtx
  
  
  case class Ctx (parent: Option[Ctx]) {
    val absTyps = ArrayBuffer[AbsTyp]()
    val recCalls = ArrayBuffer[FCall]()
    def mkAbsType = {
//      Type(new Cyclic(AbsTyp(ctx.nextId, Seq(), Seq(), false) and (absTyps += _)), Seq(), Seq())
      val at = AbsTyp(new TUid, ctx.nextId, Seq(), Seq(), false, false)
//      println(s"mk $at")
//      if (at.uid.id == 74) ???
      absTyps += at
//      Type(new Cyclic(at) and (ct => allTyps += (at -> ct)), Seq(), Seq())
      Type(new Cyclic(at) and (ct => typTable += (at.uid -> ct)), Seq(), Seq())
      // ^ that's really ugly
    }
    
    var absTypId = 0
    def nextId = {
      val letter = (absTypId % 26 + 'A').toChar
      TId(s"$letter" + (if (absTypId > 26) (absTypId/26) else ""))
    } oh_and (absTypId += 1)
    
//    var tmpVarId = 0
//    def nextTmpId = {
//      val letter = (tmpVarId % 26 + 'a').toChar
//      VId(s"tmp$$$letter" + (if (tmpVarId > 26) (tmpVarId/26) else ""))
//    } oh_and (tmpVarId += 1)
//    
//    var absRegId = 0
//    def mkAbsReg = Reg(VId("r"+absRegId)) oh_and (absRegId += 1)
    
    var tmpRegId = 0
    def mkTmpReg = Reg(VId("$tmp"+tmpRegId)) oh_and (tmpRegId += 1)
    
    var absVarId = 0
    def nextVarId = {
      val letter = (absVarId % 26 + 'a').toChar
      VId(s"'$letter" + (if (absVarId > 26) (absVarId/26) else ""))
    } oh_and (absVarId += 1)
    
//    val absRegs = ArrayBuffer[Reg]()
//    def mkAbsReg = Reg(nextVarId) and (absRegs += _)
    val absRegs = ArrayBuffer[VId]()
    def mkAbsReg = Reg(nextVarId and (absRegs += _))
    
  }
  private var ctx = Ctx(None)
  def pushCtx { ctx = Ctx(Some(ctx)) }
  def popCtx  { ctx = ctx.parent.get }
  
  
  
  
  
  
  
  implicit class TType(self: Type) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def args = Seq() // note: can cause problems; cf. ConcTyp actually has args
    // self.args
    
    def parmzd = t.value
    
    def reg(t: Term) = t.reg
    
    def valType = self match {
      case Type(RefTyp, Seq(typ), Seq(reg)) =>
        typ
      case _ => self
    }
    
  }
  
  object TType {
    def unapply(typ: Type) = Some(typ.t.value, typ.targs, typ.rargs)
  }
  
  object TRef {
    def unapply(typ: Type) = typ match {
      case TType(RefTyp, Seq(t), Seq(r)) => Some(t,r)
      case _ => None
    }
  }
  
  
  
  implicit class TaType(self: a.TypeSpec) {
    import self._
    
    // TODO also require full region args?
    def isComplete = {
      def ok(typ: a.Type): Bool =
        typ.targs.size === typ.t.typs.size && (typ.targs forall ok)
      self match {
        case None => false
  //      case Some(a.Type(Cyclic(t), targs, rargs)) =>
  //        targs forall (_ isComplete)
        case Some(typ) =>
//          println(typ,typ.targs.size, typ.t.typs.size, (typ.targs forall ok))
          ok(typ)
      }
    }
    
    def isRegComplete = {
      def ok(typ: a.Type): Bool =
        typ.rargs.size === typ.t.regs.size && (typ.targs forall ok)
      self match {
        case None => true
        case Some(typ) => ok(typ)
      }
    }
    
  }
  
  def ref(typ: Type, reg: Reg): (Type, Reg) = typ match {
//    case Type(Cyclic(RefTyp), _, Seq(r)) =>
//      (typ, r)
//    case TRef(typ, r) => (typ, r)
    case typ @ TType(at:AbsTyp,_,_) =>
//      (RefType(ctx.mkAbsType, reg), reg) // TODO: reg here?
      val r = ctx.mkAbsReg
      (RefType(ctx.mkAbsType, r), r)
    case _ if typ.t.primitive =>
//      println(">>",typ)
      (typ, reg) //Reg(VId("??"))) // note: should not be 'r', which is the place the HOR was stored, which we don't care about
    case _ => (RefType(typ, reg), reg)
  }  
  def ref(typ: Type, nam: VId): (Type, Reg) = ref(typ, Reg(nam))
  
  
  
  
}


















