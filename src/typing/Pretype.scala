package typing

import utils._
import common._
import front._
import Stages._
import Reporting._
import Regions._

/**
 * TODO:
 *   before typing, put in flat form; easier to deal with tempos' regions
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
    val t = x match {
      case a.NilExpr => ctx.mkAbsType
      case a.IntLit(n) => IntType
      case a.IntOp(a,b,o) => IntType
      case a.Var(vs) => //ctx.mkAbsType //ref(apply(vs).typ, vs.nam)
//        ref(ctx.mkAbsType, vs.nam)
        ref(r.asInstanceOf[Var].sym.typ, vs.nam)
      case a.Block(s,e) => //c[Block](r).ret.typ //terms(e).typ
        val Block(s,e) = r
        e.typ
      case a.Ite(c,t,e) => ctx.mkAbsType
      case b: a.Build =>
        b.typ.t.value match {
          case a.ConcTyp(_, _, _, _, fs) if b.args.size != fs.size =>
            throw CompileError(s"Wrong number of arguments in object construction $b")
          case at: a.AbsTyp => 
            throw CompileError(s"Cannot construct abstract type $at")
          case _ =>
        }
        r.asInstanceOf[Build].typ
//      case fc: a.FCall =>
      case fc: a.FCall =>
//      case fc @ a.FCall(f, targs, rargs, args) =>
        if (fc.args.size != fc.f.value.params.size)
//        if (args.size != f.value.params.size)
          throw CompileError(s"Wrong number of arguments in function call $fc")
        val FCall(f, targs, rargs, args) = r
        if (targs.size > fc.f.typs.size)
          throw CompileError(s"Too many type arguments in call $fc")
        r = FCall(f, targs ++ (for (i <- targs.size until fc.f.typs.size) yield ctx.mkAbsType), rargs, args)
        if (!f.wasComputerYet && fc.f.value.typs.size > 0 && !fc.f.value.ret.isComplete)
//          throw CompileError(s"Recursive-polymorphic function needs complete return type specified: ${fc.f.nam}")
          throw CompileError(s"Function '${fc.f.nam}' with polymorphic recursion (explicit type parameters) needs return type completely specified")
        ctx.mkAbsType // can't know in advance the type of the return; add cstr later
      case a.FieldAccess(e, id) => // TODO handle refs
//        val FieldAccess(e, id) = r
//        val typ = e.typ.valType
//        typ.t.value match {
//          case ct @ ConcTyp(_, _, typs, regs, params) => RefType(typ.fieldType(id), Reg.empty)
//          case at : AbsTyp => throw CompileError(s"Unknown field access _.$id on abstract type $at")
//        }
        ref(ctx.mkAbsType, VId("??")) // TODO handle region
      case a.FieldAssign(e, id, v) => UnitType
    }
    Typd(r, t)
  }
  
  
  override def apply(x: a.ConcTyp) = x match {
    case a.ConcTyp(uid, nam, typs, regs, params) =>
      pushCtx
      val ps = params map apply
      val newAbsTyps = ctx.absTyps
      popCtx
      ConcTyp(uid, nam, (typs map tparam) ++ newAbsTyps, regs, ps)
  }
  
  override def apply(x: a.Type) = {
    val t = typs(x.t)
    
    val targs = (x.targs map apply) ++ {
      if (t.wasComputerYet)
        for{i <- 0 until (t.typs.size - x.targs.size)} yield ctx.mkAbsType
      else Seq()
    }
    
    // TODOne check will be done in next phase!:
//    delayCheck {
//      if (targs.size != t.typs.size)
//        throw CompileError(s"Wrong number of type arguments in $x of type $t")
//    }
    
    Type(t, targs, x.rargs)
  }
  
  override def delegate(x: a.Fun) = {
    pushCtx
    super.delegate(x)
  }
  override def fctComputed(k: a.Fun, x: Cyclic[Fun]) = { // TODO rm x param
    x oh_and { popCtx }
  }
  
  
  case class Ctx (parent: Option[Ctx]) {
    val absTyps = ArrayBuffer[AbsTyp]()
    def mkAbsType = {
//      Type(new Cyclic(AbsTyp(ctx.nextId, Seq(), Seq(), false) and (absTyps += _)), Seq(), Seq())
      val at = AbsTyp(new TUid, ctx.nextId, Seq(), Seq(), false)
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
      TId(s"'$letter" + (if (absTypId > 26) (absTypId/26) else ""))
    } oh_and (absTypId += 1)
  }
  private var ctx = Ctx(None)
  def pushCtx { ctx = Ctx(Some(ctx)) }
  def popCtx  { ctx = ctx.parent.get }
  
  
  object TType {
    def unapply(typ: Type) = Some(typ.t.value, typ.targs, typ.rargs)
  }
  implicit class TType(self: Type) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = t.value
    
    def valType = self match {
      case Type(RefTyp, Seq(typ), Seq(reg)) =>
        typ
      case _ => self
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
    
  }
  
  def ref(typ: Type, nam: VId) = typ match {
    case Type(Cyclic(RefTyp), _, _) =>
      typ
    case _ => RefType(typ, Reg(nam))
  }
  
  
  
}


















