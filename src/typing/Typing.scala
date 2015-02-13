package typing

import util._
import common._
import front._
import Stages._
import Reporting._
import Regions._

/**
 * TODO
 *  treat build's args unif correctly
 *  rm cyclic dep in fun args/ret transl
 * 
 */
class Typing(rs: Resolve) extends StageConverter(Resolved, Typed) {
  import b._
  import collection._
  import mutable.ArrayBuffer
  import mutable.HashMap
  
  val btyps = rs.btyps map (t => getUnique(t.value))
  
//  val allTyps = HashMap[Typ, Cyclic[Typ]]()
  
  
  def btByName(nam: Sym) = (btyps.find { _.nam == TId(nam) } get)
  
  val IntType = Type(btByName('Int), Seq(), Seq())
  val RefTyp = btByName('Ref)
  def RefType(typ: Type, reg: Reg) = Type(RefTyp, Seq(typ), Seq(reg))

//  case class FunInfo(argTyps: Seq[AbsTyp])
//  case class FunInfo(argTypes: Seq[Type], retType: Type)
  case class FunInfo(params: Seq[Local], retType: Type)
//  val funInfo = HashMap[Cyclic[Fun],FunInfo]()
  val funInfo = HashMap[a.Fun,FunInfo]()
    
  val bfuns = rs.bfuns map (t => getUnique(t.value))
  
  
  def typs(x: a.TypSym) = getUnique(x.value)
  def funs(x: a.FunSym) = getUnique(x.value)
  def vars(x: a.VarSym) = apply(x)
  
  def tspec(x: a.TypeSpec) = x map apply getOrElse ctx.mkAbsType
  def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp] // TODO make cleaner
  
  def terms(x: a.Term) = {
    def c[T](x: Any) = x.asInstanceOf[T]
    val r = super.apply(x)
    val t = x match {
      case a.NilExpr => ctx.mkAbsType
      case a.IntLit(n) => IntType
      case a.IntOp(a,b,o) =>
        val IntOp(a,b,o) = r
        ctx += (a.typ.valType -> IntType)
        ctx += (b.typ.valType -> IntType)
        IntType
      case a.Var(vs) => ref(apply(vs).typ, vs.nam)
      case a.Block(s,e) => //c[Block](r).ret.typ //terms(e).typ
        val Block(s,e) = r
        e.typ
      case a.Ite(c,t,e) =>
        val Ite(c,t,e) = r
        ctx += (c.typ.valType -> IntType)
//        val tt = t.typ
//        ctx += (tt -> e.typ)
//        tt
        ctx.leastUpperBound(t.typ, e.typ)
      case b: a.Build =>
        b.typ.t.value match {
          case a.ConcTyp(_, _, _, _, fs) if b.args.size != fs.size =>
            throw CompileError(s"Wrong number of arguments in object construction $b")
          case at: a.AbsTyp => 
            throw CompileError(s"Cannot construct abstract type $at")
          case _ =>
        }
        val b2 @ Build(Type(Cyclic(t: ConcTyp), _, _), args) = r
        for (i <- 0 until args.size)
          ctx += (args(i).typ -> b2.transType(t.params(i).typ))
        b2.retType
      case fc: a.FCall =>
        if (fc.args.size != fc.f.value.params.size)
          throw CompileError(s"Wrong number of arguments in function call $fc")
        val tfc: FCall = r.asInstanceOf[FCall]
        tfc.args map (_.typ.valType) zip (tfc.paramTypes(fc.f)) foreach (ctx += _)
        tfc.retType(fc.f)
      case a.FieldAccess(e, id) => // TODO handle refs
        val FieldAccess(e, id) = r
        val typ = e.typ.valType
        typ.t.value match {
          case ct @ ConcTyp(_, _, typs, regs, params) => RefType(typ.fieldType(id), Reg.empty)
          case at : AbsTyp => throw CompileError(s"Unknown field access _.$id on abstract type $at")
        }
  //    case a.FieldAssign(e, id, v) => FieldAssign(terms(e), id, terms(v))
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
//    println(s"Typ: ${t.value}")
//    println(s"Typ: $t")
//    println(t, t.typs.size)
    
//    ctx.delayCheck {
//      if (x.targs.size > t.typs.size) throw CompileError(s"Too many type arguments in $x")
//    }
    
//    else if (x.targs.size < t.typs.size)
    
    val targs = (x.targs map apply) ++ (
      if (t.wasComputerYet)
        for{i <- 0 until (t.typs.size - x.targs.size)} yield ctx.mkAbsType
      else Seq()
    )
    
//    println(s"delaying $targs $t")
    delayCheck {
//      println(targs, t.typs)
      if (targs.size != t.typs.size)
        throw CompileError(s"Wrong number of type arguments in $x of type $t")
    }
    
    Type(t, targs, x.rargs)
  }
  
  override def delegate(x: a.Fun) = {
    pushCtx
//    funInfo(x) = FunInfo(x.params map (_ => ctx.mkAbsType), ctx.mkAbsType)
//    funInfo(x) = FunInfo(x.params map (_ => ctx.mkAbsType), ctx.mkAbsType)
//    val r = super.delegate(x)
    funInfo(x) = FunInfo(x.params map apply, tspec(x.ret))
    val r = Fun(x.uid, x.nam, x.typs map tparam, x.regs, funInfo(x).params, funInfo(x).retType, Specs.Spec.empty, terms(x.body))
    
//    val cstrs = ctx.cstrs
    ctx += (r.ret, r.body.typ)
//    println(ctx.cstrs toMap)
//    new Unify(ctx.cstrs toMap)(r) oh_and {popCtx; flushChecks}
    r
  }
  override def fctComputed(k: a.Fun, x: Cyclic[Fun]) = {
//    println(s"comp ${new Unify(ctx.cstrs toMap).mkUnique(x)}")
//    new Unify(ctx.cstrs toMap).mkUnique(x) oh_and {popCtx; flushChecks}
    new Unify(this, ctx.cstrs toMap).getUnique(x) and {popCtx; flushChecks; funTable(k.uid) = _}
  }
  
////  override def delegate(x: a.Typ) = {
////    super.delegate(x) oh_and flushChecks
////  }
//  override def mkUnique(x: a.Typ) =
//    super.mkUnique(x) oh_and (if (typTable(x).wasComputerYet) flushChecks)
//  
//  override def mkUnique(x: a.Fun) =
//    super.mkUnique(x) oh_and flushChecks
  
  
  case class Ctx (
//      cstrs: ArrayBuffer[(Typ,Typ)],
      cstrs: ArrayBuffer[(AbsTyp,Type)],
      parent: Option[Ctx] )
  {
//    def += (tt: (Type, Type)) {
//      cstrs += (tt._1.t.value -> tt._2.t.value)
//    }
    def += (tt: (Type, Type)) {
//      println(tt)
      tt match { // TODO: handle possible constraint cycles
  //      case (at: AbsTyp, ct: ConcTyp) =>
  //        cstrs += (at.t.value -> ct.t.value)
        case (TType(at:AbsTyp, targs1, rargs1), t2) =>
          assert(targs1.size === 0)
          assert(rargs1.size === 0)
//          if (cstrs isDefinedAt at)
//            cstrs += t2 -> cstrs(at)  
//          val t22 = cstrs.collectFirst{ case(`at`, t) => t }
//          t22 map (t => cstrs += (t2 -> t))
//          cstrs.collect{ case(`at`, t) => +=(t2 -> t) }
          cstrs += (at -> t2)
        
        case (_, TType(_:AbsTyp, _, _)) => +=(tt.swap)
        
        case (TType(t1:ConcTyp, targs1, rargs1), TType(t2:ConcTyp, targs2, rargs2)) =>
//          add(t1,t2)
          if (t1 =/= t2) throw UnificationError(t1,t2)
  //        (targs1 zip targs2) map (+=(_,_))
          (targs1 zip targs2) map += //(+=(_:Type,_:Type))
      }
    }
    
    def leastUpperBound(t1: Type, t2: Type): Type = (t1,t2) match {
      case _ => t1 // TODO!!
    }
    
    
    
//    def add (t1: Typ, t2: Typ): Unit = (t1,t2) match {
//      case _ if t1 === t2 =>
////      case (_:ConcTyp, _:ConcTyp) => println(t1.id,t2.id,t1==t2)
//      case (_:ConcTyp, _:ConcTyp) => throw UnificationError(t1,t2)
//      case (_:ConcTyp, _:AbsTyp) => add(t2,t1)
////      case (t1:AbsTyp, t2:ConcTyp) => cstrs += (t1 -> t2)
//      case (t1:AbsTyp, _) =>
//////        for ((k,v) <- cstrs if v === t1)
//////          cstrs(k) = t1
////        for (i <- 0 until cstrs.size if cstrs(i)._1 == t1)
////          cstrs(i) = cstrs(i)._1 -> t2
//        cstrs += (t1 -> t2)
//    }
    
    
    val absTyps = ArrayBuffer[AbsTyp]()
    def mkAbsType = {
//      Type(new Cyclic(AbsTyp(ctx.nextId, Seq(), Seq(), false) and (absTyps += _)), Seq(), Seq())
      val at = AbsTyp(new TUid, ctx.nextId, Seq(), Seq(), false)
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
  private var ctx = Ctx(ArrayBuffer(), None)
  def pushCtx = ctx = Ctx(ArrayBuffer(), Some(ctx))
  def popCtx = {
//    ctx.delayedChecks foreach (_ apply)
    ctx = ctx.parent.get
  }
  
  object TType {
    def unapply(typ: Type) = Some(typ.t.value, typ.targs, typ.rargs)
  }
  
  
//  def typeUnify(e: a.Expr) = {  // typeContained
//    pushCtx
//    terms(e)
//  } oh_and popCtx
  def typeUnify(e: a.Expr) = {
    val es = Seq()
    val Fun(uid, nam, typs, regs, params, ret, spec, body) =
      apply(a.Fun(new FUid, FId("[internal]"), es, es, es, None, Specs.Spec.empty, e))
    body
  }
  
  
  
  
  implicit class TFCall(self: FCall) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = f.value
    
    def retType(x: a.Fun) = transType(funInfo(x).retType)
//      if (f.wasComputerYet) transType(f.ret)
//      else funInfo(x).retType // ctx.mkAbsType
    
    def paramTypes(x: a.Fun) = funInfo(x).params map (_ typ) map transType
//      if (f.wasComputerYet) f.params map (_ typ) map transType
////      else f.params map (_ => ctx.mkAbsType)
//      else {
////        val af = funTable.collectFirst{case (af,`f`) => af}.get
////        af.params map (_ => ctx.mkAbsType)
//        funInfo(x).argTypes
//      }
////      else funInfo(f).argTyps
    
  }
  implicit class TBuild(self: Build) extends Inst {
    import self._
    
    def targs = typ.targs
    def rargs = typ.rargs
    def parmzd = typ.t.value
    
//    // TODOne: mv this code to 'terms' instead of having it being implicit
//    def retType = {
//      val fromGenArgs = transType(typ)
//      
//      val t = typ.t.value match {
//        case ct: ConcTyp => ct
//        case at: AbsTyp => wtf // should be prevented in 'terms' // throw CompileError("")
//      }
//      
//      for (i <- 0 until args.size)
////        ctx += (args(i).typ -> targs(i))
//        ctx += (args(i).typ -> transType(t.params(i).typ))
//      fromGenArgs
//    }
    def retType = transType(typ)
    
  }
  
  implicit class TType(self: Type) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = t.value
    
    def fieldType(id: VId) = t.value match {
      case t: ConcTyp =>
        t.getField(id) map (l => transType(l.typ)) getOrElse
        (throw CompileError(s"Type $self does not have field $id"))
      case _ => wtf
    }
    
    def valType = self match {
      case Type(RefTyp, Seq(typ), Seq(reg)) =>
        typ
      case _ => self
    }
    
  }
  
  def ref(typ: Type, nam: VId) = typ match {
    case Type(Cyclic(RefTyp), _, _) =>
      typ
    case _ => RefType(typ, Reg(nam))
  }
  
  
  
  case class TypeMismatch(found: Type, req: Type) extends Reporting.CompileError(s"type mismatch: found $found, expected $req")
  
}

















//      case a.Build(t,a) => Typd(super.apply(x), apply(t))
//      case a.FCall(fs,ta,ra,a) => Typd(super.apply(x), funs(fs).ret.get)

