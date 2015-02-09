package typing

import util._
import common._
import front._
import Stages._
import Reporting._
import Regions._

class Typing(rs: Resolve) extends StageConverter(Resolved, Typed) {
  import b._
  import collection._
  import mutable.ArrayBuffer
  import mutable.HashMap
  
//  val btypsc = rs.btyps map mkCycle // apply
//  val btyps = btypsc map (_ value)
//  val btyps = rs.btyps map mkCycle // apply
  val btyps = rs.btyps map (t => getUnique(t.value)) // apply
  
//  val allTyps = HashMap[Typ, Cyclic[Typ]]()
  
  
//  def btByName(nam: Sym) = new Cyclic(btyps.find { _.nam == TId(nam) } get)
  def btByName(nam: Sym) = (btyps.find { _.nam == TId(nam) } get)
  
  val IntType = Type(btByName('Int), Seq(), Seq())
  val RefTyp = btByName('Ref)
  def RefType(typ: Type, reg: Reg) = Type(RefTyp, Seq(typ), Seq(reg))
  
  val bfuns = rs.bfuns map (t => getUnique(t.value))
  
  
  def typs(x: a.TypSym) = getUnique(x.value)
  def funs(x: a.FunSym) = getUnique(x.value)
  def vars(x: a.VarSym) = apply(x)
  
  def terms(x: a.Term) = { // TODO perform translations!
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
      case a.Var(vs) => ref(apply(vs).typ, vs.nam) // TODO MAKE REF
      case a.Block(s,e) => c[Block](r).ret.typ //terms(e).typ
      case a.Ite(c,t,e) =>
//        val ct = terms(c).typ
//        if (ct != IntType) throw TypeMismatch(ct, IntType)
        val Ite(c,t,e) = r
        ctx += (c.typ.valType -> IntType)
//        val tt = t.typ
//        ctx += (tt -> e.typ)
//        tt
        ctx.leastUpperBound(t.typ, e.typ)
//      case a.Build(t,a) => apply(t)
      case b: a.Build =>
//        if (b.args.size != b.typ.t.value.fields.size)
        b.typ.t.value match {
          case a.ConcTyp(_, _, _, fs) if b.args.size != fs.size =>
            throw CompileError(s"Wrong number of arguments in object construction $b")
          case at: a.AbsTyp => 
            throw CompileError(s"Cannot construct abstract type $at")
          case _ =>
        }
//        println(apply(b))
        r.asInstanceOf[Build].retType // apply(t)
//      case a.FCall(fs,ta,ra,a) => funs(fs).value.retType
      case fc: a.FCall =>
        if (fc.args.size != fc.f.value.params.size)
          throw CompileError(s"Wrong number of arguments in function call $fc")
//        r.asInstanceOf[FCall].retType
        val tfc: FCall = r.asInstanceOf[FCall]
        tfc.args map (_.typ.valType) zip (tfc.paramTypes) foreach (ctx += _) // { case(a,b) => a -> b } //{ _ -> _ }
        tfc.retType
      case a.FieldAccess(e, id) => // TODO handle refs
        val FieldAccess(e, id) = r
        val typ = e.typ.valType
//        println(typ, id)
//        println(terms(e).typ.t == RefTyp)
//        terms(e).typ match { case Type(RefTyp,_,_) => }
        typ.t.value match {
          case ct @ ConcTyp(_, typs, regs, params) => RefType(typ.fieldType(id), Reg.empty)
          case at : AbsTyp => throw CompileError(s"Unknown field access _.$id on abstract type $at")
        }
  //    case a.FieldAssign(e, id, v) => FieldAssign(terms(e), id, terms(v))
    }
    Typd(r, t)
  }
  
//  def tspec(x: a.TypeSpec) = wtf // will be dealt for directly
  def tspec(x: a.TypeSpec) = x map apply getOrElse ctx.mkAbsType
  def tparam(x: a.TypeParam) = getUnique(x).value.asInstanceOf[AbsTyp] // TODO make cleaner
  
  
  
  override def apply(x: a.ConcTyp) = x match {
    case ct @ a.ConcTyp(nam, typs, regs, params) =>
      pushCtx
      val ps = params map apply
      val newAbsTyps = ctx.absTyps //.map (_ nam)
//      println(ct, newAbsTyps)
      popCtx
      ConcTyp(nam, (typs map tparam) ++ newAbsTyps, regs, ps)
//    case at: a.AbsTyp => super.apply(x)
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
    val r = super.delegate(x)
//    val cstrs = ctx.cstrs
    ctx += (r.ret, r.body.typ)
//    println(ctx.cstrs toMap)
//    new Unify(ctx.cstrs toMap)(r) oh_and {popCtx; flushChecks}
    r
  }
  override def fctComputed(k: a.Fun, x: Cyclic[Fun]) = {
//    println(s"comp ${new Unify(ctx.cstrs toMap).mkUnique(x)}")
//    new Unify(ctx.cstrs toMap).mkUnique(x) oh_and {popCtx; flushChecks}
    new Unify(this, ctx.cstrs toMap).getUnique(x) and {popCtx; flushChecks; funTable(k) = _}
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
      val at = AbsTyp(ctx.nextId, Seq(), Seq(), false)
      absTyps += at
//      Type(new Cyclic(at) and (ct => allTyps += (at -> ct)), Seq(), Seq())
      Type(new Cyclic(at) and (ct => typTable += (a.AbsTyp(at.nam, Seq(), Seq(), false) -> ct)), Seq(), Seq())
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
    val Fun(nam, typs, regs, params, ret, spec, body) =
      apply(a.Fun(FId("[internal]"), es, es, es, None, Specs.Spec.empty, e))
    body
  }
  
  
  
  
//  def print(t: Type) = {
//    val n = t.t.value match {
//      case ConcTyp(nam, _, _, _) => nam
//      case AbsTyp(nam, _, _) => nam
//    }
//    s"$n[{t.targs mkString ", "}]" t.rargs
//  }
//  def print(t: Typ) = {
//    t match {
//      case ConcTyp(nam, typs, regs, params) =>
//        s"$nam[${typs mkString ", "}]{${regs mkString ", "}}(${params mkString ", "})"
//      case AbsTyp(nam, typs, regs) =>
//        s"$nam[${typs mkString ", "}]{${regs mkString ", "}}=?"
//    }
//  }
  
  
  
//  implicit class TFun(self: Fun) extends Inst {
//    import self._
//    
//    def targs = self.targs
//    def rargs = self.rargs
//    def parmzd = t.value
//    
//    def retType = transType(ret)
//    
//  }
  implicit class TFCall(self: FCall) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = f.value
    
    def retType =
      if (f.wasComputerYet) transType(f.ret)
      else ctx.mkAbsType
    
    def paramTypes =
      if (f.wasComputerYet) f.params map (_ typ) map transType
//      else f.params map (_ => ctx.mkAbsType)
      else {
        val af = funTable.collectFirst{case (af,`f`) => af}.get
        af.params map (_ => ctx.mkAbsType)
      }
    
  }
  implicit class TBuild(self: Build) extends Inst {
    import self._
    
    def targs = typ.targs
    def rargs = typ.rargs
    def parmzd = typ.t.value
    
    def retType = {
      val fromGenArgs = transType(typ)
//      val fromArgs = new Inst {
//        def targs = self.targs
//        def rargs = self.rargs
//        def parmzd = typ.t.value
//      } transType typ
//      self.args map (ctx += (_.typ -> ))
      for (i <- 0 until args.size)
        ctx += (args(i).typ -> targs(i))
      fromGenArgs
    }
    
  }
  
  implicit class TType(self: Type) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = t.value
    
//    def fieldType(id: VId) = t.value match {
//      case t: ConcTyp =>
//        val Type(ft, targs, rargs) = t.getField(id).typ
//        println("> ", t, self.t.typs, t.getField(id).typ)
//        val tta = targs map {
////          case ct: ConcTyp => ct
//          case Type(Cyclic(at: AbsTyp), _, _) if self.t.typs contains at =>
////            val r = self.targs.
//            self.targs(self.t.typs.indexOf(at))
//          case t => t
//        }
//        Type(ft, tta, rargs)
//      case _ => wtf
//    }
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

