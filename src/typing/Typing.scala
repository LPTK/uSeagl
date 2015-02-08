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
  
  val btypsc = rs.btyps map mkCycle // apply
  val btyps = btypsc map (_ value)
  
//  def btByName(nam: Sym) = new Cyclic(btyps.find { _.nam == TId(nam) } get)
  def btByName(nam: Sym) = (btypsc.find { _.nam == TId(nam) } get)
  
  val IntType = Type(btByName('Int), Seq(), Seq())
  val RefTyp = btByName('Ref)
  def RefType(typ: Type, reg: Reg) = Type(RefTyp, Seq(typ), Seq(reg))
  
  
  def typs(x: a.TypSym) = mkCycle(x.value)
  def funs(x: a.FunSym) = mkCycle(x.value)
  def vars(x: a.VarSym) = apply(x)
  
  def terms(x: a.Term) = { // TODO perform translations!
    val r = super.apply(x)
    val t = x match {
      case a.NilExpr => ctx.mkAbsType
      case a.IntLit(n) => IntType
      case a.Var(vs) => ref(apply(vs).typ, vs.nam) // TODO MAKE REF
      case a.Block(s,e) => terms(e).typ
      case a.Ite(c,t,e) =>
//        val ct = terms(c).typ
//        if (ct != IntType) throw TypeMismatch(ct, IntType)
        ctx += (terms(c).typ -> IntType)
        val tt = terms(t).typ
        ctx += (tt -> terms(e).typ)
        tt
//      case a.Build(t,a) => apply(t)
      case b: a.Build =>
        apply(b).asInstanceOf[Build].retType // apply(t)
//      case a.FCall(fs,ta,ra,a) => funs(fs).value.retType
      case fc: a.FCall => apply(fc).asInstanceOf[FCall].retType
      case a.FieldAccess(e, id) => // TODO handle refs
        val typ = terms(e).typ.valType
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
  def tparam(x: a.TypeParam) = mkCycle(x).value.asInstanceOf[AbsTyp] // TODO make cleaner
  
  
  
  override def apply(x: a.ConcTyp) = x match {
    case ct @ a.ConcTyp(nam, typs, regs, params) =>
      pushCtx
      val ps = params map apply
      val newAbsTyps = ctx.absTyps //.map (_ nam)
      popCtx
      ConcTyp(nam, (typs map tparam) ++ newAbsTyps, regs, ps)
//    case at: a.AbsTyp => super.apply(x)
  }
  
  override def apply(x: a.Type) = {
    val t = typs(x.t)
//    println(s"Typ: ${t.value}")
//    println(s"Typ: $t")
    if (x.targs.size > t.typs.size) throw CompileError(s"Too many type arguments in $x")
//    else if (x.targs.size < t.typs.size)
    val targs = (x.targs map apply) ++ (
        for{i <- 0 until (t.typs.size - x.targs.size)} yield ctx.mkAbsType
    )
    Type(t, targs, x.rargs)
  }
  
  
  
  
  
  
  
  case class Ctx (
      cstrs: ArrayBuffer[(Typ,Typ)],
      parent: Option[Ctx] )
  {
    def += (tt: (Type, Type)) {
      cstrs += (tt._1.t.value -> tt._2.t.value)
    }
    val absTyps = ArrayBuffer[AbsTyp]()
    def mkAbsType =
      Type(new Cyclic(AbsTyp(ctx.nextId, Seq(), Seq()) and (absTyps += _)), Seq(), Seq())
    
    var absTypId = 0
    def nextId = {
      
      val letter = (absTypId % 26 + 'A').toChar
      
      TId(s"'$letter" + (if (absTypId > 26) (absTypId/26) else ""))
      
    } oh_and (absTypId += 1)
  }
  private var ctx = Ctx(ArrayBuffer(), None)
  def pushCtx = ctx = Ctx(ArrayBuffer(), Some(ctx))
  def popCtx = ctx = ctx.parent.get
  
  
  
  
  
  def typeUnify(e: a.Expr) = {  // typeContained
    pushCtx
    terms(e)
  } oh_and popCtx
  
  
  
  
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
    
    def retType = transType(f.ret)
    
  }
  implicit class TBuild(self: Build) extends Inst {
    import self._
    
    def targs = self.targs
    def rargs = self.rargs
    def parmzd = typ.t.value
    
    def retType = {
      val fromGenArgs = transType(typ)
//      val fromArgs = new Inst {
//        def targs = self.targs
//        def rargs = self.rargs
//        def parmzd = typ.t.value
//      } transType typ
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

