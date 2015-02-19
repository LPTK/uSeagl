package typing

import utils._
import common._
import front._
import Stages._
import Reporting._
import Regions._
import Typed._
//import Unify._
import collection._


/**
 * TODO
 * - coalesce Ref[Ref] into Ref and  >> Ref[Int] into Int <<
 * 
 * - find a way to make region unif work, eg:
 *     fun f(a,b) = if nil then a else b
 * 
 * 
 * Notes
 * - can't currently call functions with ref params, even with a ref argument
 *   reason is we coalesce ref args without first seeing if they fit, which would entail a form of backtracking
 * 
 * 
 */
class Unify(val ag: Aggregate) extends Types.singleStaged.Identity with StageIdentDefs {
  import ag.pt._
  
  def debug(x: Any) {
    println(x)
  }
  
  override val state = ag.state
  
//  sealed trait QualType { def map(f: Type => Type): QualType }
//  case class Strong(typ: Type) extends QualType { def map(f: Type=>Type) = Strong(f(typ)) }
//  case class Weak(typ: Type) extends QualType { def map(f: Type=>Type) = Weak(f(typ)) }
//  case class Field(typ: Type, id: VId) extends QualType { def map(f: Type=>Type) = Field(f(typ), id) }
  
  val subs = mutable.Map[AbsTyp, Type]()
  val regSubs = mutable.Map[Sym, Reg]()
  
//  println(ag.cstrs)
//  println(s"${ag.cstrs.hard_cstrs mkString ", "} - ${ag.cstrs.soft_cstrs mkString ", "} - ${ag.cstrs.field_cstrs mkString ", "}")
  
  val hc = ag.cstrs.hard_cstrs.clone
  val sc = ag.cstrs.soft_cstrs.clone
  val fc = ag.cstrs.field_cstrs.clone
  
  val rc = ag.cstrs.reg_cstrs.clone
  
  def printCstrs = {
//    def p[T](t: Traversable[T]) = 
    debug(s"${
      hc map { case(a,b) => s"$a = $b" 
    } mkString " , "}  -  ${
      sc map { case(a,b) => s"$a ~ $b" 
    } mkString " , "}  -  ${
      fc map { case(a,f,b) => s"$a.$f = $b" 
    } mkString " , "}")
  }
//  hc map {
//    case (TType(at: AbsTyp, targs, rargs), t2) =>
//      assert(targs == Seq() && rargs == Seq(), "no support for parametrized abs types")
//      subs(at) = Strong(t2)  // Strong(apply(t2))
////      for ((t1,t2) <- hc)
//      for (i <- hc.indices) hc(i) = (apply(hc(i)._1), apply(hc(i)._2))
//      println(hc.mkString(", ") + "  " + subs)
//  }
  
//  fc foreach {
//    case (t1,id,t2) =>
////      hc += (t2 -> Field(t1,id))
//      subs(t2) = Field(t1,id)
//  }
  
  printCstrs
  
  private var bindedElem = None: Opt[(AbsTyp,Type)]
  
  /** second param t is just for printing an error */
  def noCycle[T](at: AbsTyp, t: Type)(f: => T) =
    try { bindedElem = Some(at,t); f } finally bindedElem = None
  
//  while (!hc.isEmpty || !sc.isEmpty || !fc.isEmpty) {
  while (!hc.isEmpty || !sc.isEmpty) {
//    val (t1,t2)
    while (!hc.isEmpty) hc.remove(hc.size-1) match {
      
//      case (TType(at: AbsTyp, Seq(), Seq()), t2) if subs isDefinedAt at =>
//        hc += (subs(at) -> t2)
        
//      case (TType(at: AbsTyp, targs, rargs), t2) =>
//        assert(targs == Seq() && rargs == Seq(), "no support for parametrized abs types")
      case (TType(at: AbsTyp, Seq(), Seq()), t2) => subs.get(at) match {
//        case None if at =/= t2.t.value =>
        case None if t2.t.value =/= at =>
          subs(at) = t2  // Strong(apply(t2))
//          for (i <- hc.indices) hc(i) = (apply(hc(i)._1), apply(hc(i)._2))
          subs.transform{
            // trivial cycles (A->A) can have been introduced by previous transformations:
            case(at,qt) if qt.t.value =/= at => noCycle(at,qt){ apply(qt) }
            case(at,qt) => qt
          }
          debug(hc.mkString(", ") + "  " + subs)
        case None =>
        case Some(t) =>
//          hc += (apply(t) -> apply(t2))
          val b @ (x,y) = (apply(t) -> apply(t2))
          if (x =/= y) hc += b
//          println(t,t2,b)
//          if (x =/= y && y =/= t) hc += b
//          println(hc)
//        case Some(_) => ???
      }
      
      case (TType(_: AbsTyp, _, _), _) => wth("no support for parametrized abs types")
      
      case (t1, t2 @ TType(_: AbsTyp, _, _)) => hc += (t2 -> t1)
      
      case (tt1 @ TType(t1:ConcTyp, targs1, rargs1), tt2 @ TType(t2:ConcTyp, targs2, rargs2)) =>
//        if (t1 =/= t2) throw UnificationError(t1,t2)
//        if (t1 =/= t2) (t1,t2) match {
//          case (RefTyp,t2) =>
//        }
        (t1,t2) match {
          case _ if t1 === t2 =>
            (targs1 zip targs2) map hc.+=
            (rargs1 zip rargs2) map rc.+=
          case (RefTyp,t) if t.primitive =>
          case (t,RefTyp) if t.primitive =>
          case _ =>
            throw UnificationError(t1,t2)
        }
    }
    
    sc.transform{ case(t1,t2) => (apply(t1),apply(t2)) }
    fc.transform{ case(t1,id,t2) => (apply(t1),id,apply(t2)) }
    
    printCstrs
    
    if (!sc.isEmpty) sc.remove(sc.size-1) match {
      case (TType(RefTyp, Seq(t1), Seq(r1)), TType(RefTyp, Seq(t2), Seq(r2))) =>
        rc += (r1 -> r2)
        sc += (t1 -> t2)
      case (TType(RefTyp, Seq(t1), _), t2) => hc += (t1 -> t2)
//      case (t1, TType(RefTyp, Seq(t2), _)) => hc += (t1 -> t2)
      case (t1, t2) =>
        /** Note: maybe we should first send all coalescable refs before sending those; we might find out new ones in the process */
        hc += (t1 -> t2)
//        println(t1,t2,RefTyp,t1.t == RefTyp)
//        println(t1.t,RefTyp,t1.t.value == RefTyp.value)
    }
    
//    for (i <- fc.indices) fc(i) match {
    var i = 0
    while (i < fc.size) (fc(i) match {
      case (TType(RefTyp, Seq(vt), _), f, tf) =>
        fc(i) = (vt,f,tf)
        i -= 1
      case (tt @ TType(ct:ConcTyp, _, _), f, tf) =>
        fc.remove(i)
        i -= 1
//        if (ct.params.find { x => ??? })
        ct.getField(f) match {
          case Some(loc) =>
//            hc += 
//            sc += (tt.transType(loc.typ) -> tf)
            sc += (tf -> tt.transType(loc.typ))
          case None =>
            throw CompileError(s"Type ${ct} has no field $f")
        }
      case _ =>
    }) oh_and (i += 1)
    
  }
  
  fc.headOption map { case(t,f,tf) => throw CompileError(s"unknown field access _.$f on abstract type $t") }
  
  debug(subs)
  
  def printRc = debug(s"rc: ${rc map {case(a,b) => s"$a = $b"} mkString ","}")
  printRc
  
//  def unifRegs(s: Sym, r: Reg) {
//  }
  for (regs <- rc) regs match {
    case (AbsReg(ar), r) => regSubs += (ar -> r) //unifRegs(ar,r)
    case (r, AbsReg(ar)) => regSubs += (ar -> r)
    case (r1, r2) =>
      if (r1 =/= r2) throw CompileError(s"Cannot unify regions <$r1> and <$r2>")
  }
  
  debug(regSubs)
  
  
  object AbsReg {
    import regex.RegexRegions._
    
    def unapply(x: Reg) = x match {
      case Reg(Sym(sym @ Symbol(str))) if str startsWith "'" => Some(sym)
      case _ => None
    }
  }
  
//  override def delegate(x: Fun) = { debug(x); super.delegate(x) }
//  override def delegate(x: Fun) = {
//    val r = super.delegate(x)
//    
////    def ok(typ: Type): Bool =
////      typ.rargs.size === typ.t.regs.size && (typ.targs forall ok)
////    val rcompl = r.params map (_ typ) match {
////      case TType(_, Seq(), _) => true
//////      case Some(typ) => ok(typ)
////    }
//    
//    def ok(typ: Type): Bool = typ match {
//      case 
//    }
//    
//    r
//  }
  
  override def apply(x: Expr) = (x match {
    case Ascribe(e,typ) => e.obj
    case _ => super.apply(x)
  })
  
  /** Note: does not handle abs types with typ args */
  override def apply(x: Type) = (x match {
//    case at: AbsTyp if subs isDefinedAt at => subs(at)
    case TType(at: AbsTyp,_,_) if bindedElem map (_._1 === at) getOrElse false =>
      throw CompileError(s"cyclic unification with $at and ${bindedElem.get._2}")
    case TType(at: AbsTyp,_,_) =>
      subs.get(at) match {
        case Some(t) =>
//          println(s"rep $at -> $t")
          t
        case _ => x
      }
    //case TRef(IntType,_) => IntType // TODO; doesn't seem to work yet
    case TRef(TType(at: AbsTyp,_,_),_) if (subs isDefinedAt at) && subs(at).t.primitive =>
      subs(at)
    case _ => super.apply(x)
  }) //and println
  override def tspec(x: TypeSpec) = apply(x) // Lazy(apply(x.get))
  override def tparam(x: TypeParam) = apply(x)
  
  override def typs(x: TypSym) = apply(x) //getUnique(x)
  override def funs(x: FunSym) = apply(x) //getUnique(x)
  override def vars(x: a.VarSym) = apply(x)
  override def terms(x: Term) = Typd(apply(x.obj), apply(x.typ), apply(x.reg))
  
  override def apply(r: Reg) = r.transHead(regSubs)
  
}












