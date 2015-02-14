package typing

import util._
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
 *  coalesce Ref[Ref] into Ref and Ref[Int] into Int
 * 
 */
class Unify2(val ag: Aggregate) extends Types.singleStaged.Identity with StageIdentDefs {
  import ag.pt._
  
  override val state = ag.state
  
//  sealed trait QualType { def map(f: Type => Type): QualType }
//  case class Strong(typ: Type) extends QualType { def map(f: Type=>Type) = Strong(f(typ)) }
//  case class Weak(typ: Type) extends QualType { def map(f: Type=>Type) = Weak(f(typ)) }
//  case class Field(typ: Type, id: VId) extends QualType { def map(f: Type=>Type) = Field(f(typ), id) }
  
  val subs = mutable.Map[AbsTyp, Type]()
  
//  println(ag.cstrs)
//  println(s"${ag.cstrs.hard_cstrs mkString ", "} - ${ag.cstrs.soft_cstrs mkString ", "} - ${ag.cstrs.field_cstrs mkString ", "}")
  
  val hc = ag.cstrs.hard_cstrs.clone
  val sc = ag.cstrs.soft_cstrs.clone
  val fc = ag.cstrs.field_cstrs.clone
  
  def printCstrs = {
//    def p[T](t: Traversable[T]) = 
    println(s"${
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
          subs.transform{ case(at,qt) => noCycle(at,qt){ apply(qt) } }
          println(hc.mkString(", ") + "  " + subs)
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
        if (t1 =/= t2) throw UnificationError(t1,t2)
//        if (t1 =/= t2) throw UnificationError(tt1,tt2)
        (targs1 zip targs2) map hc.+=
    }
    
    sc.transform{ case(t1,t2) => (apply(t1),apply(t2)) }
    fc.transform{ case(t1,id,t2) => (apply(t1),id,apply(t2)) }
    
    printCstrs
    
    if (!sc.isEmpty) sc.remove(sc.size-1) match {
      case (TType(RefTyp, Seq(t1), _), TType(RefTyp, Seq(t2), _)) => sc += (t1 -> t2)
      case (TType(RefTyp, Seq(t1), _), t2) => hc += (t1 -> t2)
      case (t1, TType(RefTyp, Seq(t2), _)) => hc += (t1 -> t2)
      case (t1, t2) => hc += (t1 -> t2)
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
            sc += (tt.transType(loc.typ) -> tf)
          case None =>
            throw CompileError(s"Type ${ct} has no field $f")
        }
      case _ =>
    }) oh_and (i += 1)
    
  }
  
  fc.headOption map { case(t,f,tf) => throw CompileError(s"Unknown field access _.$f on abstract type $t") }
  
  println(subs)
  
  
  /** Note: does not handle abs types with typ args */
  override def apply(x: Type) = (x.t.value match {
//    case at: AbsTyp if subs isDefinedAt at => subs(at)
    case at: AbsTyp if bindedElem map (_._1 === at) getOrElse false =>
      throw CompileError(s"Cyclic unification with $at and ${bindedElem.get._2}")
    case at: AbsTyp =>
      subs.get(at) match {
        case Some(t) =>
//          println(s"rep $at -> $t")
          t
        case _ => x
      }
    case _ => super.apply(x)
  }) //and println
  override def tspec(x: TypeSpec) = apply(x) // Lazy(apply(x.get))
  override def tparam(x: TypeParam) = apply(x)
  
  override def typs(x: TypSym) = apply(x) //getUnique(x)
  override def funs(x: FunSym) = apply(x) //getUnique(x)
  override def vars(x: a.VarSym) = apply(x)
  override def terms(x: Term) = Typd(apply(x.obj), apply(x.typ))
  
  
}




/** TODO: what about ctrs overriden? a->X; a->Y */
class Unify(st: StageState[Typed.type], unifs: Map[AbsTyp,Type]) extends Types.singleStaged.Identity {
  
  override val state = st
  import state._
  
//  override val funTable = ty.funTable
//  val typTable = HashMap[a.Typ, Cyclic[Typ]]()
//  val varTable = HashMap[a.Local, Local]()
  
////  override def getUnique(x: Typ) = ty.typTable.find(_ == x)
//  override def getUnique(x: Typ) = {
////    println(x, ty.typTable.values map (_ value))
//    typTable.values.find(_.value === x) get
////    ty.allTypes(x)
//  } //and println
////  override def getUnique(x: Fun) = ty.funTable.values.find(_.value === x) get
  
  
  println("OLD UNIF")
  println(unifs)
  
  var u = unifs
  
//  val unifsTrans = mutable.Map(unifs.toSeq: _*)
  val unifsTrans = mutable.Map[AbsTyp,Type]()
//  for ((at,t) <- unifs)
  for (k <- unifs.keys) {
    unifsTrans += (k -> u(k))
//    println(k, u(k))
    u = u mapValues apply
//    println(u)
  }
//  println
//  println(unifsTrans)
  
  /** Note: does not handle abs types with typ args */
  override def apply(x: Type) = (x.t.value match {
    case at: AbsTyp if unifsTrans isDefinedAt at => unifsTrans(at)
    case _ => super.apply(x)
  }) //and println
  override def tspec(x: TypeSpec) = apply(x) // Lazy(apply(x.get))
  override def tparam(x: TypeParam) = apply(x)
  
  override def typs(x: TypSym) = getUnique(x)
  override def funs(x: FunSym) = getUnique(x)
  override def vars(x: a.VarSym) = apply(x)
  override def terms(x: Term) = Typd(apply(x.obj), apply(x.typ))
  
  
////  override def typs(x: TypSym) = mkCycle(x) //x oh_and println(x)
//  override def typs(x: TypSym) = x.value match {
//    case at: AbsTyp => mkCycle(unifs.getOrElse(at, at))
//    case ct: ConcTyp => mkCycle(ct)
//  }
//  override def tspec(x: TypeSpec) = apply(x) // oh_and println(x)
////  override def apply(x: Fun) = super.apply(x) oh_and println(x)
//  
//  override def tparam(x: TypeParam) = apply(x)
//  
////  override def apply(x: Typ) = (x match {
//////    case at: AbsTyp if unifs isDefinedAt at => unifs(at)
////    case at: AbsTyp => unifs.getOrElse(at, at)
////    case ct: ConcTyp => ct
////  }) oh_and println(x)
//  
////  val btyps = Seq() //wtf
}










