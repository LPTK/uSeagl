package regex

/**
All we need (cf paper) is:
  - inclusion test (a \subseteq b)
  - full-tree intersection test (a \cap b.*)
  - full-tree difference (a -- b.*)
  
*/
object RegexRegions {
  
  import utils._
  
  case class Sym(sym: Symbol) extends Regex {
    override def toString = sym match { case Symbol(str) => str }
  }
  implicit def symbol2Sym(s: Symbol) = Sym(s)
  object Sym {
    val empty = Sym(Symbol(""))
  }
  
  case object Empty extends Regex
  case object Nil extends Regex
  case class Cat(r1: Regex, r2: Regex) extends Regex
  case class Rep(r: Regex) extends Regex
  case class Or(r1: Regex, r2: Regex) extends Regex
  
  case class Full(r: Regex)
  
  sealed trait Regex {
    import Regex._
    
    // appension -- eqlt to Cat(this,that)
//    def + (that: Regex): Regex = this match {
//      case Empty => Empty
//      case Nil => that
//      case s: Sym => Cat(s, that)
//      case Or(r1, r2) => Or(r1 + that, r2 + that)
//      case Cat(r1, r2) => Cat(r1, r2 + that)
//      case Rep(r) =>
////        ???
//        Cat(Rep(r), that)
//    }
    /** performs "smart appension" **/
    def ~ (that: Regex): Regex = (this, that) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Nil, _) => that
      case (_, Nil) => this
      case (s: Sym, _) => Cat(s, that)
      case (Or(r1, r2), _) => Or(r1 ~ that, r2 ~ that)
      case (Cat(r1, r2), _) => Cat(r1, r2 ~ that)
      case (Rep(r), _) =>
//        ???
        Cat(Rep(r), that)
    }
    def | (that: Regex) = union(this, that)
    
    /** result contains None if can consume 0 symbol (and potentially more); **/  //-- EMPTY symbol key if consuming from $ **/
    def consume: Set[Option[(Sym, Regex)]] = {
      def cat(r1: Regex, r2: Regex) = r1.consume map {
        case Some((s, r)) => List(Some(s -> (r ~ r2)))
//        case Some((s, None)) => List(Some(s -> (Some(r2): Option[Regex])))
        case None => r2.consume
      } flatten; // that's actually weird
      this match {
        case Empty => Set()
        case Nil => Set(None) // Set(Some(Sym.empty -> Nil))
//        case Nil => Set(Some(Sym.empty -> Nil))
        case s: Sym => Set(Some(s -> Nil))
        case Or(r1, r2) => r1.consume ++ r2.consume
        case Cat(r1, r2) => cat(r1, r2)
//        case Rep(r) => r.consume ++ cat(r, Rep(r))
        case Rep(r) => cat(r, Rep(r)) + None // + Some(Sym.empty -> Nil) // + None
      }
    }
////    def consume: Set[(Sym, Option[Regex])] = consumeable.collect{ case Some((s,r)) => s -> r }
//    lazy val consume: Set[(Sym, Regex)] = consumeable.flatten // consumeable.collect{ case Some(sr) => sr }
    lazy val cons = {
      val c = consume.flatten
      c.unzip._1.map(s => s -> c.collect{case(`s`, r) => r}.reduce(union)).toMap
    }
//    lazy val cons = consume.collect { case None => None  case Some() }
    def hasNil = consume contains None
    
    def includes (that: Regex): Boolean = {
      def rec(a: Regex, b: Regex, curr: Set[(Regex,Regex)]): Boolean =
//      if (curr(a -> b) || a == b) true else {
      if (curr(a -> b)) true else {
//        println(a,b)
//        val (c1,c2) = (a.consume, b.consume)
//        if (c1.collectFirst{ case(Sym.empty, Nil) => }.isDefined) return true
        ////val cons = c1.unzip._1.map(s => s -> c1.collect{case(`s`, r) => r}.reduce(union)).toMap
        
//        println("c1: "+cons); println("c2: "+c2)
        b.consume forall {
          case None => //println(a,b,hasNil)
            a.hasNil
          case Some((s, r)) =>
            (a.cons isDefinedAt s) && rec(a.cons(s), r, curr + (a -> b)) // (r == Nil || 
        }
      }
//      println("---")
      rec(this, that, Set())
    }
    
    def equiv(that: Regex) = (this includes that) && (that includes this)

    def inter(that: Regex) = (this | that) - ((this - that) | (that - this))
    def ><(that: Regex) = this inter that
    
    /**
     * "full intersection" tests whether 'this' intersects with 'that.anything',
     * where 'anything' can be any regex.
     * For example, we have 'a.(b|c).d finter a.b' but not 'a.b finter a.b.c'
     */
    def finter(that: Full) = !((this -- that) includes this)
    
    def - (that: Regex): Regex = diff(that, false)
    def -- (f: Full): Regex = diff(f.r, true)
    
    /** TODO use curr to prevent inf rec */
    def diff (that: Regex, full: Boolean): Regex = {
      type M = Map[(Regex,Regex),Regex]
      def rec(a: Regex, b: Regex, curr: Set[(Regex,Regex)]): (Regex,M) =
//      if (curr isDefinedAt (a -> b)) (Empty, curr + (a -> b -> Nil)) else {
      if (curr (a -> b)) (Empty, Map(a -> b -> Nil)) else {
        if (full && (b includes Nil)) return (Empty, Map())
//        val (c1,c2) = (a.consume, b.consume)
//        c1 filter {
//          case (s,r) => !(c2.collectFirst{ case(`s`,r2) => } isDefined)
//        }
//        println(c1,b.cons)
        val (r,m) = (a.consume map {
//          case Some((Sym.empty, r)) => assert(r == Nil); Nil
          case Some((Sym.empty, r)) => wtf
//          case None => Nil  // TODO: really?
          case None if b.hasNil => (Empty, Map())
          case None => (Nil, Map())
          
//          case(Sym.empty, Assert(Nil)) => Nil
          case Some((s,r)) => //println(a,b)
//            s + (if (b.cons isDefinedAt s) rec(r, b.cons(s), curr) else r)
            
//            println(s"$s $r  $b")
            
            if (b.cons isDefinedAt s) {
              val (r2,m) = rec(r, b.cons(s), curr + (a -> b))
//              if (m isDefinedAt (a,b)) println("!!",a,b,m(a,b),r2)
//              val ret = if (m isDefinedAt (a,b)) Rep(m(a,b)) + s + r2
//                        else s + r2
//              (r2, m mapValues (r2 + _))
              (s ~ r2, m mapValues (s ~ _))
            }
            else (s ~ r, Map())
//        } reduce union
//        } foldLeft (Empty: Regex)) (union)
        } foldLeft (Empty: Regex, Map():M)) {
          case ((r,m),(nr,nm)) => (r | nr, m ++ nm)
        }
        val ret = if (m isDefinedAt (a,b)) Rep(m(a,b)) ~ r
                  else r
        (ret, m - (a -> b))
      }
      rec(this, that, Set())._1
    }
    
    def symDiff (that: Regex) = (this -- Full(that)) | (that -- Full(this))  // union(this -- f, f.r -- Full(this))
    
    def inter (f: Full) = ???
    
    val parDbgEnabled = false
    def parDbg(x:Any) = if (parDbgEnabled) s"($x)" else s"$x"
    
    override def toString = toString(true)
    protected def toString(printPar: Boolean): String = {
      def par(s: String) = if (printPar || parDbgEnabled) s"($s)" else s
      this match {
        case Empty => "()"
        case Nil => "$"
//        case Cat(id, Nil) => s"$id"
        case s: Sym => s.toString
        case Cat(id, rest) => parDbg(s"$id.$rest")
        case Rep(r) =>
//          parDbg(r toString)+"*" // s"$r*"
          parDbg(s"$r*")
//        case Or(r1,Or(r21,r22)) => par(s"$r1 | ${r2.toString(false)}")
        case Or(r1, r2: Or) => par(s"$r1 | ${r2.toString(false)}")
        case Or(r1: Or, r2) => par(s"${r1.toString(false)} | $r2")
        case Or(r1,r2) => par(s"$r1 | $r2")
      }
    }
    
//    override def equals(that: Any) = throw new java.lang.UnsupportedOperationException("Use function equiv instead of equals")

    
  }
  object Regex {
    def union(a: Regex, b: Regex): Regex = union(a,b,true)
    def union(a: Regex, b: Regex, tryReverse: Boolean): Regex = a match {
//      case Nil => Nil // n'importe quoi!
      case Empty => b
      case _ if tryReverse => union(b, a, false)
      case _ => Or(b,a)
    }
    
  }
  
  
//  object Assert {
//    def unapply[T](x: T) = new {
//      def isEmpty = false
//      override def equals (that: Any) =
//        (x == that) || (throw new AssertionError(s"$that is not equal to $x"))
//    }
//  }
  
    
  
}




