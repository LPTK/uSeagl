package regex

object Tests2 extends App {
  
  import utils._
  import RegexRegions._
  val p = new RegParser{}
  import p._
  
  println("Testing...")
  
  
  implicit def str2reg(s: Str) = phrase(reg)(new lexical.Scanner(s)) match {
    case Success(r, _) => r
    case f: Failure =>
      System.err.println(f)
      Empty
    case e: Error => throw new Exception(e.toString)
  }
  
//  val (t,f) = (true, false)
  val T = true
  val F = false
  
  
  def testBinary[R](name: Str, op: Str, f: (Regex,Regex) => R, cases: (Regex,Regex,R)*)(eq: (R,R) => Bool = ((_:R) == (_:R))) =
//    for ((a,b,exp) <- cases; r = f(a,b))
//      if (r != exp) println(s"/!\\ $name with  $a , $b  returned $r")
////      else println(s"$name ok")
    for ((a,b,exp) <- cases) try {
      val r = f(a,b)
      if (!eq(r, exp)) println(s"/!\\ $name:  $a $op $b  ==  $r  !=  $exp")
//      else println(s"$name ok")
    } catch {
      case e: Throwable =>
        System.err.println(s"/!\\ $name:  $a $op $b  threw  $e")
        throw e
    }
  ;
  
  
  testBinary("INCL", ">", (_ includes _),
      ("$", "$", T),
      ("$", "a", F),
      ("a", "$", F),
      ("()", "()", T),
      ("()", "a", F),
      ("a", "()", T),
      ("a", "a", T),
      ("a", "b", F),
      ("a | a", "a", T),
      ("a | b", "b", T),
      ("a|b", "b|a", T),
      ("a", "a | b", F),
      ("a.b", "a.b", T),
      ("a.b", "a.b.c", F),
      //
      ("(a.a).a", "a.(a.a)", T),
      ("(a.a).a | b", "b | a.(a.a)", T),
      ("((a))", "() | a", T),
      ("(a.(a.(a)*)) | $", "$ | ((a.a).(a)*)", T),
      ("$ | ((a.a).(a)*)", "(a.(a.(a)*)) | $", T),
//      ("$ | a.a.a*", "$ | ((a.a).(a)*)", T),
      //
      ("x*", "$", T),
      ("x*", "x", T),
      ("x.y*", "x", T),
      ("x.y*", "x.y", T),
      ("x.y*", "x.y.y.y.y.y", T),
      ("x.y.y*", "x.y", T),
      ("x.y.y*", "x", F),
      ("(a.a.a* | $)", "($ | a.a.a*)", T),
      ("($ | a.a.a*)", "(a.a.a* | $)", T),
      
      ("(a|b)*", "a*|b*", T),
      ("(a|b)*", "(a*.b).(a|b)*", T),
      
      //
      
      ("x*", "x*", T),
      ("x*", "x.x.x*", T),
      ("x.x.x*", "x*", F),
      ("x.x*", "x*", F),
      ("$|x|x.x.x*", "x*", T),
      
      
      //
      ("a", "b", F),
      ("a", "b", F)
  )()
  
  testBinary[Regex]("DIFF", "-", (_ - _),
      ("$", "$", "()"),
      ("a", "a", "()"),
      ("a", "b", "a"),
      ("()", "x", "()"),
      ("x", "()", "x"),
      ("a.b", "a", "a.b"),
      ("a.b", "a.b", "()"),
      ("(a|b).(u|v)", "a.v", "a.u | b.(u|v)"),
      ("b.u", "(a|b).(u|v)", "()"),
      //
      ("a*", "$", "a.a*"),
      ("a*", "a", "$ | a.a.a*"),
      //
      ("a*", "a*", "()"),
      ("a*", "b*", "a.a*"),
      ("a*.b*", "a*.b.b*", "a*"),
      ("a*.b*", "a*.a.b*", "b*"),
      ("(a|b)*", "a*", "(a*.b).(a|b)*"),
      ("a*", "(a|b)*", "()"),
      ("a*.b*", "(a|b)*", "()"),
      ("(a|b)*", "a*.b*", "a*.b.b*.a.(a | b)*"),
      // ^ (result copied from output:) "ensures" there is at least one instance of "b.a" so it is not in a*.b*! Neat!
      
      //
      ("$", "$", "()"),
      ("$", "$", "()")
  )(_ equiv _)
  
  testBinary[Regex]("FDIF", "--", (_ -- Full(_)),
      ("a", "a", "()"),
      ("a.b", "a", "()"),
      ("a.(b|c)", "a", "()"),
      ("a.(b|c)", "a.b", "a.c"),
      ("a.(b|c).d", "a.b", "a.c.d"),
      //
      ("a*.b*", "a*", "()"),
      ("a*.b*", "a.a*", "b*"),
      
      //
      ("a", "a", "()"),
      ("a", "a", "()")
  )(_ equiv _)
  
  testBinary[Regex]("INTER", "><", (_ >< _),
      ("a", "a", "a"),
      ("a", "()", "()"),
      ("a|b", "b|c", "b"),
      ("a.(b|c)", "a.b", "a.b"),
      ("(a|b).(c|d)", "(u|a).(v|d)", "a.d"),
      //
      ("a*.b*.c*", "a*", "a*"),
      ("a*.b*.c*", "a*.c*", "a*.c*"),
      ("a*.b*", "b*.c*", "b*"),
      
      //
      ("a", "a", "a"),
      ("a", "a", "a")
  )(_ equiv _)
  
  testBinary("FINTER", "><", (_ finter Full(_)),
      ("a.(b|c).d", "a.b", T),
      ("a.b", "a.b.c", F),
      
      //
      ("a", "a", T),
      ("a", "a", T)
  )()
  
  println("\nDone.")
  
//  println("($ | a.a.a*)" equiv "(a.a.a* | $)")
  
//  println
//  println("a".consume)
//  println("$".consume)
//  println("x*".consume)
  
}



/*

R = (a|b)* - a*
R = a.(a|b)* - a* | b.(a|b)* - a*
R = a.((a|b)* - a*) | b.(a|b)*
R = a.R | b.(a|b)*



*/





























