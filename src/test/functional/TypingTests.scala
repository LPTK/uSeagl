package functional

import org.scalatest.junit.JUnitSuite
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import org.junit.Ignore
import org.junit.Before

import utils._
import common._
import Reporting._

/**
 * TODO:
 * 
 *   List with get
 *   
 *   mutrec funs
 *   
 *   polymrec fun
 *   
 *   
 *   region inference stuff...
 *   
 *   
 *   c = new Cell(nil)
     id[Cell[Int]](c)
     id[Cell[Unit]](c) // should unif-fail
 *   
 *   
 *   
 *   
 */
class TypingTests extends JUnitSuite {
  
  
  def valid(txt: Str) = process(txt)
  def typeError(txt: Str) = try {
    val r = process(txt);
    fail("this did typecheck: "+txt+"  to:\n"+r.mkString("\n")) }
  catch {
    case CompileError(_) =>
  }
  
  def validExpr(txt: Str) = valid(s"\nfun main = { $txt ; () }\n")
  def errorExpr(txt: Str) = typeError(s"\nfun main = { $txt ; () }\n")
  
  def process(txt: Str) = {
    
    import Stages._
    import Ast._
    import front._
    import Parser._
    import Reporting.CompileError
    import typing._
    
    val ps = new Presolve
    val rs = new Resolve(ps)
    
    val pt = new Pretype(rs)
    val ag = new Aggregate(pt)
    
//    val t = phrase(rep(toplevel))(new lexical.Scanner(txt))
    phrase(pgrm)(new lexical.Scanner(txt)) match {
      case Success(Pgrm(typs,funs), _) =>
//        val pt = typs.values map (t => ps(t))
        val (pst,psf) = (typs.values map ps.apply, funs.values map ps.apply)
//        rs(pf.head)
        val (rt,rf) = (pst map rs.apply, psf map (_ value) map rs.apply)
        val (ptt,ptf) = (rt map pt.apply, rf map (_ value) map pt.apply)
        val (agt,agf) = (ptt map ag.apply, ptf map (_ value) map ag.renew) // gotcha!
        agf
    }
    
    
    
    
  }
  
  @Test def trivial {
    assertEquals(2, 1+1)
    valid(
"""
fun f(x) = x
""")
  }
  
  @Test def locals {
  
validExpr("a = (); a")
errorExpr("a; a = ()")

  }
  
  @Test def basicrefs {
validExpr("""
a = ();
a: Ref[Unit]{a};
b = a;
b: Ref[Unit]{a}
""")
validExpr("""
a = 42;
a: Int;
b = a;
b: Int
""")
  }
  
  @Test def refcoerce {
    
validExpr("a = (); a: Ref[Unit]")
errorExpr("a = (); a: Unit")

  }
  
  
  @Test def lists {
    valid(
"""
typ List[T](head: T, tail: List[T]);
fun get(ls:List,n):Ref{ls.head} = ls.head;
fun gett(ls:List,n):Ref{ls.tail.tail.head} = ls.tail.tail.head
""")
  }

  
  @Test def basicrec
{
valid("""
fun foo(x) = foo(x);
fun main = as[Int](foo(()))
""")
valid("""
fun foo(x) = if x then foo(x) else x;
fun main = as[Int](foo(42))
""")

//// TODO fix: probably broken bc functions are renewed..?
//typeError("""
//fun foo(x) = if x then foo(x) else x;
//fun main = as[Int](foo(()))
//""")
}

  
  @Test def mutrec
{
valid("""
fun foo(x) = bar(x);
fun bar(x) = foo(x);
fun main = as[Int](foo(()))
""")
valid("""
fun foo(x) = if x then bar(x) else x;
fun bar(x) = foo(x);
fun main = as[Int](foo(42))
""")
typeError("""
fun foo(x) = if x then bar(x) else x;
fun bar(x) = foo(x);
fun main = foo(())
""")
}

  
  @Test def polymrec
{;

valid("""
fun foo[T](x: T):Ref[T]{x} = x
""")
valid("""
fun foo[T](x: T) = x
""")
valid("""
fun foo[T](x: T):Ref[T] = x
""")

valid("""
fun foo[T](x: T):Ref[T]{x} = foo(x)
""")

//// TODO fix
//typeError("""
//fun foo[T](x: T):Ref[T] = foo(x)
//""")
//typeError("""
//fun foo[T](x: T):Ref = foo(x)
//""")


}

  @Test def basicunif
{
valid("""
fun main(x) = {
  y = nil;
  if x then y else y;
  as[Int](x);
  as[Unit](y)
}
""")

valid("""
fun main(x:Int) = {
  y = nil;
  coe(x,y);
  as[Int](x);
  as[Int](y)
}
""")


typeError("""
fun main = as[Int](())
""")


}
  
  
  @Test def inferreg
{
valid("""
""")
}

  
  
  @Test def ascription
{
validExpr("42:Int")
validExpr("new Pair(42,()):Pair[Int,Unit]")
errorExpr("1:Unit")
errorExpr("new Pair((),42):Pair[Int,Unit]")
}


}

































