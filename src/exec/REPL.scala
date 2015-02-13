package exec

import common.Reporting

object REPL extends App {
  
  import util._
  import common._
  import Stages._
  import Ast._
  import front._
  import Parser._
  import Reporting.CompileError
  import Memory._
  import typing._
//  import Memory._
  
  val ps = new Presolve
  val rs = new Resolve(ps)
  val ty = new Typing(rs)
  val ex = new Exec
  
  val ctx = collection.mutable.HashMap[VId,Value]()
  
  def rep {
    
    print("> ")
    
    val t = phrase(repl)(new lexical.Scanner(readLine))
    
//    println(t)
    
    try { t match {
      // Commands:
//      case Success(Var(VId('exit)), _) =>
//        System.exit(0)
//        
//      case Success(Var(VId('ctx)), _) =>
//        println(ctx)
//        println("Types:\n  " + ty.typTable.values.mkString("\n  "))
//        println("Funs:\n  " + ty.funTable.values.mkString("\n  "))
//        
//      case Success(Var(VId('h)), _) if !(ctx isDefinedAt VId('h)) =>
//        println(ex.h)
      case Success(Command(c), _) => c match {
        case "e" =>
          System.exit(0)
        case "h" =>
          println(ex.h)
        case "c" =>
          println(ctx)
          println("Types:\n  " + ty.state.typTable.values.collect{case Cyclic(ct: Typed.ConcTyp) => ct}.mkString("\n  "))
          println("Funs:\n  " + ty.state.funTable.values.mkString("\n  "))
        case _ => println(s"Unknown command :$c")
      }
      
      case Success(IntLit(42), _) =>
        while(true) ()
        
      // Legit stuff:
      case Success(fun: Fun, _) =>
        
        val f = ps(fun)
//        println(f)
        
        val r = rs(f)
//        println(r)
        
        val t = ty(r)
        println(t)
        
      case Success(typ: Typ, _) =>
//        println(ty(rs(ps(typ))))
        val t = ty(rs(ps(typ)))
        ty.flushChecks
        println(t)
      
      case Success(e: Expr, _) =>
        val re = rs(ps(e))
//        println("Typed: " + ty.terms(re))
        val te = ty.typeUnify(re)
        println("Typed: " + te.typ)
        val r = ex(re, ctx toMap)
        println(ex.h.dispVal(r))
        ex.h.dealloc(r)
        // TODO put in ctx
      
      case Success(b @ Binding(_id, value), _) =>
//        ctx(id) = ex(rs(ps(value)))
//        println(ctx(id))
//        val a = rs(ps(value))
        val Resolved.Binding(id, v) = rs(ps(b))
        val tv = ty.typeUnify(v)
        val xv = ex(v, ctx toMap)
        if (ctx isDefinedAt id)
          ex.h.dealloc(ctx(id))
        ctx(id) = xv
//        println(v)
        println(s"$id: ${ex.h.dispVal(ctx(id))}")
      
      case f: Failure =>
        println(f)
        
      case _ => wtf
        
    }} catch {
      case CompileError(msg) =>
//        System.err.
        println(s"Compile error: $msg")
      case Exceptions.ExecException(msg) => System.err.println(s"Runtime error: $msg")
//      case _ if false => ???
    }
    
    println
    
    rep
  }
  
  rep
}

















