package exec

import common.Reporting

object REPL extends App {
  
  import utils._
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
//  val ty = new Typing(rs)
  val ex = new Exec
  
  val pt = new Pretype(rs)
  val ag = new Aggregate(pt)
  
  
//  val ctx = collection.mutable.HashMap[VId,Value]()
  val ctx = collection.mutable.HashMap[VId,(Typed.Type,Value)]()
  
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
          println("Locals:\n  " + ctx.mapValues{case(t,v) => s"$v : $t"}.mkString("\n  "))
          println("Types:\n  " + ag.state.typTable.values.collect{case Cyclic(ct: Typed.ConcTyp) => ct}.mkString("\n  "))
          println("Funs:\n  " + ag.state.funTable.values.filter{_.nam =/= ag.IntlFId}.mkString("\n  "))
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
        
//        val t = ty(r)
        val t = ag.renew(pt(r))
        println(t)
        
      case Success(typ: Typ, _) =>
////        println(ty(rs(ps(typ))))
//        val t = ty(rs(ps(typ)))  
//        ty.flushChecks
        val t = ag.renew(pt(rs(ps(typ))))
        println(t)
      
      case Success(Left(e: Expr), _) =>
        val re = rs(ps(e))
//        println("Typed: " + ty.terms(re))
        
//        val te = ty.typeUnify(re)
        val te = ag.typeUnify(re)
        println("Typed: " + te.typ)

        val r = ex(re, ctx.mapValues(_ _2) toMap)
        println(ex.h.dispVal(r))
        ex.h.dealloc(r)
        // TODO put in ctx
      
      case Success(Right(b @ Binding(loc, value)), _) =>
//        ctx(id) = ex(rs(ps(value)))
//        println(ctx(id))
//        val a = rs(ps(value))
        try {
          val rb @ Resolved.Binding(loc, v) = rs(ps(b))
          val id = loc.nam
          
  //        val tv = ty.typeUnify(rb)
          val tv = ag.typeUnify(rb)
          println("Typed: " + tv.typ)
          
          val xv = ex(v, ctx.mapValues(_ _2) toMap)
          if (ctx isDefinedAt id)
            ex.h.dealloc(ctx(id)._2)
          ctx(id) = tv.typ -> xv
  //        println(v)
          println(s"$id: ${ex.h.dispVal(ctx(id)._2)}")
//          println(ag.state.varTable)
        }
        catch {
          case t: Throwable =>
            ps.getCtx.locTable -= loc.nam
            throw t
        }
      case f: Failure =>
        println(f)
        
      case _ =>
        println(t)
        wtf
        
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

















