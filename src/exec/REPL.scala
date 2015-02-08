package exec

object REPL extends App {
  
  import util._
  import common._
  import Stages._
  import Ast._
  import front._
  import Parser._
  import Reporting.CompileError
  import Memory._
//  import Memory._
  
  val ps = new Presolve
  val rs = new Resolve
  val ex = new Exec
  
  val ctx = collection.mutable.HashMap[VId,Value]()
  
  def rep {
    
    print("> ")
    
    val t = phrase(toplevel)(new lexical.Scanner(readLine))
    
//    println(t)
    
    try { t match {
      // Commands:
      case Success(Var(VId('exit)), _) =>
        System.exit(0)
        
      case Success(Var(VId('ctx)), _) =>
        println(ctx)
        
      case Success(IntLit(42), _) =>
        while(true) ()
        
      // Legit stuff:
      case Success(fun: Fun, _) =>
        
        val f = ps(fun)
        println(f)
        
        val r = rs(f)
        println(r)
      
      case Success(typ: Typ, _) =>
        println(rs(ps(typ)))
      
      case Success(e: Expr, _) =>
        val re = rs(ps(e))
        val r = ex(re, ctx toMap)
        println(ex.dispVal(r))
        // TODO put in ctx
      
      case Success(b @ Binding(_id, value), _) =>
//        ctx(id) = ex(rs(ps(value)))
//        println(ctx(id))
        val a = rs(ps(value))
        val Resolved.Binding(id, v) = rs(ps(b))
        ctx(id) = ex(v, ctx toMap)
//        println(v)
        println(s"$id: ${ex.dispVal(ctx(id))}")
      
      case f: Failure =>
        println(f)
        
      case _ => wtf
        
    }} catch {
      case CompileError(msg) => System.err.println(s"Compile error: $msg")
      case Exceptions.ExecException(msg) => System.err.println(s"Runtime error: $msg")
//      case _ if false => ???
    }
    
    println
    
    rep
  }
  
  rep
}

















