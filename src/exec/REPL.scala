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
  
  val ctx = collection.mutable.HashMap[VId,Ptr]()
  
  def rep {
    
    print("> ")
    
    val t = phrase(toplevel)(new lexical.Scanner(readLine))
    
//    println(t)
    
    try { t match {
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
        println(r)
        // TODO put in ctx
      
      case Success(b @ Binding(_id, value), _) =>
//        ctx(id) = ex(rs(ps(value)))
//        println(ctx(id))
        val a = rs(ps(value))
        val Resolved.Binding(id, v) = rs(ps(b))
        ctx(id) = ex(v, ctx toMap)
//        println(v)
        println(s"$id = ${ctx(id)}")
      
      case f: Failure =>
        println(f)
        
      case _ => wtf
        
    }} catch {
      case CompileError(msg) => println(s"Compile error: $msg")
//      case _ if false => ???
    }
    
    println
    
    rep
  }
  
  rep
}

















