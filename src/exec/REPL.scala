package exec

object REPL extends App {
  
  import common._
  import Stages._
  import Ast._
  import front._
  import Parser._
  import Reporting.CompileError
  
  def rep {
    
    print("> ")
    
    val t = phrase(toplevel)(new lexical.Scanner(readLine))
    println(t)
    try { t match {
      case Success(fun: Fun, _) =>
        
        val read = new Read
        val f = read(fun) //: Resolving.Fun
        println(f)
        
        val cf = new Cyclic[Resolving.Fun](_ => f)
        
        val res = new Resolve
//        println(res(f))
        val r = res(f)
        println(r)
        r
        
      case Success(typ: Typ, _) =>
//        try {
//          println(s"typed: ${SimplyTyped.typeof(trees)}")
//        } catch {
//          case te: TypeError => println(te toString)
//        }
////        println("exec: ")
//        for (t <- path(trees, reduce))
//          println(t)
        
      case _ =>
        
    }} catch {
      case CompileError(msg) => println(s"Compile error: $msg")
      case _ if false => ???
    }
    
    println
    
    rep
  }
  
  rep
}





