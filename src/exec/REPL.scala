package exec

object REPL extends App {
  
  import common._
  import Stages.Ast._
  import front._
  import Parser._
  
  def rep {
    
    print("> ")
    
    val t = phrase(toplevel)(new lexical.Scanner(readLine))
    println(t)
    t match {
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
    }
    
    println
    
    rep
  }
  
  rep
}





