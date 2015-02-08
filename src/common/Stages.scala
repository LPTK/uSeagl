package common

import front._
//import Exprs._

object Stages {
  import util._
  import util.Lazy
  import scala.util.Try
  
  trait Pretyped {
  self: Stage =>
    
    type Term = Expr
    
    type TypeSpec = Opt[Type]
    
  }
  
  object Ast extends Stage with Pretyped {
    
    type TypSym = TId
    type FunSym = FId
    type VarSym = VId
    
//    type Term = Expr
//    type TypeAnnot = Opt[Type]
    
    def fname(s: FunSym) = s.toString
    def tname(s: TypSym) = s.toString
  }
  
  
  object Resolving extends Stage with Pretyped {
    
//    type TypSym = Lazy[Try[Resolved.TypSym]]
//    type FunSym = Lazy[Try[Resolved.FunSym]]
//    type VarSym = Lazy[Try[Resolved.Local]]
//    type TypSym = Lazy[Try[Typ]]
//    type FunSym = Lazy[Try[Fun]]
//    type VarSym = Lazy[Try[Local]]
    type TypSym = Lazy[Typ]
    type FunSym = Lazy[Fun]
    type VarSym = Lazy[Local]
    
//    type Term = Expr
    
//    def fname(s: FunSym) = s.get.nam.toString
//    def fname(s: FunSym) = s.get match {//s.get.getOrElse("<error>").toString
//      case scala.util.Success(x) => x.nam
//      case scala.util.Failure(_) => "<error>"
//    }
    def fname(s: FunSym) = "??"
    def tname(s: TypSym) = "??"
  }
  
  object Resolved extends Stage with Pretyped {
    
    type TypSym = Cyclic[Typ]
//    type FunSym = Fun
    type FunSym = Cyclic[Fun]  // Fun
//    type FunSym = Lazy[Fun]  // Fun
    type VarSym = Local
    
//    type Term = Expr
    
//    def fname(s: FunSym) = s.nam.toString
    def fname(s: FunSym) = s.value.nam.toString
    def tname(s: TypSym) = s.value.nam.toString
  }
  
  object Typed extends Stage {
    import typing._
    
    type TypSym = Cyclic[Typ]
    type FunSym = Cyclic[Fun]
    type VarSym = Local
    
    type Term = Typd[Expr]
    
    type TypeSpec = Type
    
    def fname(s: FunSym) = s.value.nam.toString
    def tname(s: TypSym) = s.value.nam.toString
  }
  
  
  
}




















