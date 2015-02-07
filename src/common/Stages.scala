package common

import front._
//import Exprs._

object Stages {
  import util._
  import util.Lazy
  import scala.util.Try
  
  object Ast extends Stage {
    
    type TypSym = TId
    type FunSym = FId
    type VarSym = VId
    
    type Term = Expr
    
    def fname(s: FunSym) = s.toString
  }
  
  
  object Resolving extends Stage {
    
//    type TypSym = Lazy[Try[Resolved.TypSym]]
//    type FunSym = Lazy[Try[Resolved.FunSym]]
//    type VarSym = Lazy[Try[Resolved.Local]]
    type TypSym = Lazy[Typ]
    type FunSym = Lazy[Fun]
    type VarSym = Lazy[Local]
    
    type Term = Expr
    
    def fname(s: FunSym) = s.get.nam.toString
  }
  
  object Resolved extends Stage {
    
    type TypSym = Typ
//    type FunSym = Fun
//    type FunSym = Cyclic[Fun]  // Fun
    type FunSym = Lazy[Fun]  // Fun
    type VarSym = Local
    
    type Term = Expr
    
//    def fname(s: FunSym) = s.nam.toString
    def fname(s: FunSym) = s.get.nam.toString
  }
  
  
  
  
}















