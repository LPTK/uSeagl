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
    
  }
  
  
  object Resolving extends Stage {
    
//    type TypSym = Lazy[Try[Resolved.TypSym]]
//    type FunSym = Lazy[Try[Resolved.FunSym]]
//    type VarSym = Lazy[Try[Resolved.Local]]
    type TypSym = Lazy[Try[Typ]]
    type FunSym = Lazy[Try[Fun]]
    type VarSym = Lazy[Try[Local]]
    
    type Term = Expr
    
  }
  
  object Resolved extends Stage {
    
    type TypSym = Typ
    type FunSym = Cyclic[Fun]  // Fun
    type VarSym = Local
    
    type Term = Expr
    
  }
  
  
  
  
}















