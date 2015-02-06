package common

import front._
//import Exprs._

object Stages {
  
  object Ast extends Stage {
    
    type TypSym = TId
    type FunSym = FId
    type VarSym = VId
    
    type Term = Expr
    
  }
  
  
  object Resolved extends Stage {
    
    type TypSym = Typ
    type FunSym = Fun
    
    type Term = Expr
    
  }
  
  
  
  
}






