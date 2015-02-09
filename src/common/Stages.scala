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
    
    type TypeParam = TId
    
    def fname(s: FunSym) = s.toString
    def tname(s: TypSym) = s.toString
    def vname(s: VarSym) = s.toString
    def tpname(s: TypeParam) = s.toString
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
    
    type TypeParam = AbsTyp //Lazy[AbsTyp]
    
//    type Term = Expr
    
//    def fname(s: FunSym) = s.get.nam.toString
//    def fname(s: FunSym) = s.get match {//s.get.getOrElse("<error>").toString
//      case scala.util.Success(x) => x.nam
//      case scala.util.Failure(_) => "<error>"
//    }
    def fname(s: FunSym) = "??"
    def tname(s: TypSym) = "??"
    def vname(s: VarSym) = "??"
    def tpname(s: TypeParam) = s.nam
  }
  
  trait ResolvedStage {
  self: Stage =>
    
    type TypSym = Cyclic[Typ]
    type FunSym = Cyclic[Fun]
    type VarSym = Local
    
    type TypeParam = AbsTyp
    
    
    def fname(s: FunSym) = s.value.nam.toString
    def tname(s: TypSym) = s.value.nam.toString
    def vname(s: VarSym) = s.nam.toString
    def tpname(s: TypeParam): Str = s.nam
    
  }
  
  object Resolved extends Stage with Pretyped with ResolvedStage {
    
//    type TypSym = Cyclic[Typ]
////    type FunSym = Fun
//    type FunSym = Cyclic[Fun]  // Fun
////    type FunSym = Lazy[Fun]  // Fun
//    type VarSym = Local
    
//    type Term = Expr
    
////    def fname(s: FunSym) = s.nam.toString
//    def fname(s: FunSym) = s.value.nam.toString
//    def tname(s: TypSym) = s.value.nam.toString
  }
  
  object Typed extends Stage with ResolvedStage {
    import typing._
    
//    type TypSym = Cyclic[Typ]
//    type FunSym = Cyclic[Fun]
//    type VarSym = Local
    
    type Term = Typd[Expr]
    
    type TypeSpec = Lazy[Type]
//    
//    def fname(s: FunSym) = s.value.nam.toString
//    def tname(s: TypSym) = s.value.nam.toString
  }
  
  
  
}




















