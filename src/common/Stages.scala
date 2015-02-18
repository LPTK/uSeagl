package common

import utils._
import front._
import typing._


trait Stage extends Exprs with Types with Pgrms {
  
  type TypSym
  type FunSym
  type VarSym
  
  type Term //<: Stmt
  
  type Decl
  
  type TypeSpec
  type TypeParam
  
  
  def fname(s: FunSym): Str
  def tname(s: TypSym): Str
  def vname(s: VarSym): Str
  def tpname(s: TypeParam): Str  
  
}


object Stages {
  import utils.Lazy
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
    
    type TypeParam = TId
    
    def fname(s: FunSym) = s.toString
    def tname(s: TypSym) = s.toString
    def vname(s: VarSym) = s.toString
    def tpname(s: TypeParam) = s.toString
  }
  
  
  object Resolving extends Stage with Pretyped {
    
    type TypSym = Lazy[Typ]
    type FunSym = Lazy[Fun]
    type VarSym = Lazy[Local]
    
    type TypeParam = AbsTyp
    
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
    def tname(s: TypSym) = s.value.namStr
    def vname(s: VarSym) = s.nam.toString
    def tpname(s: TypeParam): Str = s.nam
  }
  
  object Resolved extends Stage with Pretyped with ResolvedStage {
    
  }
  
  object Typed extends Stage with ResolvedStage {
    import typing._
    
    type Term = Typd[BasicExpr]
    
    type TypeSpec = Type
    
  }
  
  
  
}




















