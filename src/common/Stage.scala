package common

import util._
import front._
import typing._

trait Stage extends Exprs with Types with Pgrms {
  
  type TypSym
  type FunSym
  type VarSym
  
  type Term
  
  type Decl
  
  type TypeSpec
  type TypeParam
  
  
  def fname(s: FunSym): Str
  def tname(s: TypSym): Str
  def vname(s: VarSym): Str
  def tpname(s: TypeParam): Str  
  
}







