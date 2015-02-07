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
  
  
  def fname(s: FunSym): Str
  
  
  
}







