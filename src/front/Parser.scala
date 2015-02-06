package front

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

object Parser extends StandardTokenParsers with regex.RegParser {
  import util._
  import common._
  import Stages.Ast._
  import Specs._
//  import RegParser._
  import Regions._
  
  lexical.delimiters ++= List(".", "{", "}", ",", "=", ";", ":", "(", ")", "[", "]", "|", "=>", "->")
  lexical.reserved   ++= List("fun", "typ", "@read", "@inval", "@trans")
  
  def typ: Parser[Typ] = "typ" ~> ident ~ (typList?) ~ (regList?) ~ (paramList?) ^^ {
    case nam ~ typs ~ regs ~ params => Typ(TId(nam),
        typs getOrElse (Seq()),
        regs getOrElse (Seq()),
        params getOrElse (Seq())
    )
  }
  
  def paramList: Parser[Seq[Param]] = "(" ~> repsep(param, ",") <~ ")"
  def typList: Parser[Seq[TId]] = "[" ~> repsep(ident, ",") <~ "]" ^^ (_ map TId.apply)
  def regList: Parser[Seq[VId]] = "{" ~> repsep(ident, ",") <~ "}" ^^ (_ map VId.apply)
  
  def param: Parser[Param] = ident ~ (":" ~> typez) ^^ {
    case nam~t => Param(VId(nam),t)
  }
  
  def targs = ("[" ~> repsep(typez, ",") <~ "]")
  def rargs = ("{" ~> repsep(regi, ",") <~ "}")
  
  def typez: Parser[Type] = ident ~ (targs?) ~ (rargs?) ^^ {
    case nam ~ targs ~ rargs => Type(TId(nam),
        targs, //getOrElse (Seq()),
        rargs //getOrElse (Seq())
    )
  }
//  def ftyp: Parser[Type] = ident ~ (typList?) ~ (paramList?) ^^ {
//    case nam ~ typs ~ regs => Type(TId(nam), 
//        typs getOrElse (Seq()),
//        regs getOrElse (Seq())
//    )
//  }
  
  def fun: Parser[Fun] = "fun" ~> ident ~ (typList?) ~ (regList?) ~ (paramList?) ~ (":" ~> typez) ~ (spec?) ^^ {
    case nam ~ typs ~ regs ~ params ~ ret ~ spec => Fun(FId(nam),
        typs getOrElse (Seq()),
        regs getOrElse (Seq()),
        params getOrElse (Seq()),
        ret,
        spec getOrElse (Spec.empty)
    )
  }
  
  def spec: Parser[Spec] = 
    ("@read" ~> reg) ~ ("@inval" ~> reg) ~ ("@trans" ~> repsep(trans,",")) ^^ {
    case r ~ i ~ t => Spec(XReg(r),XReg(i),t)
  }
  
//  def a = ("=>" | "->") ~ reg
  
//  def trans: Parser[Seq[Trans]] = reg ~ ("=>" | "->") ~ reg ^^ {
//    case f ~ "->" ~ t => Trans(f,t,false)
//    case f ~ "=>" ~ t => Trans(f,t,true)
//  }
  def trans: Parser[Trans] =
//    ((reg <~ "=>") ~ reg) ^^ { case f~t => Trans(XReg(f),XReg(t),false) } |
//    ((reg <~ "->") ~ reg) ^^ { case f~t => Trans(XReg(f),XReg(t),true)  }
    ((regi <~ "=>") ~ regi) ^^ { case f~t => Trans(f,t,false) } |
    ((regi <~ "->") ~ regi) ^^ { case f~t => Trans(f,t,true)  }
  ;
  def regi = reg ^^ (XReg.apply)
  
  def decl: Parser[Decl] = typ | fun
  
  def pgrm: Parser[Pgrm] = repsep(decl, ";") ^^ {
    case dls => Pgrm(
        dls collect {case typ: Typ => typ.nam -> typ} toMap,
        dls collect {case fun: Fun => fun.nam -> fun} toMap
    )
  }
  
  def expr = ident
  
  
  def toplevel = decl | expr
  
}
















