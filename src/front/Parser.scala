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
  
  def paramList: Parser[Seq[Local]] = "(" ~> repsep(param, ",") <~ ")"
  def typList: Parser[Seq[TId]] = "[" ~> repsep(ident, ",") <~ "]" ^^ (_ map TId.apply)
  def regList: Parser[Seq[VId]] = "{" ~> repsep(ident, ",") <~ "}" ^^ (_ map VId.apply)
  
  def param: Parser[Local] = ident ~ (":" ~> typez) ^^ {
    case nam~t => Local(VId(nam),t)
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
  
  def fun: Parser[Fun] = "fun" ~> ident ~ (typList?) ~ (regList?) ~ (paramList?) ~ ((":" ~> typez)?) ~ (spec?) ~ ("=" ~> expr) ^^ {
    case nam ~ typs ~ regs ~ params ~ ret ~ spec ~ expr => Fun(FId(nam),
        typs getOrElse (Seq()),
        regs getOrElse (Seq()),
        params getOrElse (Seq()),
        ret,
        spec getOrElse (Spec.empty),
        expr // Cyclic(expr)
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
  
  def stmt: Parser[Stmt] = bind | expr
  
  def bind: Parser[Binding] = {(varname ~ ("=" ~> expr)) ^^ {
    case id ~ e => Binding(id, e)
  }}
  
  def block: Parser[Block] = ("{" ~> repsep(stmt,";") <~ "}") ^^ {
    case stmts => Block(stmts)
  }
  
  def expr: Parser[Expr] = block | (ident ~ expr ^^ { case id~e => FCall(FId(id),None,None,Some(Seq(e)))}) | (varname ^^ (Var))
  
  def varname: Parser[VId] = ident ^^ (VId.apply)
  
  
  
  
  
  
  def toplevel = decl | stmt
  
}



























