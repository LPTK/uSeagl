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
  
  lexical.delimiters ++= List(".", "{", "}", ",", "=", ";", ":", "(", ")", "[", "]", "|", "=>", "->", "<-", "-")
  lexical.reserved   ++= List("if", "then", "else", "fun", "typ", "nil", "new", "@read", "@inval", "@trans")
  
  def typ: Parser[ConcTyp] = "typ" ~> ident ~ (typList?) ~ (regList?) ~ (paramList?) ^^ {
    case nam ~ typs ~ regs ~ params => ConcTyp(new TUid, TId(nam),
        typs getOrElse (Seq()),
        regs getOrElse (Seq()),
        params getOrElse (Seq())
    )
  }
  
  def paramList: Parser[Seq[Local]] = "(" ~> repsep(param, ",") <~ ")"
  def typList: Parser[Seq[TId]] = "[" ~> repsep(ident, ",") <~ "]" ^^ (_ map TId.apply)
  def regList: Parser[Seq[VId]] = "{" ~> repsep(ident, ",") <~ "}" ^^ (_ map VId.apply)
  
  def param: Parser[Local] = ident ~ ((":" ~> typez)?) ^^ {
    case nam ~ t => Local(new VUid, VId(nam), t)
  }
  
  def targs = ("[" ~> repsep(typez, ",") <~ "]")
  def rargs = ("{" ~> repsep(regi, ",") <~ "}")
  
  def typez: Parser[Type] = ident ~ (targs?) ~ (rargs?) ^^ {
    case nam ~ targs ~ rargs => Type(TId(nam),
        targs getOrElse (Seq()),
        rargs getOrElse (Seq())
    )
  }
//  def ftyp: Parser[Type] = ident ~ (typList?) ~ (paramList?) ^^ {
//    case nam ~ typs ~ regs => Type(TId(nam), 
//        typs getOrElse (Seq()),
//        regs getOrElse (Seq())
//    )
//  }
  
  def fun: Parser[Fun] = "fun" ~> ident ~ (typList?) ~ (regList?) ~ (paramList?) ~ ((":" ~> typez)?) ~ (spec?) ~ ("=" ~> expr) ^^ {
    case nam ~ typs ~ regs ~ params ~ ret ~ spec ~ expr => Fun(new FUid, FId(nam),
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
        dls collect {case typ: ConcTyp => typ.nam -> typ} toMap,
        dls collect {case fun: Fun => fun.nam -> fun} toMap
    )
  }
  
  def stmt: Parser[Stmt] = bind | expr
  
//  def bind: Parser[Binding] = {(varname ~ ("=" ~> expr)) ^^ {
//    case id ~ e => Binding(id, e)
//  }}
  def bind: Parser[Binding] = {(param ~ ("=" ~> expr)) ^^ {
    case loc ~ e => Binding(loc, e)
  }}
  
//  def block: Parser[Block] = ("{" ~> repsep(stmt,";") <~ "}") ^^ {
//    case stmts => Block(stmts)
//  }
  def block: Parser[Block] = ("{" ~> rep(stmt <~ ";") ~ expr <~ "}") ^^ {
    case stmts ~ ret => Block(stmts, ret)
  }
  def parblock: Parser[Expr] = ("(" ~> expr <~ ")") | (
    "(" ~ ")" ^^ {_ => Build(Type(TId("Unit"),Seq(),Seq()),Seq())}
  )
  
  def int: Parser[Expr] = ("-"?) ~ numericLit ^^ {
    case None ~ s => IntLit(s.toInt)
    case Some(_) ~ s => IntLit(-s.toInt)
  }
  
  def build: Parser[Build] = ("new" ~> typez) ~ (("(" ~> repsep(expr,",") <~ ")")?) ^^ {
    case t ~ es => Build(t, es getOrElse Seq())
  }
  
  def bexpr: Parser[Expr] = ("nil" ^^ (_ => NilExpr)) | ite | build | int | block | parblock | fcall | (varname ^^ (Var))
  
  def ite: Parser[Ite] = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
    case c ~ t ~ e => Ite(c,t,e)
  }
  
  def fcall = ident ~ (targs?) ~ (rargs?) ~ ("(" ~> repsep(expr,",") <~ ")") ^^ {
    case id ~ ta ~ ra ~ es => FCall(FId(id), ta getOrElse Seq(), ra getOrElse Seq(), es)
  }
  
  
  def expr: Parser[Expr] = freass | faccess | bexpr
  
  
//  def faccess = expr ~ ("." ~> varname) ^^ {
//    case e ~ vid => FieldAccess(e, vid)
//  }
//  def faccess = (bexpr <~ ".") ~ repsep(varname, ".") ^^ {
  def faccess = (bexpr <~ ".") ~ varname ~ rep("." ~> varname) ^^ {
    case e ~ vid ~ vids =>
//      vids.foldRight(FieldAccess(e, vid)){ case (id,e) => FieldAccess(e, id) }
      vids.foldLeft(FieldAccess(e, vid)){ case (e,id) => FieldAccess(e, id) }
  }
  
//  def freass: Parser[FieldAssign] = (bexpr <~ ".") ~ varname ~ ("<-" ~> expr) ^^ {
//    case e ~ id ~ v => FieldAssign(e, id, v)
//  }
  def freass: Parser[FieldAssign] = (bexpr <~ ".") ~ varname ~ ("<-" ~> expr) ^^ {
    case e ~ id ~ v => FieldAssign(e, id, v)
  } | faccess ~ ("<-" ~> expr) ^^ {
    case FieldAccess(e, id) ~ v => FieldAssign(e, id, v)
  }
  
  
  def varname: Parser[VId] = ident ^^ (VId.apply)
  
  
  
  def toplevel = decl | stmt
  
  def repl = cmd | decl | stmt
  
  
  case class Command(id: Str)
  
  def cmd = ":" ~> ident ^^ Command
  
}



























