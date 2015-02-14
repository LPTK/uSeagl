package regex

//import scala.util.parsing.combinator._
//import lexical._
////import token._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

trait RegParser extends StandardTokenParsers {
  
//  import util._
  import RegexRegions._
  
  lexical.delimiters ++= List(".", "{", "}", ",", "=", ";", "(", ")", "*", "$", "|")
  lexical.reserved   ++= List("all")
  
  
//  def reg = ident | ("("?)
  def reg: Parser[Regex] = repsep(nil | cat, "|") ^^ (_ reduce Or)
  
  def cat: Parser[Regex] = repsep(term | parenRep, ".") ^^ (_ reduce Cat)
//  {x => println(x); x reduce Cat}
  
  def nil: Parser[Regex] = "$" ^^ (_ => Nil)
  
  def term: Parser[Regex] = ident ~ ("*"?) ^^ {
    case str ~ None => Sym(Symbol(str))
    case str ~ Some(_) => Rep(Sym(Symbol(str)))
  }
  
  def paren: Parser[Regex] = ("(" ~ ")" ^^ (_ => Empty)) | ("(" ~> reg <~ ")")
  def parenRep: Parser[Regex] = paren ~ ("*"?) ^^ {
    case r ~ None => r
    case r ~ Some(_) => Rep(r)
  }
  
//  def reg: Parser[Regex] = repet | field | aggr
//  
//  def aggr: Parser[DP] = "{"~>repsep(reg, "|")<~"}" ^^ {
//    case dps => Aggr(dps toSet) purge
//  }
//  def field: Parser[DP] = (term | aggr | repet) ~ ("." ~> dp?) ^^ {
//    case (te: Str) ~ rest => Field(Symbol(te), None, rest getOrElse Nil)
//    case (pre: DP) ~ rest => pre append (rest getOrElse Nil)
//  }
//  def repet: Parser[DP] = term ~ "=" ~ dp ^^ {
////    (te: Str) ~_~ body => Alias("",_=>body)
////    case te ~_~_ if te =/= te.toUpperCase => failwith("Invalid alias name")
//    case te ~_~ body => Alias(te, self => body transform((), new DefMap[Unit] with Transformer[Unit,DP] {
//      def map(a: A, dp: PDP) = dp match {
//        case Field(Id(Symbol(`te`)),k,r) =>
//          r match {
//            case Nil => Stop(self)
//            case _ => throw new RegionDefError(s"Alias '$te=$body' is ill-defined: it is not tail-recursive (remove '$r').")
//          }
//        case _ => Continue((), dp)
//      }
//    })) . sanitize purge
//  }
  
  
}


















