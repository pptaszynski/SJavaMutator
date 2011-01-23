package com.pp.sjm.parser

import com.pp.sjm.token.{JavaTokens}

import scala.collection.mutable.{HashSet}
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.{Positional}


class JavaLexer extends StdLexical with JavaTokenParsers with JavaTokens {
  
  reserved += (
    "import",
    "package"
  )
	reserved += (
    "public",
    "protected",
    "private",
    "static",
    "abstract",
    "final",
    "native",
    "synchronized",
    "transient",
    "volatile",
    "strictfp"
    )
	reserved += (
    "class",
    "interface",
    "extends",
    "implements",
    "enum"
    )
	reserved += (
    "throws",
    "void",
    "super",
    "this"
    )
	reserved += (
    "boolean",
    "char",
    "byte",
    "short",
    "int",
    "long",
    "float",
    "double"
    )
	reserved += (
    "assert",
    "if",
    "else",
    "while",
    "do",
    "switch",
    "synchronized",
    "return",
    "throw",
    "break",
    "continue",
    "case",
    "default",
    "try",
    "catch",
    "finally",
    "for",
    "goto"
	)
	reserved += (
    "instanceof",
    "new",
    "true",
    "false",
    "null"
  )

  delimiters.clear()

  override type Elem = Char
  
  override protected def processIdent(name: String) = 
    if (reserved contains name) Keyword(name) else JavaIdentifier(name)
    
  //override def token = positioned(token2)
  override def token: Parser[Token] = (
    ident                      ^^ { case ident => processIdent(ident.mkString("")) }
    | charLiteral            ^^ { case chars => CharLit(chars.toString) }
    | stringLiteral          ^^ { case string => JavaStringLit(string mkString "") }
    | floatLiteral | doubleLiteral | longLiteral | intLiteral
    | ellipsis
    | eqeq | ampamp | barbar | pluseq | subeq | stareq | slasheq | bareq | careteq  | bangeq | percenteq | ampeq
    | plusplus | subsub
    | greaterThan | lowerThan | leftParen | rigthParen  | leftBrace | rigthBrace | leftBracket | rigthBracket
    | semi | comma | dot
    | eq | bang | tilde | quest | colon | plus | sub | star | slash | amp | bar | caret | percent | monkey
    | shiftLeft | shiftRightUnsign | shiftRight
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
//  | delim
    | EofCh ^^^ EOF
    | failure("illegal character")
  )
  
//  override protected def processIdent(name: String) =
//    if (reserved contains name) JavaKeyword(name) else JavaIdentifier(name)

  override def whitespace: Parser[Any] = rep(
      whitespaceChar
    | '/' ~ '*' ~ rep(commentChars) ~ '*' ~ '/'
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    )

  override protected def comment: Parser[Any] = (
      """(.|[\r\n])*?\*/""".r
    )
  protected def commentChars: Parser[Any] = chrExcept('*') | '*' ~ guard(chrExcept('/'))

  def escapeSeq: Parser[String] = (//"""\\[btnfr\"\'\\]|\\[0-3][0-7]{2}|\\[0-7]{1,2}""".r
    "\\" ~ (
    "b" | "t" | "n" | "f" | "r" | "\"" | "'" | "\\"
    | """[0-3]""".r ~ """[0-7]""".r ~ """[0-7]""".r | """[0-7]""".r ~ """[0-7]""".r | "[0-7]".r )
  ) ^^ { case start ~ seq => start + seq.asInstanceOf[String] }
  def charLiteral: Parser[String] = ("'" ~ (unicodeChar | escapeSeq | chrExcept('\'', '\'', '\r', '\n')) ~
    "'") ^^ { case start ~ middle ~ end => middle match {
        case c: Char => c.toString
        case s: String => s
        case _ => throw new Exception("Unexpected Parser[] type")
      }
    }
  override def stringLiteral: Parser[String] = (
    "\""
    ~ rep[String](unicodeChar | escapeSeq | chrExcept('\'', '\"', '\r', '\n').asInstanceOf[Parser[String]])
    ~ "\""
  ) ^^ { case start ~ middle ~ end => start.toString + middle.mkString("") + end.toString }
  def longLiteral: Parser[NumericLongLit] =
    integerNumber ~ longSufix ^^ { case number ~ suffix => NumericLongLit(number.mkString("") + suffix.mkString("")) }
  def intLiteral: Parser[NumericIntLit] =
    integerNumber ^^ { case number => NumericIntLit(number mkString "") }
  def integerNumber: Parser[String] = (
    octNumber
    | hexPrefix ~ rep1(hexDigit) ^^ { case prefix ~ value => (prefix mkString "")  + (value mkString "") }
    | "0" | """[1-9]\d*""".r
    )
  def octNumber: Parser[String] =
    """0[0-7]+""".r
  def hexPrefix: Parser[String] =
    """0[xX]""".r
  def hexDigit: Parser[String] =
    ("""[0-9a-fA-f]""").r
  def longSufix: Parser[String] =
    "L" | "l"
  def nonIntegerNumber: Parser[String] = (
    """[0-9]+""".r ~ dot ~ """[0-9]*""".r ~ opt(exponent) ^^ {
      case int ~ dot ~ float ~ exp =>
        (int mkString "") + "." + (float mkString "") + (if (exp.isDefined) exp.get mkString "" )
      }
    |  dot ~ """[0-9]+""".r ~ opt(exponent)
        ^^ { case dot ~ number ~ exp => "." + (number mkString "") + (if (exp.isDefined) exp.get mkString "") }
    | """[0-9]+""".r ~ exponent
        ^^ { case number ~ exp => (number mkString "") + (exp mkString "") }
    | """[0-9]+""".r
        ^^ { case number => number mkString "" }

 //TODO: Implement lexing hexadecimal representation of floating point numbers
 /**
 * Consider one of two options commented below.
 */
/*
    | hexPrefix ~ rep(hexDigit) ~ dot ~ rep(hexDigit) ~ """[pP]""".r ~ """[\+\-]""".r ~ """[0-9]+""".r ^^ {
      case prefix ~ int ~ dot ~ fract ~ p ~ sign ~ exp  => {
        prefix.mkString("") + int.mkString("") + "." + fract.mkString("") + p.mkString("") + sign.mkString("") + exp.mkString("") 
      }
    }
    | hexPrefix ~ rep(hexDigit) ~ dot ~ rep(hexDigit) ~ """[pP]""".r ~ """[0-9]+""".r ^^ {
      case prefix ~ int ~ dot ~ fract ~ p ~ exp  => {
        prefix.mkString("") + int.mkString("") + "." + fract.mkString("") + p.mkString("") + exp.mkString("") 
      }
    }
    | hexPrefix ~ rep(hexDigit) ~ """[pP]""".r ~ """[\+\-]""".r ~ """[0-9]+""".r ^^ {
      case prefix ~ int ~ p ~ sign ~ exp  => {
        prefix.mkString("") + int.mkString("") + p.mkString("") + sign.mkString("") + exp.mkString("") 
      }
    }
    | hexPrefix ~ rep(hexDigit) ~ """[pP]""".r ~ """[0-9]+""".r ^^ {
      case prefix ~ int ~ p ~ exp  => {
        prefix.mkString("") + int.mkString("") + p.mkString("") + exp.mkString("")
      }
    }
*/
// Another option to consider
//    | hexPrefix ~
//        rep(hexDigit) ^^ { case digits => if (digits.isEmpty) ""
//                                           else digits.reduceLeft((a,b) => a.mkString("") + b.mkString(""))
//        } ~
//      ("" ^^ { case empty => new String("") }
//        | ("." ~ rep(hexDigit)) ^^ {
//          case dot ~ digits => "." + digits.reduceLeft((a,b) => a.mkString("") + b.mkString(""))
//        }
//      ) ~
//      """[pP]""".r ~ opt ("""[\+\-]""".r) ~ rep1("""[0-9]""".r)
//        ^^ {
//             case prefix ~ digits ~ fract ~ lit ~ sign ~ rep => {
//               prefix.mkString("") + digits.mkString("") + fract.mkString("") + lit.mkString("") +
//               (if (sign.isDefined) sign.get.mkString("") else "") + (rep mkString "")
//             }
//        }
    )
  def exponent =
    """[eE][\+\-]?[0-9]+""".r
  def floatSuffix: Parser[String] =
    "f" | "F"
  def doubleSuffix: Parser[String] =
    "d" | "D"
  def floatLiteral: Parser[NumericFloatLit] =
    nonIntegerNumber ~ floatSuffix ^^ { case number ~ suffix => NumericFloatLit(number.mkString("") + suffix.mkString("")) }
  def doubleLiteral: Parser[NumericDoubleLit] =
    nonIntegerNumber ~ opt(doubleSuffix) ^^ {
      case number ~ suffix => NumericDoubleLit(number.mkString("") + (if (suffix.isDefined) suffix.mkString("") else ""))
    }  
  def unicodeChar: Parser[String] = """\\u[a-fA-F0-9]{4}""".r
  def leftParen: Parser[LeftParenthesis] = "(" ^^^ LeftParenthesis()
  def rigthParen: Parser[RigthParenthesis] = ")" ^^^ RigthParenthesis()
  def leftBrace: Parser[LeftBrace] = "{" ^^^ LeftBrace()
  def rigthBrace: Parser[RigthBrace] = "}" ^^^ RigthBrace()
  def leftBracket: Parser[LeftBracket] = "[" ^^^ LeftBracket()
  def rigthBracket: Parser[RigthBracket] = "]" ^^^ RigthBracket()
  // ;
  def semi: Parser[Semicolon] = ";" ^^^  Semicolon()
  def comma: Parser[Comma] = "," ^^^ Comma()
  // .
  def dot: Parser[Dot] = "." ^^ { case dot => new Dot }
  def ellipsis: Parser[Ellipsis] = "..." ^^^ Ellipsis()
  def eqeq: Parser[RelationalOp] = "==" ^^^ RelationalOp("==")
  def ampamp: Parser[JavaToken] = "&&" ^^^ JavaToken("&&")
  def barbar: Parser[JavaToken] = "||" ^^^ JavaToken("||")
  def plusplus: Parser[ArithmOp] = "++" ^^^ ArithmOp("++")
  def subsub: Parser[ArithmOp] = "--" ^^^ ArithmOp("--")
  def pluseq: Parser[AssignArithmOp] = "+=" ^^^ AssignArithmOp("+=")
  def subeq: Parser[AssignArithmOp] = "-=" ^^^ AssignArithmOp("-=")
  def stareq: Parser[AssignArithmOp] = "*=" ^^^ AssignArithmOp("*=")
  def ampeq: Parser[AssignArithmOp] = "&=" ^^^ AssignArithmOp("&=")
  def slasheq: Parser[AssignArithmOp] = "/=" ^^^ AssignArithmOp("/=")
  def bareq: Parser[AssignArithmOp] = "|=" ^^^ AssignArithmOp("|=")
  def careteq: Parser[AssignArithmOp] = "^=" ^^^ AssignArithmOp("^=")
  def percenteq: Parser[AssignArithmOp] = "%=" ^^^ AssignArithmOp("%=")
  def plus: Parser[ArithmOp] = "+" ^^^ ArithmOp("+")
  def sub: Parser[ArithmOp] = "-" ^^^ ArithmOp("-")
  def star: Parser[ArithmOp] = "*" ^^^ ArithmOp("*")
  def slash: Parser[ArithmOp] = "/" ^^^ ArithmOp("/")
  def monkey: Parser[OtherToken]= "@" ^^^ OtherToken("@")
  def bangeq: Parser[RelationalOp] = "!=" ^^^ RelationalOp("!=")
  def shiftLeft: Parser[BitwiseOp] = "<<" ^^^ BitwiseOp("<<")
  def shiftRightUnsign: Parser[BitwiseOp] = ">>>" ^^^ BitwiseOp(">>>")
  def shiftRight: Parser[BitwiseOp] = ">>" ^^^ BitwiseOp(">>")
  def greaterEqThan: Parser[RelationalOp] = ">=" ^^^ RelationalOp(">=")
  def lowerEqThan: Parser[RelationalOp] = "<=" ^^^ RelationalOp("<=")
  def greaterThan: Parser[RelationalOp] = ">" ^^^ RelationalOp(">")
  def lowerThan: Parser[RelationalOp] = "<"  ^^^ RelationalOp("<")
  def bang: Parser[JavaToken] = "!" ^^^ JavaToken("!")
  def tilde: Parser[BitwiseOp] = "~" ^^^ BitwiseOp("~")
  def quest: Parser[QuestMark] = "?" ^^^ QuestMark()
  def colon: Parser[Colon] = ":"  ^^^ Colon()
  def eq: Parser[AssignOp] = "=" ^^^ AssignOp()
  def amp: Parser[BitwiseOp] = "&" ^^^ BitwiseOp("&")
  def bar: Parser[BitwiseOp] = "|" ^^^ BitwiseOp("|")
  def caret: Parser[BitwiseOp] = "^" ^^^ BitwiseOp("^")
  def percent: Parser[ArithmOp] = "%" ^^^ ArithmOp("~")
}
