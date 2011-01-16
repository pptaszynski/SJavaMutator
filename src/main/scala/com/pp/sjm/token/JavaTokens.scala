package com.pp.sjm.token

import scala.util.parsing.combinator.token._
import scala.util.parsing.input.{Positional}
/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: 1/6/11
 * Time: 3:39 PM
 * To change this template use File | Settings | File Templates.
 */

trait JavaTokens extends StdTokens {

  type Token <: Token

  sealed abstract class JavaToken(val str: String) extends Token with Positional {
    override def chars: String = str
  }

  case class CharLit(override val str: String) extends JavaToken(str) with Positional {
    override def chars = "'" + str + "'"
  }
  case class ArithmOp(op: String) extends JavaToken(op) with Positional
  case class RelationalOp(op: String) extends JavaToken(op) with Positional
  //case class Bracket(s: String) extends JavaToken(s) with Positional
  case class LeftBracket() extends JavaToken("[") with Positional
  case class RigthBracket() extends JavaToken("]") with Positional
  //case class Brace(s: String) extends JavaToken(s) with Positional
  case class RigthBrace() extends JavaToken("{") with Positional
  case class LeftBrace() extends JavaToken("}") with Positional
  //case class Parenthesis(s: String) extends JavaToken(s) with Positional
  case class LeftParenthesis() extends JavaToken("(") with Positional
  case class RigthParenthesis() extends JavaToken(")") with Positional
  case class LogicalOp(op: String) extends JavaToken(op) with Positional
  case class AssignOp(op: String = "=") extends JavaToken(op) with Positional
  case class AssignArithmOp(override val op: String) extends AssignOp(op) with Positional
  case class BitwiseOp(op: String) extends JavaToken(op) with Positional
  case class OtherToken(override val str: String) extends JavaToken(str) with Positional
  case class Dot() extends JavaToken(".") with Positional
  case class Semicolon() extends JavaToken(";") with Positional
  case class Colon() extends JavaToken(":")  with Positional
  case class Comma() extends JavaToken(",")  with Positional
  case class Ellipsis() extends JavaToken("...") with Positional
  case class Wildcard() extends JavaToken("?") with Positional
  case class QuestMark() extends JavaToken("?") with Positional
  case class NumericIntLit(override val chars: String) extends NumericLit(chars) with Positional
  case class NumericFloatLit(override val chars: String) extends  NumericLit(chars) with Positional
  case class NumericDoubleLit(override val chars: String) extends  NumericLit(chars) with Positional
  case class NumericLongLit(override val chars: String) extends NumericLit(chars) with Positional
  case class OctNumericLit(override val chars: String) extends NumericLit(chars) with Positional
  case class HexNumericLit(override val chars: String) extends NumericLit(chars) with Positional
  case class JavaKeyword(str: String) extends Keyword(str) with Positional
  case class JavaIdentifier(str: String) extends Identifier(str) with Positional
  case class JavaStringLit(str: String) extends StringLit(str) with Positional

}