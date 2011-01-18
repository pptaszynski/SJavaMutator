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

  case class JavaToken(str: String) extends Token with Positional {
	  override def toString = str
	  
	  def chars = this.str
  }
  
  case class CharLit(chars: String) extends Token with Positional {
    override def toString = "'" + chars + "'"
  }
  case class ArithmOp(override val chars: String) extends JavaToken(chars) with Positional
  case class RelationalOp(override val chars: String) extends JavaToken(chars) with Positional
  //case class Bracket(s: String) extends JavaToken(s) with Positional
  case class LeftBracket() extends JavaToken("[") with Positional {
	  override def chars = super.chars
  }
  case class RigthBracket() extends JavaToken("]") with Positional {
	  override def chars = super.chars
  }
  //case class Brace(s: String) extends JavaToken(s) with Positional
  case class RigthBrace() extends JavaToken("{") with Positional {
	  override def chars = super.chars
  }
  case class LeftBrace() extends JavaToken("}") with Positional {
	  override def chars = super.chars
  }
  //case class Parenthesis(s: String) extends JavaToken(s) with Positional
  case class LeftParenthesis() extends JavaToken("(") with Positional {
	  override def chars = super.chars
  }
  case class RigthParenthesis() extends JavaToken(")") with Positional {
	  override def chars = super.chars
  }
  //case class LogicalOp(override val chars: String) extends JavaToken(chars) with Positional
  case class AssignOp(override val chars: String = "=") extends JavaToken(chars) with Positional
  case class AssignArithmOp(override val chars: String) extends AssignOp(chars) with Positional
  case class BitwiseOp(override val chars: String) extends JavaToken(chars) with Positional
  case class OtherToken(override val chars: String) extends JavaToken(chars) with Positional
  case class Dot() extends JavaToken(".") with Positional {
	  override def chars = super.chars
  }
  case class Semicolon() extends JavaToken(";") with Positional {
	  override def chars = super.chars
  }
  case class Colon() extends JavaToken(":")  with Positional {
	  override def chars = super.chars
  }
  case class Comma() extends JavaToken(",")  with Positional {
	  override def chars = super.chars
  }
  case class Ellipsis() extends JavaToken("...") with Positional {
	  override def chars = super.chars
  }
  case class Wildcard() extends JavaToken("?") with Positional {
	  override def chars = super.chars
  }
  case class QuestMark() extends JavaToken("?") with Positional {
	  override def chars = super.chars
  }
  case class NumericIntLit(override val chars: String) extends NumericLit(chars) with Positional
  case class NumericFloatLit(override val chars: String) extends  NumericLit(chars) with Positional
  case class NumericDoubleLit(override val chars: String) extends  NumericLit(chars) with Positional
  case class NumericLongLit(override val chars: String) extends NumericLit(chars) with Positional
  case class OctNumericLit(override val chars: String) extends NumericLit(chars) with Positional
  case class HexNumericLit(override val chars: String) extends NumericLit(chars) with Positional
  case class JavaKeyword(override val chars: String) extends Keyword(chars) with Positional {
    override def toString = "`"+chars+"'"
  }
  case class JavaIdentifier(override val chars: String) extends Identifier(chars) with Positional {
    
  }
  case class JavaStringLit(override val chars: String) extends StringLit(chars) with Positional {
    override def toString = "identifier "+chars
  }
  
}
/** Selfless trait pattern companion object */
object JavaTokens extends JavaTokens