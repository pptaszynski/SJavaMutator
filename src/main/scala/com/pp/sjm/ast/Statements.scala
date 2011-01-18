package com.pp.sjm.ast
/**
 * @author Pawel
 * @date 2011/01/17
 */

import scala.collection.mutable.{HashSet,LinkedList}
import scala.util.parsing.input.{Positional}

import com.pp.sjm.symbols.{TypeClass,Type}

/**
 * Enumeration to classify statements.
 * Used to pattern match statements that are candidates for mutation.
 */
object StatementClass extends Enumeration {
  type StatementClass = Value
  val OTHER, RETURN, THROW, THIS, FOR, WHILE, IF, ELSE, ELSEIF = Value
  val SEQUENCE = Value
}

import StatementClass._

/**
 * A single statement
 */
trait Statement extends Node {
  /** Usual statement is not classified */
  def cat: StatementClass = OTHER
  
  def childs: Iterator[Node]
}


/**
 * Sequence of two statements.
 * Can include two simple statements or other sequences.
 */
@Deprecated
case class StatementSeq[A <: Statement ,B <: Statement](stmt1: A, stmt2: B)
  extends Statement {
	override def toString = stmt1.toString + "\n" + stmt2.toString
	
	override def cat: StatementClass = SEQUENCE
	
	def childs: Iterator[Node] = List[Node](stmt1, stmt2).iterator
}
/*object StatementSeq {
	def apply[A <: Statement,B <: Statement](stmt1: A, stmt2: B) = new StatementSeq(stmt1, stmt2)
	def unapply(n: Node) = n match {
		case n: StatementSeq => Some(n.stmt1, n.stmt2)
	}
}*/