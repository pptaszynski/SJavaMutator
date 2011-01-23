package com.pp.sjm.ast
/**
 * @author Pawel
 * @date 2011/01/17
 */

import scala.collection.mutable.{HashSet,LinkedList}
import scala.collection.immutable.{List}
import scala.util.parsing.input.{Positional}

import com.pp.sjm.symbols.{TypeClass,Type}

/**
 * Enumeration to classify statements.
 * Used to pattern match statements that are candidates for mutation.
 */
object StatementClass extends Enumeration {
  type StatementClass = Value
  val OTHER, RETURN, THROW, THIS, FOR, WHILE, IF, ELSE, ELSEIF = Value
  val TRYCATCH,SEQUENCE = Value
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

/** Stub for not considered statements */
class SomeStatement extends Statement {
  def childs: Iterator[Node] = List[Node]().iterator
  override def toString = "SomeStatement()"
}
/** Companion extractor and injector for SomeStatement stub */
object SomeStatement {
  def apply(): SomeStatement = new SomeStatement()
  def unapply(x: Any) : Option[Any] = x match {
    case s: SomeStatement => Some[Any]()
    case _ => None
  }
}

/**
 * Return statement stub - for the purpose of getters mutation 
 */
class ReturnStatement extends Statement with TypedNode {
  protected var e: Expression = null
  
  private var _child = List[Node]()
  
  def this(e: Expression) = {
    this()
    this.e = e
    _child = List[Node](this.e)
    pos = e.pos
  }
  
  override def cat = RETURN
  
  override def childs: Iterator[Node] = _child.iterator
  
  override def ofType = e.ofType
  
  override def toString = "return " + eToString + ";"
  
  def eToString = if (e != null) e.toString else ""
}
/**
 * Companion extractor object for ReturnStatement
 */
object ReturnStatement {
  def apply() = new ReturnStatement()
  def apply(e: Expression) = new ReturnStatement(e)
  def unapply(x: Any) : Option[Expression] = x match {
    case rs: ReturnStatement => Some(rs.e)
    case _ => None
  }
}

/** 
 * Throw statement node
 * Used to recognize candidate statements to be removed
 */
class ThrowStatement extends Statement {
  protected var e: Expression = null 
  
  private var _child = List[Node]()
  
  def this(e: Expression) = {
    this()
    this.e = e
    this._child = e :: this._child
  }
  
  override def childs : Iterator[Node] = _child.iterator
  
  override def cat = THROW 
  
  override def toString = "throw " + e.toString
}
/** Companion extractor object for ThrowStatement */
object ThrowStatement {
  def apply() = new ThrowStatement()
  def apply(e: Expression) = new ThrowStatement(e)
  def unapply(x: Any) : Option[Expression] = x match {
    case ts: ThrowStatement => Some(ts.e)
    case _ => None
  }
}

/**
 * If Else statement
 * Needs to be analyzed as a lot of code is placed there
 * 
 * @param cond The conditional expression of if()
 * @param block1 The statement or block to be performed on cond = true
 */
class IfElseStmt(val cond: Expression, val block1: Node) extends Statement {
  var block2: Node = null
  
  var _childs: List[Node] = List[Node](cond,block1,block2)
  
  /**
   * 
   * @param cond The conditional expression of if statement
   * @param block1 the block if true
   * @param block2 the block if false
   */
  def this(cond: Expression, block1: Node, block2: Node) = {
    this(cond, block1)
    this.block2 = block2
    this._childs = List[Node](cond,block1,block2)
  }
  
  override def cat = IF
  
  override def childs = _childs.iterator
  
  override def toString = "if (" + cond.toString + ") {\n" + block1.toString + "\n}" + elsePartString
   
      
  private def elsePartString: String =  if (block2 != null) " else {\n" + block2.toString + "\n}\n" else "\n"
}
/** IfElseStmt companion extractor object */
object IfElseStmt {
  /**
   * 
   * @param cond The conditional expression of if()
   * @param block1 The statement or block to be performed on cond = true
   */
  def apply(cond: Expression, block1: Node) = new IfElseStmt(cond, block1)
  /**
   * 
   * @param cond The conditional expression of if statement
   * @param block1 the block if true
   * @param block2 the block if false
   */
  def apply(cond: Expression, block1: Node, block2: Node) = new IfElseStmt(cond, block1, block2)
  def unapply(x: Any) : Option[(Expression,Node,Node)] = x match {
    case ies: IfElseStmt => Some(ies.cond, ies.block1, ies.block2)
    case _ => None
  }
}

class TryCatchStmt(val tryBlock: BlockNode, val catches: List[BlockNode] = List[BlockNode](), val finalBlock: BlockNode = BlockNode()) extends Statement {
  //if (catches == null) catches = List[BlockNode]()
  //if (finalBlock == null) finalBlock = BlockNode()
  var _childs = tryBlock :: (finalBlock :: catches)
  
  override def childs = _childs.iterator
  
  override def cat = TRYCATCH
  
  override def toString = " try " + tryBlock.toString + " " + catchesToString + "final " + finalBlock.toString
 
  private def catchesToString = {
    var str = ""
    for (c <- catches) {
      str += " catch "
      str += c.toString
    }
    str
  }
}
object TryCatchStmt {
  def apply(tryBlock: BlockNode, catches: List[BlockNode], finalBlock: BlockNode) = {
    var c: List[BlockNode] = catches
    var fb: BlockNode = finalBlock
    if (catches == null) c = List[BlockNode]()
    if (finalBlock == null) fb = BlockNode()
    
    new TryCatchStmt(tryBlock, catches, finalBlock)
  }
  def unapply(x: Any) : Option[(BlockNode,List[BlockNode],BlockNode)] = x match {
    case tcs: TryCatchStmt => Some(tcs.tryBlock, tcs.catches, tcs.finalBlock)
    case _ => None
  }
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