package com.pp.sjm.ast

import com.pp.sjm.token.JavaTokens
import com.pp.sjm.symbols.{TypeClass,Type}

import scala.collection.mutable.{HashSet,LinkedList}
import scala.util.parsing.input.{Positional}


trait Node extends Positional {
  def childCount: Int = childs.size
  def childs: Iterator[Node]
}

trait NamedNode extends Node {
	def name: String
	
	override def equals(that: Any) : Boolean = that match {
		case n: NamedNode => this.name == n.name
		case _ => false
	}
}

trait TypedNode extends Node {
	def ofType: Type
	
	override def equals(that: Any) : Boolean = that match {
		case n: TypedNode => this.ofType == n.ofType
		case _ => false
	}
	
	def compatibile(that: Any) : Boolean = that match {
		case n: TypedNode => this.ofType == n.ofType
		case _ => false
	}
}

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
 * Some AST node.
 * Node that represents an syntax element not significant in
 */
case class SomeNode() extends Node {
  val _childs: LinkedList[Node] = new LinkedList[Node]()
  
  def childs: Iterator[Node] = _childs.iterator
}

/**
 * Represents a class declaration and definition node.
 * This is a top node of compilation unit, as package declaration, imports and annotations are skipped
 * as not significant for currently implemented mutation operators.
 */
case class ClassNode(val className: String, var mem: HashSet[Node]) extends NamedNode {
  /**
   * Name of the class whose definition statrts in this node.
   * Implementation of <i>NameElement</i> trait.
   *
   * @see scala.util.parsing.ast.AbstractSyntax.NameElement
   */
  def name = className
  
  def members: HashSet[Node] = mem
  
  //TODO: Implement methods after creating proper AST class
  def methods: HashSet[MethodNode] = members.filter(_.isInstanceOf[MethodNode]).asInstanceOf[HashSet[MethodNode]]

  //TODO: Implement fields list after implementing appropriate AST class
  def fields: HashSet[FieldNode] = members.filter(_.isInstanceOf[FieldNode]).asInstanceOf[HashSet[FieldNode]]

  def childs: Iterator[Node] = members.iterator
}

/**
 * An Variable in AST.
 * 
 * Represents the variable of name `id` and type `t`.
 * 
 * @pram id	Name of variable
 * @param t Type of variable.
 * @author Pawel
 * 
 */
case class VariableNode(val id: String, val t: Type) extends NamedNode with TypedNode {
	/** Name of the variable */
	def name = id
	
	/** Type of the variable */
	def ofType = t
	
	/**
	 * Variable Nodes are equal when have the same name and are of the same type
	 */
	override def equals(that: Any) : Boolean = that match {
		case n: VariableNode => this.name == n.name && this.ofType == n.ofType
		case _ => false
	  }
	
	/** Make variable node a leaf node */
	def childs: Iterator[Node] = List[Node]().iterator
}

/**
 * Class field node.
 * AST Class representing a field declaration inside a class body.
 */
case class FieldNode(val n:String, t:Type) extends NamedNode with TypedNode {
  def name: String = n
  /**
  * Gets the type of field represented by this node
  */
  def ofType: Type = t
  
  override def equals(that: Any) : Boolean = that match {
      case n: FieldNode => this.name == n.name && this.ofType == n.ofType
      case _ => false
    }
  
  /** Make field node a leaf node */
  def childs: Iterator[Node] = List[Node]().iterator
}

/**
 * AST Method node.
 */
case class MethodNode(n:String, retType: Type, body: BlockNode) extends NamedNode {
  /** Set of methods formal parameters */
  val parameters: HashSet[NamedNode] = HashSet[NamedNode]()
  
  def block: BlockNode = body

  override def childs: Iterator[Node] =  (parameters.toList :: List[Node](block)).asInstanceOf[List[Node]].iterator
  
  def name: String = n
}

/**
 * Block of statements node.
 * AST class representing block of statements - a method body, or a block of if, else,
 * for while, or other block construct
 */
case class BlockNode() extends Node {
  val statements: LinkedList[Statement] = new LinkedList[Statement]()
  
  def childs: Iterator[Node] = statements.asInstanceOf[LinkedList[Node]].iterator 
}

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