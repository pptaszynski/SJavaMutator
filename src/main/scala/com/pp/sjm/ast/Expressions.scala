/**
 * @author Pawel
 * @date 2011/01/17
 */
package com.pp.sjm.ast

import scala.collection.mutable.{HashSet,LinkedList}
import scala.util.parsing.input.{Positional,Position,NoPosition,OffsetPosition}

import com.pp.sjm.symbols.{TypeClass,Type}
import com.pp.sjm.token.JavaTokens._

trait Expression extends TypedNode {
  /**  
   * The result type of the expression
   * Will I need this?
   * If yes then it shall be recognized by parser (scoping) for leaf nodes and determined basing on
   * child nodes types for nonl-leaf nodes.
   * 
   */
  def ofType: Type
  
}

trait OpExpr extends Expression {
  /** Operator token of the expression */ 
  def op: JavaToken
  
  /** Dummy overloaded toString */
  override def toString = op.chars
}

case class SomeExpr() extends Expression {
  override def toString = "Unimportant Expression"
  override def ofType: Type = Type.VOID
  override def childs: Iterator[Node] = List[Node]().iterator
}

/**
 * Represents an use of variable identifier in an expression 
 */
class Id(n: VariableNode) extends Expression {
	/** variable bound with this node */
	def variable: VariableNode = n 
	
	/** Id node is equal to other Id node or to encapsulated VariableNode */
	override def equals(that: Any) = that match {
	  case x: Id => this.n.equals(x.variable)
	  case _ => false
	}
	
	/** Name of encapsulated variable */
	def name: String = n.id 
	
	/** Prints only the encapsulated variable */
	override def toString: String = n.toString
	
	/** Type of encapsulated variable */
	def ofType: Type = n.ofType
	
	def childs: Iterator[Node] = List[VariableNode](n).iterator
}
/** Companion injector and extractor object for class `Id` */
object Id {
	/** Injector */
	def apply(x: VariableNode) : Id = new Id(x)
	/** Extractor */
	def unapply(x: Any) : Option[VariableNode] = x match {
		case n: Id => Some(n.variable)
		case _ => None
	}
}

class ThisExpr(val rest: Id) extends Expression {
  pos = if (rest.pos != NoPosition) new OffsetPosition(rest.pos.asInstanceOf[OffsetPosition].source,
      rest.pos.asInstanceOf[OffsetPosition].offset - 5) else NoPosition

  /**
	 * Compares ThisExpr `p` to other object.
	 * It enables comparing ThisExpr `t` instance to VariableNode `v` instance. Return true if `this.rest` equals `v.name`
	 */
	override def equals(that: Any) : Boolean= that match {
		case n: VariableNode => n.name == this.rest.name && this.rest.ofType == n.ofType
		case n: ThisExpr => this.rest == n.rest && this.ofType == n.ofType
		case _ => false
	}
	
	/**
	 * Indicates whether `that` node is type compatibile with this one.
	 */
	def compatibile(that: Node) : Boolean = that match {
		case n: TypedNode => n.ofType == this.ofType
		case _ => false
	}
	
	/**
	 * Override 
	 */
	override def toString : String = "this."+rest.toString
	
	/**
	 * Return class member type.
	 * Implements TypedNode trait.
	 */
	def ofType: Type = rest.ofType
	
	def childs: Iterator[Node] = List[Node](rest).iterator
}
/** `ThisExpr' Companion extractor and injector */
object ThisExpr {
	def apply(x: Id) : ThisExpr = new ThisExpr(x)
	def unapply(x: Any) : Option[Id] = x match {
		case n: ThisExpr => Some(n.rest)
		case _ => None
	}
}

/**
 * Represents a reference to super class node.
 * Behaves the same as ThisExpr
 * 
 * @see com.pp.sjm.ast.ThisExpr
 */
class SuperExpr(val rest: String) extends Expression {
	override def toString: String = "super."+rest
	
	/** So far it has no childs. After using suffix it will get some. 
	 * TODO: Re-implement after processing suffix */
	override def childs: Iterator[Node] = List[Node]().iterator
	
	/** So far it's not possible to determine type of this thing so make it VOID :) */
	override def ofType = Type.VOID
}
object SuperExpr {
	def apply(x: String) : SuperExpr = new SuperExpr(x)
	def unapply(x: Any) : Option[String] = x match {
		case n: SuperExpr => Some(n.rest)
		case _ => None
	}
}

/**
 * Represents arithmetic, two operands epxression. It will also be used bor bitwise operations so far.
 * The type determination for the expression is not determined so far.
 */
class BinaryExpr(t: JavaToken, val e1: Expression, val e2: Expression) extends OpExpr {
  /** This is Tuple2 of lhs and rhs operands. */
  val _childs: Tuple2[Expression,Expression] = new Tuple2[Expression,Expression](e1, e2)
  
  def childs: Iterator[Node] = _childs.productIterator.asInstanceOf[Iterator[Node]]
  
  /** Operator token of the expression */
  def op: JavaToken = t
  
  /** Dummy overloaded toString */
  override def toString: String = e1.toString + op.chars + e2.toString
  
  /** 
   * Method which determines the type of the expression
   * This dummy implementation return the type of first expression. It does not care about anything more.
   */
  // TODO: Implement determination of expression type. For now - indicate type of first operator 
  def ofType: Type = e1.ofType
}
/** 
 * Companion injector and extractor object.
 * For new-less creation and pattern matching
 */
object BinaryExpr {
	def apply(t: JavaToken, x1: Expression, x2: Expression) : BinaryExpr = new BinaryExpr(t,x1,x2)
	def unapply(x: Any) : Option[(JavaToken,Expression,Expression)] = x match {
		case n: BinaryExpr => Some((n.op, n.e1, n.e2))
		case _ => None
	}
}

/**
 * Class representing node of unary expression. 
 * Will not modify them, so far, but the child expression may contain some candidates for
 * modification
 */
class UnaryExpr(t: JavaToken, val e: Expression) extends OpExpr {
	/** Child node */
	val _childs = List[Node](e)
	
	/** Implementation of Node trait */
	def childs: Iterator[Node] = _childs.iterator
	
	/** returns the type of the child expression */
	def ofType: Type = e.ofType
	
	/** Is a string */
	override def toString: String = t.toString + e.toString
	
	/** Unary operator token */
	def op: JavaToken = t
}
/** UnaryExpr injector & extractor */
object UnaryExpr {
	def apply(t: JavaToken, e: Expression) : UnaryExpr = new UnaryExpr(t, e)
	def unapply(x: Any) : Option[(JavaToken, Expression)] = x match {
		case n: UnaryExpr => Some(n.op, n.e)
		case _ => None
	}
}

class SuffixUnaryExpr(t: JavaToken, override val e: Expression) extends UnaryExpr(t, e) {
  override def toString: String = e.toString + t.toString
}
object SuffixUnaryExpr {
  def apply(t: JavaToken, e: Expression) : SuffixUnaryExpr = new SuffixUnaryExpr(t,e)
  def unapply(x: Any) : Option[(JavaToken,Expression)] = x match {
    case sue: SuffixUnaryExpr => Some(sue.op, sue.e)
    case _ => None
  }
}
/**
 * Class represents a constant, a literal in the source code. It is also for representing true | false
 * It is a leaf in AST (childs returns null !!)
 */
class Constant(t: Token) extends Expression {
	/** Always `null`. This is a leaf node so. */
	def childs = null
	
	/**
	 * determines type basing on the token `t`.
	 * Possible types are Int, Double, Float, Long, Char, String and Boolean (for true, false).
	 * Uses pattern matching to check the token type.
	 */
	def ofType: Type = t match {
		case NumericIntLit(_) => Type.INT
		case NumericDoubleLit(_) => Type.DOUBLE 
		case NumericFloatLit(_) => Type.FLOAT
		case NumericLongLit(_) => Type.LONG
		case CharLit(_) => Type.CHAR 
		case StringLit(_) => Type.CHAR
		case Keyword(x) => x match {
			case "false" | "true" => Type.BOOLEAN
			case _ => Type.VOID 
		}
		case _ => Type.VOID
	}
	
	override def toString = t.toString
	
	/** The literal`s token. */
	def token: Token = t
	
	override def equals(that: Any) = that match {
		case c: Constant => c.token == this.token
	}
}
/** `Constant` class companion extractor & injector. Also serves the Constant`s TRUE and FALSE constants. */
object Constant {
	def apply(t: Token) : Constant = new Constant(t)
	def unapply(x: Any) : Option[Token] = x match {
		case c: Constant => Some(c.token)
		case _ => None
	}
	
	/** Constants for boolean constants, use Keyword token, as JavaLexer.token returns this when tokenizing source */
	val FALSE: Constant = Constant(Keyword("false"))
	val TRUE: Constant = Constant(Keyword("true"))
	val NULL: Constant = Constant(Keyword("null"))
}

/**
 * Represents the nodes of logical AND, OR operations. These nodes are candidates for mutations.
 */
class Logical(t: JavaToken, override val e1: Expression, override val e2: Expression) extends BinaryExpr(t, e1, e2) {
	/** Logical || and && operands always give true */
	override def ofType = Type.BOOLEAN
}
/** `Logical' companion extractor & injector object */
object Logical {
	def apply(op: JavaToken, e1: Expression, e2: Expression) : Logical = new Logical(op, e1, e2)
	def unapply(x: Any) : Option[(JavaToken,Expression,Expression)] = x match {
		case l: Logical => Some(l.op, l.e1, l.e2)
		case _ => None
	}
}

/** Logical not needs different handling as it is unary logical operator.This is candidate for mutations also */
class NotExpr(t: JavaToken, override val e1: Expression) extends Logical(t,e1,e1){
	override def childs: Iterator[Node] = List[Node](e1).iterator
	
	override def toString: String = op.chars + e1.toString
}
/** `NotExpr` companion extractor & injector object */
object NotExpr {
	def apply(t: JavaToken, e1: Expression) : NotExpr = new NotExpr(t, e1)
	def unapply(x: Any) : Option[Expression] = x match {
		case n: NotExpr => Some(n.e1)
		case _ => None
	}
}
/** 
 * Relational comparision nodes.
 * They behave the same as logical, but I've distinguished them so to use pattern matching for processing by mutators.
 * Relational and Logical will be processed by different mutation operators.
 */
class Relational(t: JavaToken, e1: Expression, e2: Expression) extends Logical(t, e1, e2)
/** `Relational` companion extractor & injector object */
object Relational {
	def apply(t: JavaToken, e1: Expression, e2: Expression) : Relational = new Relational(t, e1, e2)
	def unapply(x: Any) : Option[(JavaToken, Expression, Expression)] = x match {
		case r: Relational => Some(r.op, r.e1, r.e2)
	}
}
/** Represents casted expression node */
class CastExpr(toType: Type, val e: Expression) extends Expression {
	/** It's of type it is casted to */
	def ofType = toType
	/** It has only one child */
	val _child : Tuple1[Expression] = Tuple1(e)
	/** Return the iterator to child Tuple */
	def childs : Iterator[Node] = _child.productIterator.asInstanceOf[Iterator[Node]]
	
	override def toString = "(" + ofType.toString + ")" +e.toString
}
/** `CastExpr` companion extractor & injector object */
object CastExpr {
	def apply(t: Type, e: Expression) : CastExpr = new CastExpr(t, e)
	def unapply(x: Any) : Option[(Type,Expression)] = x match {
		case ce: CastExpr => Some(ce.ofType, ce.e)
		case _ => None
	}
}

class ConditionalExpr(val cond: Expression, val trueExp: Expression, val falseExp: Expression) extends Expression {
  val _childs: Tuple3[Expression,Expression,Expression] = (cond, trueExp, falseExp)
  
  def childs: Iterator[Node] = _childs.productIterator.asInstanceOf[Iterator[Node]]
  
  def ofType = trueExp.ofType
}
object ConditionalExpr {
  def apply(cond: Expression, trueExp: Expression, falseExp: Expression) : ConditionalExpr = new ConditionalExpr(cond, trueExp, falseExp)
  def unapply(x: Any) : Option[(Expression,Expression,Expression)] = x match {
    case ce: ConditionalExpr => Some(ce.cond, ce.trueExp, ce.falseExp)
    case _ => None
  }
}

class AssignExpr(t: JavaToken, val lhs: Expression, val rhs: Expression) extends BinaryExpr(t, lhs, rhs) {
  override def ofType = rhs.ofType 
}
object AssignExpr {
  def apply(t: JavaToken, lhs: Expression, rhs: Expression) : AssignExpr = new AssignExpr(t, lhs, rhs)
  def unapply(x: Any) : Option[(JavaToken,Expression,Expression)] = x match {
    case ae: AssignExpr => Some((ae.op, ae.lhs, ae.rhs))
    case _ => None
  }
}