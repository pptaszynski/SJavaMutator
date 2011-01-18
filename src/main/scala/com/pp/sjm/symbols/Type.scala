package com.pp.sjm.symbols

/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: 1/15/11
 * Time: 2:22 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Classificator to distinct primitive and declared types.
 */
object TypeClass extends Enumeration {
  type TypeClass = Value
  val PRIMITIVE, DEFINED, META = Value
}

/**
 * Describes the type of AST Elements
 */
case class Type(var name: String, val typeClass: TypeClass.TypeClass) {

  /**
   * Equality operator.
   * @see equals
   */
  //override def ==(that: Any) = this.equals(that)

  /**
   * Compares this object to <i>that</i>.
   * Return true only if <i>that</i> is of class <i>Type</i> and the name as well as typeClass are equal.
   *
   * @return Whether to compared object is semantically equal to this.
   */
  override def equals(that: Any) = that match {
    case Type(name, typ) => this.name == name && this.typeClass == typ
    case _ => false
  }
  
  override def toString: String = name
}

/**
 * Serves primitive types instances
 */
object Type {
  lazy val INT = new Type("int", TypeClass.PRIMITIVE)

  lazy val LONG = new Type("long", TypeClass.PRIMITIVE)

  lazy val BOOLEAN = new Type("boolean", TypeClass.PRIMITIVE)

  lazy val FLOAT = new Type("float", TypeClass.PRIMITIVE)

  lazy val DOUBLE = new Type("double", TypeClass.PRIMITIVE)

  lazy val CHAR = new Type("char", TypeClass.PRIMITIVE)

  lazy val BYTE = new Type("byte", TypeClass.PRIMITIVE)

  lazy val SHORT = new Type("short", TypeClass.PRIMITIVE)
  
  lazy val VOID = new Type("void", TypeClass.PRIMITIVE)
  
  lazy val STRING = new Type ("String", TypeClass.DEFINED)
  
  lazy val CONSTRUCT = new Type("constructor", TypeClass.META)
}