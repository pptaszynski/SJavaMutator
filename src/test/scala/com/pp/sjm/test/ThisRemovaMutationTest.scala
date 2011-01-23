/**
 * 
 */
package com.pp.sjm
package test
import mutation.ThisMutations
import ast._
import symbols._
import token.JavaTokens._
import scala.util.parsing.input.{Position, OffsetPosition}
import junit.framework._

/**
 * @author Pawel
 *
 */
object ThisRemovaMutationTest  {
  def suite: Test = {
      val suite = new TestSuite(classOf[ThisRemovaMutationTest]);
      suite
  }

  def main(args : Array[String]) {
      junit.textui.TestRunner.run(suite);
  }
}

class ThisRemovaMutationTest extends TestCase with ThisMutations {
  def testRemoveFirstThis {
    val source = "  this.name = this.fname + this.sname;\n  return this.name;\n"
    val shallBe = "  name = this.fname + this.sname;\n  return this.name;\n"
    val id = Id(VariableNode("name", Type.STRING))
    id.setPos(new OffsetPosition(source,8))
    val node = ThisExpr(id)
    
    var mut = new ThisRemovalMutation(new ThisMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testRemoveMidThis {
    val source = "  this.name = this.fname + this.sname;\n  return this.name;\n"
    val shallBe = "  this.name = fname + this.sname;\n  return this.name;\n"
    val id = Id(VariableNode("fname", Type.STRING))
    id.setPos(new OffsetPosition(source,20))
    val node = ThisExpr(id)
    
    var mut = new ThisRemovalMutation(new ThisMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testRemoveLastThis {
    val source = "  this.name = this.fname + this.sname;\n  return this.name;\n"
    val shallBe = "  this.name = this.fname + this.sname;\n  return name;\n"
    val id = Id(VariableNode("name", Type.STRING))
    id.setPos(new OffsetPosition(source,52))
    val node = ThisExpr(id)
    
    var mut = new ThisRemovalMutation(new ThisMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testRemoveThisAtBegining {
    val source = "this.name = arg1;\n"
    val shallBe = "name = arg1;\n"
    val id = Id(VariableNode("name", Type.STRING))
    id.setPos(new OffsetPosition(source,6))
    val node = ThisExpr(id)
    
    var mut = new ThisRemovalMutation(new ThisMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testRemoveThisAtEnd {
    val source = "return this.a;\n"
    val shallBe = "return a;\n"
    val id = Id(VariableNode("a", Type.STRING))
    id.setPos(new OffsetPosition(source,13))
    val node = ThisExpr(id)
    
    var mut = new ThisRemovalMutation(new ThisMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
}