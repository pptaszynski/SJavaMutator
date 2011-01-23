/**
 * 
 */
package com.pp.sjm
package test

import mutation.MethodsMutations
import ast.{Id,ReturnStatement,ThisExpr,VariableNode}
import symbols.{Type,TypeClass}

import scala.util.parsing.input.{Position,OffsetPosition}

import junit.framework._

/**
 * @author Pawel
 *
 */
object GetterChangeMutationTest  {
  def suite: Test = {
      val suite = new TestSuite(classOf[GetterChangeMutationTest]);
      suite
  }

  def main(args : Array[String]) {
      junit.textui.TestRunner.run(suite);
  }
}

class GetterChangeMutationTest extends TestCase with MethodsMutations {
  def testChangeGetter = {
    val source = "  return fname;\n"
    val shallBe = "  return sname;\n"
    val id = Id(VariableNode("fname", Type.STRING))
    id.setPos(new OffsetPosition(source,9))
    val node = ReturnStatement(id)
    
    var mut = new ChangeGetterMutation(new ReturnValueMutationCandidate(node), List[String]("sname"))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe) 
  }
  def testChangeThisGetter = {
    val source = "  return this.fname;\n"
    val shallBe = "  return sname;\n"
    val id = Id(VariableNode("fname", Type.STRING))
    id.setPos(new OffsetPosition(source,9))
    val node = ReturnStatement(ThisExpr(id))
    
    var mut = new ChangeGetterMutation(new ReturnValueMutationCandidate(node), List[String]("sname"))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe) 
  }
  def testMethodInvocationGetter = {
    val source = "  return client.getName();\n"
    val shallBe = "  return newClient.getName();\n"
    val id = Id(VariableNode("client", Type("Client", TypeClass.DEFINED )))
    id.setPos(new OffsetPosition(source,9))
    val node = ReturnStatement(id)
    
    var mut = new ChangeGetterMutation(new ReturnValueMutationCandidate(node), List[String]("newClient"))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe) 
  }
  def testThisMethodInvocationGetter = {
    val source = "  return this.client.getName();\n"
    val shallBe = "  return newClient.getName();\n"
    val id = Id(VariableNode("client", Type("Client", TypeClass.DEFINED )))
    id.setPos(new OffsetPosition(source,9))
    val node = ReturnStatement(ThisExpr(id))
    
    var mut = new ChangeGetterMutation(new ReturnValueMutationCandidate(node), List[String]("newClient"))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe) 
  }
}