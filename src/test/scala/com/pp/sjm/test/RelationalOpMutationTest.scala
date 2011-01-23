package com.pp.sjm
package test
import mutation.RelationalOpMutations
import ast._
import token.JavaTokens._
import scala.util.parsing.input.{Position, OffsetPosition}
import junit.framework._

object RelationalOpMutationTest  {
  def suite: Test = {
      val suite = new TestSuite(classOf[RelationalOpMutationTest]);
      suite
  }

  def main(args : Array[String]) {
      junit.textui.TestRunner.run(suite);
  }
}

class RelationalOpMutationTest extends TestCase with RelationalOpMutations {

  def testEqualityMutation {
    val source = "  if (x != y) {"
    val shallBe = source.replaceFirst("!=", "==")
    val token = JavaToken("!=")
    token.setPos(new OffsetPosition(source, 8))
    val node = Relational(token,SomeExpr(),SomeExpr())
    
    var mut = new RelationalOpChangeMutation(new RelationalOpMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testUnequalityMutation {
    val source = "  if (x == y) {"
    val shallBe = source.replaceFirst("==", "!=")
    val token = JavaToken("==")
    token.setPos(new OffsetPosition(source, 8))
    val node = Relational(token,SomeExpr(),SomeExpr())
    
    var mut = new RelationalOpChangeMutation(new RelationalOpMutationCandidate(node))
    var result = mut(source)
    assert(result == shallBe, "Result different than expected:\nresult: " + result + "\nexpected: " + shallBe)
  }
  def testRelationalMutation {
    val source = "  if (x > y) {"
    //val shallBe = source.replaceFirst("==", "!=")
    val token = JavaToken(">")
    token.setPos(new OffsetPosition(source, 8))
    val node = Relational(token,SomeExpr(),SomeExpr())
    
    var mut = new RelationalOpChangeMutation(new RelationalOpMutationCandidate(node))
    var result = mut(source)
    assert(result != source, "Result different than expected:\nresult: " + result + "\nsource: " + source + "\n")
  }
  def testMultiline {
    val source = "  if (x > y) {\n" +
    		"    return this.member;\n" +
    		"  }\n" +
    		"  else if(x < 45) return this.member2;\n" +
    		"}\n"
    //val shallBe = source.replaceFirst("==", "!=")
    val token = JavaToken(">")
    token.setPos(new OffsetPosition(source, 8))
    val node = Relational(token,SomeExpr(),SomeExpr())
    
    var mut = new RelationalOpChangeMutation(new RelationalOpMutationCandidate(node))
    var result = mut(source)
    println()
    println(result)
    assert(result != source, "Result different than expected:\nresult: " + result + "\nsource: " + source + "\n")
  }
}