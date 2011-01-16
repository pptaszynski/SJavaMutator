package com.pp.sjm.test

import junit.framework._
import Assert._
import scala.util.parsing.combinator.token.StdTokens;
import com.pp.sjm.parser._

/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: Dec 5, 2010
 * Time: 4:11:34 PM
 * To change this template use File | Settings | File Templates.
 */

/*object JavaLexerTest {
  def suite: Test = {
      val suite = new TestSuite(classOf[JavaLexerTest]);
      suite
  }

  def main(args : Array[String]) {
      junit.textui.TestRunner.run(suite);
  }
}*/

/**
 * Unit test for simple
 */
class JavaLexerTest extends TestCase with StdTokens {
  /**
   * Test to check parsing of basic tokens.
   *
   */
  def testKeywords = {
    val lexer = new JavaLexer()
    var tokens = new lexer.Scanner("package clienteditor;\n\nimport java.beans.PropertyChangeListener;\nimport java.beans.PropertyChangeSupport;")

    var tokensList = List[lexer.Token]()

    while (!tokens.atEnd) {
      tokensList = tokens.first :: tokensList
      tokens = tokens.rest
    }
    tokensList = tokensList.reverse
    
    assert(tokensList(1) match {
      case c: lexer.Identifier => true
      case _ => false
    })
    assert(tokensList(0) match {
      case c: lexer.Keyword => true
      case _ => false
    })
  }

}