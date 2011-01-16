package com.pp.sjm.test


import junit.framework._
import com.pp.sjm.parser.JavaParser
import com.pp.sjm.parser.JavaLexer
import junit.framework.Assert._
import scala.util.parsing.combinator.Parsers;

/**
 * Created by IntelliJ IDEA.
 * User: Pawel
 * Date: 1/6/11
 * Time: 3:00 PM
 * To change this template use File | Settings | File Templates.
 */

object JavaParserTests {
  def suite: Test = {
      val suite = new TestSuite(classOf[JavaParserTest]);
      suite
  }

  def main(args : Array[String]) {
      junit.textui.TestRunner.run(suite);
  }
}


class JavaParserTest extends TestCase with Parsers {
  import scala.util.parsing.input.Reader
  val parser = new JavaParser
  import parser._
 
  def testPackageDeclaration = {
   val testStr = "package com.pp.sjm;"

   parser.phrase(packageDeclaration)(new lexical.Scanner(testStr)) match {
     case parser.Success(item,_) => assertTrue(true)
     case parser.Failure(msg, _) => assertTrue(msg, false)
     case _ => fail()
   }
  }


  def testImport = {
    val testStr = "import ut.pack.tra.Class;"

    parser.phrase(importDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }
  def testAsteriskImport = {
    val testStr = "import ut.pack.tra.*;"

    parser.phrase(importDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }
  def testStaticImport = {
    val testStr = "import static ut.pack.tra.Class;"

    parser.phrase(importDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }
  def testBadImport1 = {
    val testStr = "import ut.pack.tra.;"

    parser.phrase(importDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(false)
      case parser.Failure(msg, _) => assertTrue(msg, true)
      case _ => fail()
    }
  }
  def testBadImport2 = {
    val testStr = "import package.class.Name;"

    parser.phrase(importDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(false)
      case parser.Failure(msg, _) => assertTrue(msg, true)
      case _ => fail()
    }
  }
  def testPrimitiveTypes = {
    val testBool = "boolean"
    val testShort = "short"
    val testFlot = "float"

    parser.phrase(primitiveType)(new lexical.Scanner(testBool)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }

    parser.phrase(primitiveType)(new lexical.Scanner(testShort)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }

    parser.phrase(primitiveType)(new lexical.Scanner(testFlot)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }

    parser.phrase(primitiveType)(new lexical.Scanner("int")) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }
  def testFieldDeclaration {
    var testStr = "private String firstName;"

    parser.phrase(fieldDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }

  def testMethodDeclaration {
    var testStr = "public String getSurname() {\n        return surname;\n    }"

    parser.phrase(methodDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }

  def testMethodDeclarationPropertyChangeListener {
    var testStr = "    public void addPropertyChangeListener(PropertyChangeListener listener) {\n        changeSupport.addPropertyChangeListener(listener);\n    }"

    parser.phrase(methodDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }

  def testInitializedFieldDeclaration {
    var testStr = "private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);"

    parser.phrase(fieldDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }

  def testVariableDeclaration {
    var testStr = "String oldFirstName = this.firstName;"

    parser.phrase(localVariableDeclarationStatement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }

  def testMethodInvocationMultiArg
  {
     var testStr = "changeSupport.firePropertyChange(\"maritalStatus\", oldMaritalStatus, maritalStatus);"

    parser.phrase(statement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testMethodInvocationIntArg
  {
    var testStr = "client.setAge(30);"

    parser.phrase(statement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testThisSatement
  {
    var testStr = "this.maritalStatus = maritalStatus;"

    parser.phrase(statement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testVarDelcWithThisSatementAssignment
  {
    var testStr = "int oldMaritalStatus = this.maritalStatus;"

    parser.phrase(localVariableDeclarationStatement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testReturnStatement
  {
    var testStr = "return maritalStatus;"

    parser.phrase(statement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testThrowStatement
  {
    var testStr = "throw new ArrayIndexOutOfBoudException();"

    parser.phrase(statement)(new lexical.Scanner(testStr)) match {
      case parser.Success(item, _) => assertTrue(true)
      case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
        next.toString() + "\n")
      case _ => fail()
    }
  }
  def testSystemExitMinusOne
  {
    var testStr = "System.exit(-1);"

    parser.phrase(blockStatement)(new lexical.Scanner(testStr)) match {
            case parser.Success(item, _) => assertTrue(true)
    case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
      next.toString() + "\n")
    case _ => fail()
    }
  }
  def testEmptyIf
  {
    var testStr = "if (_homeImpl != null) {\n      }\n"

    blockStatementTest(testStr)
  }
  def testEmptyIfElseIf
  {
    var testStr = "if (_homeImpl != null) {\n      }\n      else if (_objectImpl != null)\n	_objectAPI = _objectImpl.getClass();"

    blockStatementTest(testStr)
  }
  def test_IfElse
  {
    var testStr = "if (_homeImpl != null) { \n }\n else {	if (getClass().equals(HessianServlet.class))\n	  throw new ServletException(\" server must extend HessianServlet \");\n _homeImpl = this;\n}\n"

    parser.phrase(blockStatement)(new lexical.Scanner(testStr)) match {
            case parser.Success(item, _) => assertTrue(true)
    case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
      next.toString() + "\n")
    case _ => fail()
    }

  }
  def test_Cast_with_Member_Access
  {
    var testStr = "((Service) service).init(getServletConfig());"

    blockStatementTest(testStr)
  }
  def test_MethodDeclaration_with_throws
  {
    var testStr = "public void service(ServletRequest request, ServletResponse response)\n    throws IOException, ServletException\n  {\n   return;\n    }"

    parser.phrase(methodDeclaration)(new lexical.Scanner(testStr)) match {
      case parser.Success(item,_) => assertTrue(true)
      case parser.Failure(msg, _) => assertTrue(msg, false)
      case _ => fail()
    }
  }
  def test_TryCatches {
    var testStr = ("try {\n" +
      "if (_homeImpl != null) {\n" +
      "}\n" +
      "else {\n	" +
      "  if (getClass().equals(HessianServlet.class))\n" +
      "	   throw new ServletException(\" server must extend HessianServlet \");\n" +
      "\n_homeImpl = this;\n" +
      "}\n\n" +
      "} catch (ServletException e) {\n" +
      "      throw e;\n" +
      "} catch (Exception e) {\n" +
      "    throw new ServletException(e);\n" +
      "}\n")

    blockStatementTest(testStr)
  }
  def test_Identifier = {
    def testStr = "Nazwa"

    parser.phrase(identifier)
  }
  private def blockStatementTest(statement: String) {
    parser.phrase(blockStatement)(new lexical.Scanner(statement)) match {
            case parser.Success(item, _) => assertTrue(true)
    case parser.Failure(msg, next) => fail("\nMessage: " + msg + "\nNext input: " + next.pos + " " +
      next.toString() + "\n")
    case _ => fail()
    }
  }

}