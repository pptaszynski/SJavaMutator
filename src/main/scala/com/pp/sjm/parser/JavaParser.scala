package com.pp.sjm
package parser

import ast._
import symbols.{Type,TypeClass,Scoping}
import token.{JavaTokens => tokens}

import scala.util.parsing.combinator.{PackratParsers}
import scala.util.parsing.input.{Positional}
import scala.util.parsing.combinator.syntactical.{StdTokenParsers, StandardTokenParsers}
import scala.util.parsing.combinator.token.{StdTokens}
import scala.collection.mutable.{HashSet,LinkedList}

/**
 * @author Pawel
 *
 */
class JavaParser extends StandardTokenParsers with Scoping {  
  override type Tokens <: StdTokens
  override val lexical: JavaLexer = new JavaLexer
  
  import lexical.{Identifier,Dot,Semicolon,ArithmOp,JavaToken,BitwiseOp,RelationalOp,QuestMark,CharLit}
  import lexical.{LeftBracket,RigthBracket,LeftBrace,RigthBrace,LeftParenthesis}
  import lexical.{RigthParenthesis,AssignOp,AssignArithmOp,OtherToken,Colon,Comma,Ellipsis,Wildcard,NumericLongLit}
  import lexical.{NumericDoubleLit,NumericFloatLit,NumericIntLit,NumericLit,OctNumericLit,HexNumericLit,StringLit}
  import lexical.{JavaStringLit, Keyword, JavaIdentifier}
  import lexical.Scanner
  import MutationKind._ 
  
  def scoped[T](p: => Parser[T]) : Parser[T] = {
    var env = Env getNew;
    val pars = Parser { in => 
      p(in) match {
        case Success(t, in1) => { Env reduce; Success(t, in1) }
        case ns: NoSuccess => { Env reduce; Env remove env; ns }
      }
    }
    pars
  }
  
  implicit def accept(e: JavaToken) : Parser[JavaToken] = positioned(acceptIf(_ == e)("`"+e+"' expected but " + _ + " found") ^^ {case e => e.asInstanceOf[JavaToken]})
//  override implicit def accept(e: Elem): Parser[Elem] = e match {
//    case e: JavaToken => positioned(acceptIf(_ == e.JavaToken)("`"+e+"' expected but " + _ + " found"))
//    case e: Elem => acceptIf(_ == e)("`"+e+"' expected but " + _ + " found")
//  }
  
  /**
   * Start symbol of the parser
   */
  def program: Parser[List[Node]] = compilationUnit

  /**
   * compilationUnit
   * :   (   (annotations
   *         )?
   *         packageDeclaration
   *     )?
   *     (importDeclaration
   *     )*
   *     (typeDeclaration
   *     )*
   * ;
   */
  def compilationUnit: Parser[List[Node]] = opt(opt(annotations) ~ packageDeclaration) ~> rep(importDeclaration) ~> rep(typeDeclaration)

  /**
   * packageDeclaration
   * :   'package' qualifiedName
   *     ';'
   * ;
   */
  def packageDeclaration = keyword("package") ~> qualifiedName <~ Semicolon()

  /**
   *  importDeclaration
   * :   'import'
   *     ('static'
   *     )?
   *     IDENTIFIER '.' '*'
   *     ';'
   * |   'import'
   *     ('static'
   *     )?
   *     IDENTIFIER
   *     ('.' IDENTIFIER
   *     )+
   *     ('.' '*'
   *     )?
   *     ';'
   * ;
   */
  def importDeclaration = (
    keyword("import") ~> opt(keyword("static")) ~ qualifiedName ~ accept(Dot()) ~ accept(ArithmOp("*"))
    | keyword("import") ~> opt(keyword("static")) ~ qualifiedName
    ) ~ Semicolon()

  /*
  Names
   */
  def qualifiedName = rep1sep(identifier, Dot()) ^^ { case list => list.mkString(".")}
  def identifier: Parser[JavaIdentifier] = positioned(elem("identifier", _.isInstanceOf[JavaIdentifier]) ^^ ((x: Elem) => JavaIdentifier(x.chars)))
  /**
   * Types
   */
  /**
   * type
   * :   classOrInterfaceType
   *     ('[' ']'
   *     )*
   * |   primitiveType
   *     ('[' ']'
   *     )*
   * ;
   */
  def typeExpr: Parser[Type] = (
    classOrInterfaceType <~ rep(LeftBracket() <~ RigthBracket())
    |
    primitiveType <~ rep(LeftBracket() <~ RigthBracket())
    )

  /**
   * classOrInterfaceType
   * :   IDENTIFIER
   *     (typeArguments
   *     )?
   *     ('.' IDENTIFIER
   *         (typeArguments
   *         )?
   *     )*
   * ;
   */
  def classOrInterfaceType: Parser[Type] = rep1sep(identifier ~ opt(typeArguments),Dot()) ^^ {
	  case parts => {
	 	  var name: StringBuilder = new StringBuilder 
	 	  for (p <- parts) p match {
	 	 	  case id ~ typeArgs => {
	 	 	 	  name.append(id.chars)
	 	 	 	  if (typeArgs.isDefined) name.append(typeArgs.get)
	 	 	 	  if (p != parts.last) name.append(".")
	 	 	  }
	 	  }
	 	  Type(name.toString, TypeClass.DEFINED)
	  }
    }

  /**
   * typeArguments
   * :   '<' typeArgument
   *     (',' typeArgument
   *     )*
   *     '>'
   * ;
   * 
   * Return it as string as we don't process it. Just compare string representations to check equality
   */
  def typeArguments: Parser[String] = RelationalOp("<") ~> rep1sep(typeArgument, Comma()) <~ RelationalOp(">") ^^ {
      case args => {
	 	  var str: StringBuilder = new StringBuilder()
	 	  str.append("<")
	 	  str.append(args.mkString(", "))
	 	  str.append(">")
	 	  str.toString
	  }
	}
    
  

  /**
   *   typeArgument
   * :   type
   * |   '?'
   *     (
   *         ('extends'
   *         |'super'
   *         )
   *         type
   *     )?
   * ;
   */
  def typeArgument: Parser[String] = (
    typeExpr ^^ { case typeNode => typeNode.toString }
    | QuestMark() ~> keyword("extends") ~> typeExpr ^^ { case typeExpr => "? extends " + typeExpr.toString }  
    | QuestMark() ~> keyword("super") ~> typeExpr ^^ { case typeExpr => "? super " + typeExpr.toString }
    | QuestMark() ^^^ "?"
    )

  /**
   * typeList
    :   type
        (',' type
        )*
    ;
   */
  def typeList = rep1sep(typeExpr, Comma())

  /**
   *    primitiveType
   * :   'boolean'
   * |   'char'
   * |   'byte'
   * |   'short'
   * |   'int'
   * |   'long'
   * |   'float'
   * |   'double'
   * ;
   */
  def primitiveType: Parser[Type] = (
    keyword("boolean") ^^^ Type.BOOLEAN
    | keyword("char") ^^^ Type.CHAR
    | keyword("byte") ^^^ Type.BYTE
    | keyword("short") ^^^ Type.SHORT 
    | keyword("int") ^^^ Type.INT
    | keyword("long") ^^^ Type.LONG
    | keyword("float") ^^^ Type.FLOAT
    | keyword("double") ^^^ Type.DOUBLE
    )

  /**
   * typeDeclaration
   * :   classOrInterfaceDeclaration
   * |   ';'
   * ;
   */
  def typeDeclaration: Parser[Node] = classOrInterfaceDeclaration | Semicolon() ^^^ SomeNode()

  /**
   * classOrInterfaceDeclaration
   * :    classDeclaration
   * |   interfaceDeclaration
   * ;
   */
  def classOrInterfaceDeclaration: Parser[Node] = (
    classDeclaration 
  | interfaceDeclaration ^^^ SomeNode()
  )

  /**
   *  classBody
   * :   '{'
   *     (classBodyDeclaration
   *     )*
   *     '}'
   * ;
   */
  def classBody: Parser[HashSet[Node]] = LeftBrace() ~> rep(classBodyDeclaration) <~ RigthBrace() ^^ {
      case membersLists => {
    	var members: HashSet[Node] = new HashSet[Node]()
        for(list <- membersLists) {
          members ++= list
        }
    	members.filter(x => x.isInstanceOf[ClassNode] || x.isInstanceOf[FieldNode] || 
    	  x.isInstanceOf[MethodNode])
      }
    }

  /**
   * interfaceBody
   * :   '{'
   *     (interfaceBodyDeclaration
   *     )*
   *     '}'
   * ;
   */
  def interfaceBody = LeftBrace() ~> rep(interfaceBodyDeclaration) <~ RigthBrace()

  /**
   * classBodyDeclaration
   * :   ';'
   * |   ('static'
   *     )?
   *     block
   * |   memberDecl
   * ;
   */
  def classBodyDeclaration: Parser[List[Node]] = (
    Semicolon() ^^^ List[Node](SomeNode())
    | opt(keyword("static")) ~> block ^^ { case block => List[Node](BlockNode()) } // TODO: Implement returning childs of this node
    | memberDecl
    )

  /**
   * memberDecl
   * :    fieldDeclaration
   * |    methodDeclaration
   * |    classDeclaration
   * |    interfaceDeclaration
   * ;
   */
  def memberDecl : Parser[List[Node]] = (
    fieldDeclaration
    | scoped(methodDeclaration) ^^ { case method => List[Node](method) }
    | classDeclaration ^^ { // Care only about internal classess - there may be some opprotunity do mutate.
        case c: ClassNode => List[Node](c)
        case _ => List[Node](SomeNode())
      }
    | interfaceDeclaration ^^^ List[Node](SomeNode()) //Don't care about internal interface
    )

  /**
   * methodDeclaration
   * :
   *    
   *      modifiers
   *     (typeParameters
   *     )?
   *     IDENTIFIER
   *     formalParameters
   *     ('throws' qualifiedNameList
   *     )?
   *     '{'
   *     (explicitConstructorInvocation
   *     )?
   *     (blockStatement
   *     )*
   *     '}'
   * |   modifiers
   *     (typeParameters
   *     )?
   *     (type
   *     |   'void'
   *     )
   *     IDENTIFIER
   *     formalParameters
   *     ('[' ']'
   *     )*
   *     ('throws' qualifiedNameList
   *     )?
   *     (
   *         block
   *     |   ';'
   *     )
   * ;
   */
  def methodDeclaration: Parser[MethodNode] = (
    /* Constructors */
    modifiers ~> opt(typeParameters) ~> identifier ~ formalParameters ~ (opt(keyword("throws") ~> qualifiedNameList)
      ~> LeftBrace() ~> opt(explicitConstructorInvocation) ~> rep(blockStatement) <~ RigthBrace()) ^^ {
      	case id ~ args ~ block => MethodNode(id.chars, Type.CONSTRUCT, BlockNode(block.foldLeft(List[Node]())(_ ::: _)))
      }
    |
    /* member methods */
    modifiers ~> opt(typeParameters) ~> (typeExpr | keyword("void") ^^^ Type.VOID) ~ identifier ~ formalParameters
      ~ (rep(LeftBracket() ~ RigthBracket()) ~> opt(keyword("throws") ~> qualifiedNameList) ~> (block | Semicolon() ^^^ BlockNode())) ^^ {
    	// TODO: Implement serious dealing with parameters and block
        case t ~ id ~ args ~ block => MethodNode(id.chars, t, block)
      }
    )

  /**
   * fieldDeclaration
   * :   modifiers
   *     type
   *     variableDeclarator
   *     (',' variableDeclarator
   *     )*
   *     ';'
   * ;
   * 
   * Initially don't care about modifiers. Just read type and names
   *
   */
  // TODO: Read modifiers if going to implement some access mutators
  // TODO: Read initializers of variables in case of dealing with initializers or constructors someday
  def fieldDeclaration: Parser[List[FieldNode]] = modifiers ~> typeExpr ~ rep1sep(variableDeclarator, Comma()) <~ Semicolon() ^^ {
	  case typ ~ names => {
  	 	var fields: List[FieldNode] = List[FieldNode]()
  	 	// names is a collection of Tuple2 where _1 is the name and _2 is dimension
  	 	for(name <- names) {
  	 		var t = typ
  	 		if (name._2 > 0) for (i <- 0 until name._2) t.name += "[]"
  	 		var field = FieldNode(name._1.chars, t)
  	 		Env mount field
  	 		fields = field :: fields
  	 	}
  	 	
  	 	fields.reverse
	  }
  }

  /**
   * variableDeclarator
   * :   IDENTIFIER
   *     ('[' ']'
   *     )*
   *     ('=' variableInitializer
   *     )?
   * ;
   * 
   * Return the parser of Tuple containing variable`s identifier at _1 and number of dimenssions at _2
   */
  def variableDeclarator: Parser[Tuple2[Elem,Int]] = identifier ~ rep(LeftBracket() ~ RigthBracket()) <~ opt(AssignOp() <~ variableInitializer) ^^ {
	  case id ~ brackets => Tuple2(id, brackets.length)    
    }

  /**
   * interfaceBodyDeclaration
   * :
   *     interfaceFieldDeclaration
   * |   interfaceMethodDeclaration
   * |   interfaceDeclaration
   * |   classDeclaration
   * |   ';'
   * ;
   */
  def interfaceBodyDeclaration = (
    interfaceFieldDeclaration
    | interfaceMethodDeclaration
    | interfaceDeclaration
    | classDeclaration
    | Semicolon()
    )

  /**
   * interfaceMethodDeclaration
   * :   modifiers
   *     (typeParameters
   *     )?
   *     (type
   *     |'void'
   *     )
   *     IDENTIFIER
   *     formalParameters
   *     ('[' ']'
   *     )*
   *     ('throws' qualifiedNameList
   *     )? ';'
   * ;
   */
  def interfaceMethodDeclaration = (
      modifiers ~ opt(typeParameters) ~ (typeExpr | keyword("void")) ~ identifier ~ formalParameters
        ~ rep(LeftBracket() ~ RigthBracket()) ~ opt(keyword("throws") ~ qualifiedNameList) <~ Semicolon()
    )

  /**
   * interfaceFieldDeclaration
   * :   modifiers type variableDeclarator
   *     (',' variableDeclarator
   *     )*
   *     ';'
   * ;
   */
  def interfaceFieldDeclaration = modifiers ~ typeExpr ~ variableDeclarator ~ repsep(variableDeclarator, Comma()) ~ Semicolon();

  /**
   *
   */
  /**
   * modifiers
   * :
   * (    annotation
   * |   'public'
   * |   'protected'
   * |   'private'
   * |   'static'
   * |   'abstract'
   * |   'final'
   * |   'native'
   * |   'synchronized'
   * |   'transient'
   * |   'volatile'
   * |   'strictfp'
   * )*
   * ;
   */
  def modifiers = rep(annotation | keyword("public") | keyword("protected") | keyword("private") | keyword("static") |
    keyword("abstract") | keyword("final") | keyword("native") | keyword("synchronized") | keyword("volatile") |
    keyword("strictfp") )

  /**
   * variableModifiers
   * :   (   'final'
   *     |   annotation
   *     )*
   * ;
   */
  def variableModifiers = rep(keyword("final") | annotation)

  /**
   * classDeclaration
   * :   normalClassDeclaration
   * |   enumDeclaration
   * ;
   */
  def classDeclaration : Parser[Node] = (
    scoped(normalClassDeclaration) 
  | enumDeclaration ^^^ SomeNode()
  )

  /**
   * normalClassDeclaration
   * :   modifiers  'class' IDENTIFIER
   *     (typeParameters
   *     )?
   *     ('extends' type
   *     )?
   *     ('implements' typeList
   *     )?
   *     classBody
   * ;
   */
  def normalClassDeclaration: Parser[ClassNode] = modifiers ~> keyword("class") ~> identifier ~ (opt(typeParameters) ~>
    opt(keyword("extends") ~ typeExpr) ~> opt(keyword("implements") ~ typeList) ~> classBody) ^^ {
      case name ~ body => {
    	  val members = new HashSet[Node]()
    	  members ++= body
    	  ClassNode(name.chars, members)
      }
  }
  
  /**
   * typeParameters
   * :   '<'
   *         typeParameter
   *         (',' typeParameter
   *         )*
   *     '>'
   * ;
   */
  def typeParameters = RelationalOp("<") ~> rep1sep(typeParameter, Comma()) <~ RelationalOp(">")

  /**
   * typeParameter
   * :   IDENTIFIER
   *     ('extends' typeBound
   *     )?
   * ;
   */
  def typeParameter = identifier ~ opt(keyword("extends") ~ typeBound)

  /**
   * typeBound
   * :   type
   *     ('&' type
   *     )*
   * ;
   */
  def typeBound = typeExpr ~ rep(BitwiseOp("&") ~ typeExpr)

  /**
   * enumDeclaration
   * :   modifiers
   *     ('enum'
   *     )
   *     IDENTIFIER
   *     ('implements' typeList
   *     )?
   *     enumBody
   * ;
   */
  def enumDeclaration = modifiers ~ keyword("enum") ~ identifier ~ opt(keyword("implements") ~ typeList) ~ enumBody

  /**
   * enumBody
   * :   '{'
   *     (enumConstants
   *     )?
   *     ','?
   *     (enumBodyDeclarations
   *     )?
   *     '}'
   * ;
   */
  def enumBody = LeftBrace() ~ opt(enumConstants) ~ opt(Comma()) ~ opt(enumBodyDeclarations) ~ RigthBrace()

  /**
   * enumConstants
   * :   enumConstant
   *     (',' enumConstant
   *     )*
   * ;
   */
  def enumConstants : Parser[Any] = rep1sep(enumConstants,Comma())

  /**
   * enumConstant
    :   (annotations
        )?
        IDENTIFIER
        (arguments
        )?
        (classBody
        )?
        /* $GScope::name = names.empty. enum constant body is actually
        an anonymous class, where constructor isn't allowed, have to add this check*/
    ;
   */
  def enumConstant = opt(annotations) ~ identifier ~ opt(arguments) ~ opt(classBody)

  /**
   * enumBodyDeclarations
   * :   ';'
   *     (classBodyDeclaration
   *     )*
   * ;
   */
  def enumBodyDeclarations = Semicolon() ~ rep(classBodyDeclaration)

  /**
   * interfaceDeclaration
   * :   normalInterfaceDeclaration
   * |   annotationTypeDeclaration
   * ;
   */
  def interfaceDeclaration = normalInterfaceDeclaration | annotationTypeDeclaration

  /**
   * normalInterfaceDeclaration
   * :   modifiers 'interface' IDENTIFIER
   *     (typeParameters
   *     )?
   *     ('extends' typeList
   *     )?
   *     interfaceBody
   * ;
   */
  def normalInterfaceDeclaration: Parser[Any] = modifiers ~ keyword("interface") ~ identifier ~ opt(typeParameters) ~
    opt(keyword("extends") ~ typeList) ~ interfaceBody

  /**
   * qualifiedNameList
   * :   qualifiedName
   *     (',' qualifiedName
   *     )*
   * ;
   */
  def qualifiedNameList = rep1sep(qualifiedName, Comma())

  /**
   * formalParameters
    :   '('
        (formalParameterDecls
        )?
        ')'
    ;
   */
  def formalParameters = LeftParenthesis() ~> opt(formalParameterDecls) <~ RigthParenthesis()

  /**
   * formalParameterDecls
   * :   ellipsisParameterDecl
   * |   normalParameterDecl
   *     (',' normalParameterDecl
   *     )*
   * |   (normalParameterDecl
   *     ','
   *     )+
   *     ellipsisParameterDecl
   * ;
   */
  def formalParameterDecls: Parser[List[VariableNode]] = (
    ellipsisParameterDecl ^^ { case vn => List[VariableNode](vn) }
    | rep1sep(normalParameterDecl, Comma())
    | rep1sep(normalParameterDecl, Comma()) ~ (Comma() ~> ellipsisParameterDecl) ^^ { case vn ~ el => vn :+ el }  
    )

  /**
   * normalParameterDecl
   * :   variableModifiers type IDENTIFIER
   *     ('[' ']'
   *     )*
   * ;
   */ 
  def normalParameterDecl: Parser[VariableNode] = variableModifiers ~> typeExpr ~ identifier ~ rep(LeftBracket() ~ RigthBracket()) ^^ {
    case typ ~ name ~ arr => {
      var t = typ
      if (!(arr isEmpty)) for (i <- 0 until arr.length) t.name += "[]"
      var vn = VariableNode(name.chars, t)
      if (Env.current isDefined) Env.current.get.mount(vn.asInstanceOf[ScopedNode])
      vn
    }
  } 

  /**
   * ellipsisParameterDecl
   * :   variableModifiers
   *     type  '...'
   *     IDENTIFIER
   * ;
   */  def ellipsisParameterDecl: Parser[VariableNode] = variableModifiers ~ (typeExpr <~ Ellipsis() ) ~ identifier ^^ {
    case mod ~ expr ~ id => {
      var vn = VariableNode(id.chars, Type(expr.name + "...", TypeClass.ITERABLE))
      if (Env.current isDefined) Env.current.get.mount(vn.asInstanceOf[ScopedNode])
      vn
    }
  }

  /**
   * explicitConstructorInvocation
   * :   (nonWildcardTypeArguments
   *     )?     //NOTE: the position of Identifier 'super' is set to the type args position here
   *     ('this'
   *     |'super'
   *     )
   *     arguments ';'
   *
   * |   primary
   *     '.'
   *     (nonWildcardTypeArguments
   *     )?
   *     'super'
   *     arguments ';'
   * ;
   */
  def explicitConstructorInvocation = (
      opt(nonWildcardTypeArguments) ~ (keyword("this") | keyword("super")) ~ arguments ~ Semicolon()
    | primary ~ Dot() ~ opt(nonWildcardTypeArguments) ~ keyword("super") ~ arguments ~ Semicolon()
    )

// ****************************************************************************
//   Statements
// ****************************************************************************

  /**
   * block
   * :   '{'
   *     (blockStatement
   *     )*
   *     '}'
   * ;
   */
  def block: Parser[BlockNode] =  scoped(LeftBrace() ~> rep(blockStatement) <~ RigthBrace() ^^ {
    case stmts => BlockNode(stmts.foldLeft(List[Node]())( _ ::: _))
  })
  /**
   * blockStatement
   * :   localVariableDeclarationStatement
   * |   classOrInterfaceDeclaration
   * |   statement
   * ;
   */
  def blockStatement: Parser[List[Node]] = (
      localVariableDeclarationStatement
    | classOrInterfaceDeclaration ^^ { case decl => List[Node](decl) }
    | statement ^^ { case stmt => List[Node](stmt) }
    )

  /**
   * localVariableDeclarationStatement
   * :   localVariableDeclaration
   *     ';'
   * ;
   */
   def localVariableDeclarationStatement: Parser[List[VariableNode]] = localVariableDeclaration <~ Semicolon()

  /**
   * localVariableDeclaration
   * :   variableModifiers type
   *     variableDeclarator
   *     (',' variableDeclarator
   *     )*
   * ;
   */
  def localVariableDeclaration: Parser[List[VariableNode]] = variableModifiers ~> typeExpr ~ rep1sep(variableDeclarator,Comma() ) ^^ {
      case typ ~ names => {
        var fields: List[VariableNode] = List[VariableNode]()
        // names is a collection of Tuple2 where _1 is the name and _2 is dimension
        for(name <- names) {
          var t = typ
          if (name._2 > 0) for (i <- 0 until name._2) t.name += "[]"
          var variable = VariableNode(name._1.chars, t)
          Env mount variable
          fields = variable :: fields
        }
        
        fields.reverse
    }
  }

  /**
   * statement
   * :   block
   *
   * |   ('assert'
   *     )
   *     expression (':' expression)? ';'
   * |   'assert'  expression (':' expression)? ';'
   * |   'if' parExpression statement ('else' statement)?
   * |   forstatement
   * |   'while' parExpression statement
   * |   'do' statement 'while' parExpression ';'
   * |   trystatement
   * |   'switch' parExpression '{' switchBlockStatementGroups '}'
   * |   'synchronized' parExpression block
   * |   'return' (expression )? ';'
   * |   'throw' expression ';'
   * |   'break'
   *         (IDENTIFIER
   *         )? ';'
   * |   'continue'
   *         (IDENTIFIER
   *         )? ';'
   * |   expression  ';'
   * |   IDENTIFIER ':' statement
   * |   ';'
   * ;
   */
  def statement :  Parser[Node] = positioned(
      block
    | keyword("assert") ~ expression ~ opt(Colon() ~ expression) <~ Semicolon() ^^^ SomeStatement()
    | keyword("if") ~> parExpression ~ statement ~ opt(keyword("else") ~> statement) ^^ {
      case cond ~ stmt ~ Some(els) => IfElseStmt(cond, stmt, els)
      case cond ~ stmt ~ None => IfElseStmt(cond, stmt)
    }
    | forStatement ^^^ SomeStatement()
    | keyword("while") ~ parExpression ~ statement ^^^ SomeStatement()
    | keyword("do") ~ statement ~ keyword("while") ~ parExpression <~ Semicolon() ^^^ SomeStatement()
    | tryStatement
    | keyword("switch") ~ parExpression <~ LeftBrace() ~> switchBlockStatementGroups <~ RigthBrace() ^^^ SomeStatement()
    | keyword("synchronized") ~ parExpression ~ block ^^^ SomeStatement()
    | keyword("return") ~ opt(expression) <~ Semicolon() ^^ {
      case t ~ Some(expr) => expr match {
        case id: Id => {
          var vn = id.variable
          var compatibile = Env.current.get.apply(vn.asInstanceOf[TypedNode])
          var ret = ReturnStatement(Id(vn))
          if (compatibile.filter(_.name != vn.name).size  > 0) {
            ret.mutationCandidate = true 
            //println("*** Found Getter modification mutation candidate !!! :D:D :D Pos: " + expr.pos)
            print("Candidates: ");
            for (x <- compatibile.filter(_.name != vn.name)) {
              print(x.name +",")
            }
            println("");
            MutationController.addMutationCandidate(ret, MutationKind.CHANGE_GETTER, compatibile.filter(_.name != vn.name).map(_.name))
          }
          ret 
        }
        /*case vn: VariableNode => {
          var compatibile = Env.current.get.apply(vn.asInstanceOf[TypedNode])
          var ret = ReturnStatement(Id(vn))
          if (compatibile.filter(_.name != vn.name).size  > 0) {
            ret.mutationCandidate = true 
            println("*** Found Getter modification mutation candidate !!! :D:D :D")
            print("Candidates: ");
            for (x <- compatibile.filter(_.name != vn.name)) {
              print(x.name +",")
            }
            println("");
            
          }
          ret 
        }*/
        case _ => ReturnStatement(expr)
      }
      case t ~ None => ReturnStatement()
    }
    | keyword("throw") ~ expression <~ Semicolon() ^^ { case t ~ exp => {
        var ret = ThrowStatement(exp); ret.mutationCandidate = true;
        println("*** Found THROW mutation candidate !!! :D:D :D pos " + exp.pos); ret
      }
    }
    | keyword("break") ~ opt(identifier) <~ Semicolon() ^^^ SomeStatement()
    | keyword("continue") ~ opt(identifier) <~ Semicolon() ^^^ SomeStatement()
    | expression <~ Semicolon()
    | identifier ~> Colon() ~> statement
    | Semicolon() ^^^ SomeStatement()
    )

  /**
   * switchBlockStatementGroups
   * :   (switchBlockStatementGroup )*
   * ;
   */
  def switchBlockStatementGroups = rep(switchBlockStatementGroup)

  /**
   * switchBlockStatementGroup
   * :
   *     switchLabel
   *     (blockStatement
   *     )*
   * ;
   */
  def switchBlockStatementGroup = switchLabel ~ rep(blockStatement)

  /**
   * switchLabel
   * :   'case' expression ':'
   * |   'default' ':'
   * ;
   */
  def switchLabel = (
    keyword("case") ~ expression <~ Colon()
    | keyword("default") <~ Colon()
    )

  /**
   * tryStatement
   * :   'try' block
   *     (   catches 'finally' block
   *     |   catches
   *     |   'finally' block
   *     )
   *  ;
   */
  def tryStatement: Parser[Node] = (
      keyword("try") ~> block ~ catches ~ opt(keyword("finally") ~> block) ^^ {
        case tryBlock ~ catchBlocks ~ Some(finallyBlock) => TryCatchStmt(tryBlock, catchBlocks, finallyBlock)
        case tryBlock ~ catchBlocks ~ None  => TryCatchStmt(tryBlock, catchBlocks, BlockNode())
      }
    | keyword("try") ~> block ~ (keyword("finally") ~> block) ^^ {
        case tryBlock ~ finallyBlock => TryCatchStmt(tryBlock, List[BlockNode](), finallyBlock)
      }
    )

  /**
   * catches
   * :   catchClause
   *     (catchClause
   *     )*
   * ;
   */
  def catches: Parser[List[BlockNode]] = rep1(catchClause) 

  /**
   * catchClause
   * :   'catch' '(' formalParameter
   *     ')' block
   * ;
   */
  def catchClause: Parser[BlockNode] = keyword("catch") ~> LeftParenthesis() ~> formalParameter ~> RigthParenthesis() ~> block ^^ {
    // Don't return formal parameter. So far only block are browserd to look for mutation opportunity.
    case b => b
  }

  /**
   * formalParameter
   * :   variableModifiers type IDENTIFIER
   *     ('[' ']'
   *     )*
   * ;
   */
  def formalParameter = variableModifiers ~> typeExpr ~ identifier ~ rep(LeftBracket() ~ RigthBracket()) ^^ {
    case typ ~ name ~ arr => {
      var t = typ
      if (!(arr isEmpty)) for (i <- 0 until arr.length) t.name += "[]"
      var vn = VariableNode(name.chars, t)
     // if (Env.current isDefined) Env.current.get.mount(vn.asInstanceOf[ScopedNode])
      vn
    }
  }

  /**
   * forstatement
   * :
   *     // enhanced for loop
   *     'for' '(' variableModifiers type IDENTIFIER ':'
   *     expression ')' statement
   *
   *     // normal for loop
   * |   'for' '('
   *             (forInit
   *             )? ';'
   *             (expression
   *             )? ';'
   *             (expressionList
   *             )? ')' statement
   * ;
   */
  def forStatement = (
      keyword("for") ~> LeftParenthesis() ~> variableModifiers ~ typeExpr ~ identifier <~
        Colon() ~> expression <~ RigthParenthesis() ~> statement
    | keyword("for") ~> LeftParenthesis()
        ~> opt(forInit) <~ Semicolon()
        ~> opt(expression) <~ Semicolon()
        ~> opt(expressionList) <~ RigthParenthesis() ~> statement
    )

  /**
   * forInit
   * :   localVariableDeclaration
   * |   expressionList
   * ;
   */
  def forInit = localVariableDeclaration | expressionList

// ****************************************************************************
//  Expressions
// ****************************************************************************
  /**
   *                          expressionList
   * :   expression
   *     (',' expression
   *     )*
   * ;
   */
  def expressionList: Parser[List[Expression]] = rep1sep(expression, Comma())

  /**
   * parExpression
   * :   '(' expression ')'
   * ;
   */
  def parExpression: Parser[Expression] = LeftParenthesis() ~> expression <~ RigthParenthesis()

  /**
   *  expression
   * :   conditionalExpression
   *     (assignmentOperator expression
   *     )?
   * ;
   */
  def expression : Parser[Expression]= positioned(conditionalExpression ~ opt(assignmentOperator ~ expression) ^^ {
    case ce ~ assign => if (assign isDefined) assign.asInstanceOf[Some[~[JavaToken,Expression]]].get match {
      case op ~ exp => {
        var tok = tokens.JavaToken(op.chars)
        tok.setPos(op.asInstanceOf[Positional].pos)
        AssignExpr(tok, ce, exp)
      }
    } else ce 
  })
 
  /**
   * assignmentOperator
   * :   '='
   * |   '+='
   * |   '-='
   * |   '*='
   * |   '/='
   * |   '&='
   * |   '|='
   * |   '^='
   * |   '%='
   * |    '<' '<' '='
   * |    '>' '>' '>' '='
   * |    '>' '>' '='
   * ;
   */
  def assignmentOperator : Parser[Elem] = ( AssignOp("=") | AssignOp("+=") | AssignOp("-=") | AssignOp("*=") | AssignOp("/=") |
    AssignOp("&=") | AssignOp("|=") | AssignOp("^=") | AssignOp("%=") | AssignOp("") | BitwiseOp("<<") ~ AssignOp("=") ^^^ AssignOp("<<=")
    | BitwiseOp(">>>") ~ AssignOp("=") ^^^ AssignOp(">>>=") | BitwiseOp(">>") ~ AssignOp("=") ^^^ AssignOp(">>=")
    )


  /**
   *   conditionalExpression
   * :   conditionalOrExpression
   *     ('?' expression ':' conditionalExpression
   *     )?
   * ;
   */
  def conditionalExpression: Parser[Expression] = conditionalOrExpression ~ opt(QuestMark() ~> expression ~ (Colon() ~> conditionalExpression)) ^^ {
    case e1 ~ cond => cond match {
      case c: Some[~[Expression,Expression]] => c.get match {
        case texp ~ fexp => ConditionalExpr(e1, texp, fexp)
      }
      case None => e1 
    }
  }
    /**
   * conditionalOrExpression
   * :   conditionalAndExpression
   *     ('||' conditionalAndExpression
   *     )*
   * ;
   */
  def conditionalOrExpression: Parser[Expression] = chainl1(conditionalAndExpression, JavaToken("||") ^^ { 
	  //rep1sep(conditionalAndExpression, JavaToken("||")) ^^ {
	  orOp => {
	 	  var tok = tokens.JavaToken("||")
	 	  tok.setPos(orOp.asInstanceOf[JavaToken].pos)
	 	  Logical(tok, _: Expression, _: Expression)
	  }
  })

  /**
   * conditionalAndExpression
   * :   inclusiveOrExpression
   *     ('&&' inclusiveOrExpression
   *     )*
   * ;
   */
  def conditionalAndExpression: Parser[Expression] = chainl1(inclusiveOrExpression,JavaToken("&&") ^^ {
	  andOp => {
	 	  var tok = tokens.JavaToken("&&")
	 	  tok.setPos(andOp.asInstanceOf[JavaToken].pos)
	 	  Logical(tok, _: Expression, _: Expression)
	  }
  })

  /**
   * inclusiveOrExpression
   * :   exclusiveOrExpression
   *     ('|' exclusiveOrExpression
   *     )*
   * ;
   */
  def inclusiveOrExpression: Parser[Expression] = chainl1(exclusiveOrExpression, BitwiseOp("|") ^^ {
	  biOp => {
	 	  var tok = tokens.JavaToken("|")
	 	  tok.setPos(biOp.asInstanceOf[JavaToken].pos)
	 	  BinaryExpr(tokens.JavaToken("|"),_ : Expression, _ :  Expression)  
	  }
  })

  /**
   * exclusiveOrExpression
   * :   andExpression
   *     ('^' andExpression
   *     )*
   * ;
   */
  def exclusiveOrExpression: Parser[Expression] = chainl1(andExpression, BitwiseOp("^") ^^ {
	  biOp => {
	 	  var tok = tokens.JavaToken("^")
	 	  tok.setPos(biOp.asInstanceOf[JavaToken].pos)
	 	  BinaryExpr(tokens.JavaToken("^"),_ : Expression, _ :  Expression)  
	  }
  })

  /**
   * andExpression
   * :   equalityExpression
   *     ('&' equalityExpression
   *     )*
   * ;
   */
  def andExpression: Parser[Expression] = rep1sep(equalityExpression, BitwiseOp("&")) ^^ {
	  case exps => exps.dropRight(1).foldRight[Expression](exps.last)((a,b)=> BinaryExpr(tokens.JavaToken("&"),b, a))
  }

  /**
   * equalityExpression
   * :   instanceOfExpression
   *     (
   *         (   '=='
   *         |   '!='
   *        )
   *         instanceOfExpression
   *     )*
   * ;
   */
  def equalityExpression: Parser[Expression] = chainl1(instanceOfExpression, equalityOp ^^ {
		  op => {
			  var tok = tokens.JavaToken(op.chars)
			  tok.setPos(op.asInstanceOf[JavaToken].pos)
		 	  def rel(tok: tokens.JavaToken, e1: Expression, e2: Expression) = {
			    var ret = Relational(tok, e1, e2)
          ret.mutationCandidate = true
          //println("*** Found EQUALITY EXPR mutation candidate !!! :D:D :D: POS="+tok.pos);
          MutationController.addMutationCandidate(ret, RELATIONAL_OP_CHANGE)
			    ret
			  }
			  rel(tok, _: Expression, _: Expression)
		  }
//  		  ret.mutationCandidate = true
//        println("*** Found mutation candidate !!! :D:D :D");
//        ret
	  })
//	  instanceOfExpression ~ rep((JavaToken("==") | JavaToken("!=")) ~ instanceOfExpression) ^^ {
//	  case expr ~ rest => {
//	 	  def foldRest(list: List[Parser[~[JavaToken,Expression]]]): Expression = list match {
//	 	 	  case xs :: Nil => xs match {
//	 	 	 	  cz
//	 	 	  }
//	 	 	  case xs :: rest => xs match {
//	 	 	 	  case op ~ expr => Relational(JavaToken(op.chars), expr, foldRest(rest))
//	 	 	 	  Relational.
//	 	 	  }
//	 	  }
//	  }
//  }
  /**
   * equalityOp 
   *   :   
   *     '=='
   *   | '!='
   * ;
   */
  def equalityOp = RelationalOp("==") | RelationalOp("!=")
  /**
   * instanceOfExpression
   *   :   relationalExpression
   *     ('instanceof' type
   *     )?
   * ;
   */
  def instanceOfExpression: Parser[Expression] = relationalExpression ~ opt(keyword("instanceof") ~> typeExpr) ^^ {
	  case expr ~ Some(instOfExpr) => expr //TODO: deal with instanceOf
	  case expr ~ None => expr 
  }

  /**
   * relationalExpression
   * :   shiftExpression
   *     (relationalOp shiftExpression
   *     )*
   * ;
   */
  def relationalExpression: Parser[Expression] = chainl1(shiftExpression, relationalOp ^^ {
	  op => {
    	 	var tok = tokens.JavaToken(op.chars)
    	 	tok.setPos(op.asInstanceOf[JavaToken].pos)
    	 	def rel(tok: tokens.JavaToken, e1: Expression, e2: Expression) = {
    	 	  var ret = Relational(tok, e1, e2)
          ret.mutationCandidate = true
          //println("*** Found RELATIONAL mutation candidate !!! :D:D :D POS: " + tok.pos);
    	 	  MutationController.addMutationCandidate(ret, RELATIONAL_OP_CHANGE)
     	    ret
    	 	}
    	 	rel(tok, _: Expression, _: Expression)
	    }
//	  ret.mutationCandidate = true
//    println("*** Found mutation candidate !!! :D:D :D");
//    ret
  })

  /**
   * relationalOp
   * :    '<' '='
   * |    '>' '='
   * |   '<'
   * |   '>'
   * ;
   */
  def relationalOp = RelationalOp("<=") | RelationalOp(">=") | RelationalOp("<") | RelationalOp(">")


  /**
   * shiftExpression
   * :   additiveExpression
   *     (shiftOp additiveExpression
   *     )*
   * ;
   */
  def shiftExpression: Parser[Expression] = chainl1(additiveExpression, shiftOp ^^ {
	op => {
		var tok = tokens.JavaToken(op.chars)
		tok.setPos(op.asInstanceOf[JavaToken].pos)
		BinaryExpr(tok, _: Expression, _: Expression)
	}
  })

  /**
   * shiftOp
   * :    '<' '<'
   * |    '>' '>' '>'
   * |    '>' '>'
   * ;
   */
  def shiftOp = accept(BitwiseOp("<<")) | accept(BitwiseOp(">>>")) | accept(BitwiseOp(">>"))

  /**
   * additiveExpression
   * :   multiplicativeExpression
   *     (
   *         (   '+'
   *         |   '-'
   *         )
   *         multiplicativeExpression
   *      )*
   * ;
   */
  def additiveExpression: Parser[Expression] = chainl1(multiplicativeExpression, additivteOp ^^ {
	  op => {
	 	  var tok = tokens.JavaToken(op.chars)
	 	  tok.setPos(op.asInstanceOf[JavaToken].pos)
	 	  BinaryExpr(tok, _: Expression, _: Expression)
	  }
  })
  
  def additivteOp = ArithmOp("+") | ArithmOp("-")
  /**
   * multiplicativeExpression
   * :
   *     unaryExpression
   *     (
   *         (   '*'
   *         |   '/'
   *         |   '%'
   *         )
   *         unaryExpression
   *     )*
   * ;
   */
  def multiplicativeExpression: Parser[Expression] = chainl1(unaryExpression, multiplicativeOp ^^ {
	op => {
		var tok = tokens.JavaToken(op.chars)
		tok.setPos(op.asInstanceOf[JavaToken].pos)
		BinaryExpr(tok, _: Expression, _: Expression)
	}
  })
  def multiplicativeOp = ArithmOp("*") | ArithmOp("/") | ArithmOp("%")
  /**
   * unaryExpression
   * :   '+'  unaryExpression
   * |   '-' unaryExpression
   * |   '++' unaryExpression
   * |   '--' unaryExpression
   * |   unaryExpressionNotPlusMinus
   * ;
   */
  def unaryExpression : Parser[Expression] = (
    (  ArithmOp("+")
    | ArithmOp("-")
    | ArithmOp("++")
    | ArithmOp("--") ) ~ unaryExpression ^^ {
    	case op ~ expr => { 
    		var tok = tokens.JavaToken(op.chars)
    		tok.setPos(op.asInstanceOf[JavaToken].pos)
    		UnaryExpr(tok, expr)
    	}  
    }
    | unaryExpressionNotPlusMinus
    )

  /**
   * unaryExpressionNotPlusMinus
   * :   '~' unaryExpression
   * |   '!' unaryExpression
   * |   castExpression
   * |   primary
   *     (selector
   *     )*
   *     (   '++'
   *     |   '--'
   *     )?
   * ;
   */
  def unaryExpressionNotPlusMinus: Parser[Expression] = (
     BitwiseOp("~") ~ unaryExpression ^^ {
    	 case op ~ expr => {
    		 var tok = tokens.JavaToken(op.chars)
    		 tok.setPos(op.asInstanceOf[Positional].pos)
    		 UnaryExpr(tok, expr)
    	 }
     }
    | JavaToken("!") ~ unaryExpression ^^ {
    	 case op ~ expr => {
    		 var tok = tokens.JavaToken(op.chars)
    		 tok.setPos(op.asInstanceOf[Positional].pos)
    		 NotExpr(tok, expr)
    	 }
     }
    | castExpression
    | primary ~ rep(selector) ~ opt(ArithmOp("++") | ArithmOp("--")) ^^ {
      case exp ~ selector ~ suffixUn => if (suffixUn.isDefined) suffixUn.get match {
        case unary => {
          var tok = tokens.JavaToken(unary.chars)
          tok.setPos(unary.asInstanceOf[Positional].pos)
          SuffixUnaryExpr(tok,exp)
        }
      } else exp
    }  
    )

  /**
   * castExpression
   * :   '(' primitiveType ')' unaryExpression
   * |   '(' type ')' unaryExpressionNotPlusMinus
   * ;
   */
  def castExpression: Parser[Expression] = (
    (LeftParenthesis() ~> primitiveType <~ RigthParenthesis()) ~ unaryExpression ^^ { case t ~ expr => CastExpr(t, expr) }
    | (LeftParenthesis() ~> typeExpr <~ RigthParenthesis()) ~ unaryExpressionNotPlusMinus ^^
      { case t ~ expr => CastExpr(t, expr) }
  )

  /**
   * primary
   * :   parExpression
   * |   'this'
   *     ('.' IDENTIFIER
   *     )*
   *     (identifierSuffix
   *     )?
   * |   IDENTIFIER
   *     ('.' IDENTIFIER
   *     )*
   *     (identifierSuffix
   *     )?
   * |   'super'
   *     superSuffix
   * |   literal
   * |   creator
   * |   primitiveType
   *     ('[' ']'
   *     )*
   *     '.' 'class'
   * |   'void' '.' 'class'
   * ;
   */
  def primary: Parser[Expression]= (
    parExpression
    // So far I won't handle the suffixes
    | keyword("this") ~ rep(Dot() ~> identifier) ~ opt(identifierSuffix) ^^ {
    	case t ~ path ~ idSuffix => {
    	  var name = ""
    	  var id = JavaIdentifier(name)
    	  path match {
    	    // Consider only first identifier before node as this may be the reall identifier of variable
    	    case xs :: rest => {
    	      name = xs.chars; id = xs.asInstanceOf[JavaIdentifier]
        	  var mutationCandidate = Env getFromCurrent name
        	  var onTop = Env getFromTop name
        	  var returnVal = {
        	    if(onTop.isDefined) {
        	      var nod = Id(onTop.get.asInstanceOf[VariableNode])
        	      nod.setPos(id.pos)
        	      ThisExpr(nod)  
        	    } else if(mutationCandidate isDefined) {
        	      var nod = Id(mutationCandidate.get.asInstanceOf[VariableNode])
        	      nod.setPos(id.pos)
        	      ThisExpr(nod) 
        	    } else {
        	      var nod = Id(VariableNode(name, Type.DEFFERED))
        	      nod setPos id.pos  
        	      ThisExpr(nod)   
        	    }
        	  }  
        	  if (mutationCandidate.isDefined && onTop.isDefined ) {
        	    if(mutationCandidate.get.boundToId != onTop.get.boundToId) returnVal.mutationCandidate = true
        	    //println("*** Found THIS mutation candidate !!! :D:D :D POS: " + id.pos)
        	    returnVal.setPos(id.pos)
        	    MutationController.addMutationCandidate(returnVal, MutationKind.REMOVE_THIS)
        	  }
        	  returnVal
    	    }
    	    case Nil => ThisExpr(Id(VariableNode("", Type.DEFFERED)))
    	  }
    	}
    }
    | identifier ~ rep(Dot() ~> identifier) ~ opt(identifierSuffix)^^ {
      case id ~ ids ~ Some(suffix) => SomeExpr()
      case id ~ rest ~ None => Env getFromCurrent id.chars match {
        case Some(vn) => Id(vn.asInstanceOf[VariableNode])
        case None => { 
        //  println("!!! Didn't found variable in scope. " + id.chars + "***"); 
          Id(VariableNode(id.chars, Type.DEFFERED)) 
        }
      }
    }
    | keyword("super") ~ superSuffix ^^^ SuperExpr("") //Does not not do anything with the suffix
    | literal
    | creator ^^^ SomeExpr()
    | primitiveType ~ rep(accept(LeftBracket()) ~ accept(RigthBracket())) ~ Dot() ~ keyword("class") ^^^ SomeExpr()
    | keyword("void") ~ accept(Dot()) ~ keyword("class") ^^^ SomeExpr()
    )

  /**
   * superSuffix
   * :   arguments
   * |   '.' (typeArguments
   *     )?
   *     IDENTIFIER
   *     (arguments
   *     )?
   * ;
   */
  def superSuffix = (
    arguments
    | accept(Dot()) ~ opt(typeArguments) ~ identifier ~ opt(arguments)
    )

  /**
   * identifierSuffix
   * :   ('[' ']'
   *     )+
   *     '.' 'class'
   * |   ('[' expression ']'
   *     )+
   * |   arguments
   * |   '.' 'class'
   * |   '.' nonWildcardTypeArguments IDENTIFIER arguments
   * |   '.' 'this'
   * |   '.' 'super' arguments
   * |   innerCreator
   * ;
   */
  def identifierSuffix: Parser[Node] = (
    rep1(LeftBracket() ~ RigthBracket()) ~ Dot() ~ keyword("class")
    | rep1(LeftBracket() ~ expression ~ RigthBracket())
    | arguments
    | Dot() ~ keyword("class")
    | Dot() ~ nonWildcardTypeArguments ~ identifier ~ arguments
    | Dot() ~ keyword("this")
    | Dot() ~ keyword("super") ~ arguments
    | innerCreator
    ) ^^^ SomeNode()

  /**
   * selector
    :   '.' IDENTIFIER
        (arguments
        )?
    |   '.' 'this'
    |   '.' 'super'
        superSuffix
    |   innerCreator
    |   '[' expression ']'
    ;
   */
  def selector = (
    Dot() ~ identifier ~ opt(arguments)
    | Dot() ~ keyword("this")
    | Dot() ~ keyword("super") ~ superSuffix
    | innerCreator
    | LeftBracket() ~ expression ~ RigthBracket()
    )

  /**
   * creator
   * :   'new' nonWildcardTypeArguments classOrInterfaceType classCreatorRest
   * |   'new' classOrInterfaceType classCreatorRest
   * |   arrayCreator
   * ;
   */
  def creator = (
    keyword("new") ~ nonWildcardTypeArguments ~ classOrInterfaceType ~ classCreatorRest
    | keyword("new") ~ classOrInterfaceType ~ classCreatorRest
    | arrayCreator
    )

  /**
   * arrayCreator
   * :   'new' createdName
   *     '[' ']'
   *     ('[' ']'
   *     )*
   *     arrayInitializer
   *
   * |   'new' createdName
   *     '[' expression
   *     ']'
   *     (   '[' expression
   *         ']'
   *     )*
   *     ('[' ']'
   *     )*
   * ;
   */
  def arrayCreator = (
    keyword("new") ~ createdName ~ rep1(LeftBracket() ~ RigthBracket()) ~ arrayInitializer
    |
    keyword("new") ~ createdName ~ rep1(LeftBracket() ~ expression ~ RigthBracket()) ~ rep(LeftBracket() ~ RigthBracket())
    )

  /**
   * variableInitializer
   * :   arrayInitializer
   * |   expression
   * ;
   */
  def variableInitializer = arrayInitializer | expression;

  /**
   * arrayInitializer
   * :   '{'
   *         (variableInitializer
   *             (',' variableInitializer
   *             )*
   *         )?
   *         (',')?
   *     '}'             //Yang's fix, position change.
   * ;
   */
  def arrayInitializer : Parser[Any] = LeftBrace() ~ opt(rep1sep(variableInitializer,Comma())) ~ opt(Comma()) ~ RigthBrace()

  /**
   * createdName
   * :   classOrInterfaceType
   * |   primitiveType
   * ;
   */
  def createdName = classOrInterfaceType | primitiveType

  /**
   * innerCreator
   * :   '.' 'new'
   *     (nonWildcardTypeArguments
   *     )?
   *     IDENTIFIER
   *     (typeArguments
   *     )?
   *     classCreatorRest
   * ;
   */
  def innerCreator = Dot() ~ keyword("new") ~ opt(nonWildcardTypeArguments) ~ identifier ~ opt(typeArguments) ~ classCreatorRest

  /**
   *  classCreatorRest
   * :   arguments
   *     (classBody
   *     )?
   * ;
   */
  def classCreatorRest = arguments ~ opt(classBody)

  /**
   * nonWildcardTypeArguments
   * :   '<' typeList
   *     '>'
   * ;
   */
  def nonWildcardTypeArguments = RelationalOp("<") ~ typeList ~ RelationalOp(">")

  /**
   * arguments
   * :   '(' (expressionList
   *     )? ')'
   * ;
   */
  def arguments = LeftParenthesis() ~ opt(expressionList) ~ RigthParenthesis()

  /**
   * literal
   * :   INTLITERAL
   * |   LONGLITERAL
   * |   FLOATLITERAL
   * |   DOUBLELITERAL
   * |   CHARLITERAL
   * |   STRINGLITERAL
   * |   TRUE
   * |   FALSE
   * |   NULL
   * ;
   */
  def literal: Parser[Constant] = positioned(
    acceptIf(_.isInstanceOf[NumericIntLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.NumericIntLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | acceptIf(_.isInstanceOf[NumericLongLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.NumericLongLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | acceptIf(_.isInstanceOf[NumericFloatLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.NumericFloatLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | acceptIf(_.isInstanceOf[NumericDoubleLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.NumericDoubleLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | acceptIf(_.isInstanceOf[CharLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.CharLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | acceptIf(_.isInstanceOf[JavaStringLit])("Not a literal: " + _.toString()) ^^ {
    	case lit => {
    		var tok = tokens.JavaStringLit(lit.chars)
    		tok.setPos(lit.asInstanceOf[Positional].pos)
    		Constant(tok)
    	}
    }
    | keyword("true") ^^^ Constant.TRUE 
    | keyword("false") ^^^ Constant.FALSE
    | keyword("null") ^^^ Constant.NULL
    )
  /*
  Annotations
   */
  def annotations = rep1(annotation)

  /**
   *   annotation
   * :   '@' qualifiedName
   *     (   '('
   *               (   elementValuePairs
   *               |   elementValue
   *               )?
   *         ')'
   *     )?
   * ;
   */
  def annotation = OtherToken("@") ~> qualifiedName <~ opt(LeftParenthesis() ~> opt(elementValuePairs | elementValue) <~ RigthParenthesis())

  /**
   * elementValuePairs
   * :   elementValuePair
   *     (',' elementValuePair
   *     )*
   * ;
   */
  def elementValuePairs = rep1sep(elementValuePair, Comma())

  /**
   * elementValuePair
   * :   IDENTIFIER '=' elementValue
   * ;
   */
  def elementValuePair =  identifier ~ AssignOp() ~ elementValue

  /**
   * elementValue
   * :   conditionalExpression
   * |   annotation
   * |   elementValueArrayInitializer
   * ;
   */
  def elementValue : Parser[Any] = conditionalExpression | annotation | elementValueArrayInitializer

  /**
   * elementValueArrayInitializer
   * :   '{'
   *     (elementValue
   *         (',' elementValue
   *         )*
   *     )? (',')? '}'
   * ;
   */
  def elementValueArrayInitializer = LeftBrace() ~> repsep(elementValue,Comma()) <~ RigthBrace()

  /**
   * annotationTypeDeclaration
   * :   modifiers '@'
   *     'interface'
   *     IDENTIFIER
   *     annotationTypeBody
   * ;
   */
  def annotationTypeDeclaration = modifiers ~ OtherToken("@") ~ keyword("interface") ~ identifier ~ annotationTypeBody

  /**
   * annotationTypeBody
   * :   '{'
   *     (annotationTypeElementDeclaration
   *     )*
   *     '}'
   * ;
   */
  def annotationTypeBody = LeftBrace() ~ rep(annotationTypeElementDeclaration) ~ RigthBrace()

  /**
   * annotationTypeElementDeclaration
   * :   annotationMethodDeclaration
   * |   interfaceFieldDeclaration
   * |   normalClassDeclaration
   * |   normalInterfaceDeclaration
   * |   enumDeclaration
   * |   annotationTypeDeclaration
   * |   ';'
   * ;
   */
  def annotationTypeElementDeclaration : Parser[Any] = (
      annotationMethodDeclaration
      | interfaceFieldDeclaration
      | normalClassDeclaration
      | normalInterfaceDeclaration
      | enumDeclaration
      | annotationTypeDeclaration
      | Semicolon()
    )

  /**
   * annotationMethodDeclaration
   * :   modifiers type IDENTIFIER
   *     '(' ')' ('default' elementValue
   *             )?
   *     ';'
   *     ;
   */
  def annotationMethodDeclaration = modifiers ~ typeExpr ~ identifier ~ LeftParenthesis() ~ RigthParenthesis() ~
    opt(keyword("default") ~ elementValue) ~ Semicolon()
  
  def apply(source: String) : Option[List[Node]]= {
    phrase(program)(new Scanner(source)) match {
      case Success(item, next) => {
          println("Source parsing succeded...")
          println("Applying mutations... ")
          return Some(item)
      }
      case Failure(msg, next) => {
        println("Parsing failed with msg: " + msg)
        println("At position: " + next.pos + " in:")
        println(next.source.toString.lines.drop(next.pos.line - 1).next())
        for (i <- 0 until next.pos.column) {
          print(" ")
        }
        println("^")
        return None
      } 
    } 
  }
}