package com.pp.sjm.parser

import com.pp.sjm.ast._
import com.pp.sjm.symbols.{Type,TypeClass}
import com.pp.sjm.token.JavaTokens

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.collection.mutable.{HashSet,LinkedList}

/**
 * @author Pawel
 *
 */
class JavaParser extends StandardTokenParsers with PackratParsers {
  override val lexical: JavaLexer = new JavaLexer

  import lexical.{Identifier,Dot,Semicolon,ArithmOp,JavaToken,LogicalOp,BitwiseOp,RelationalOp,QuestMark,CharLit}
  import lexical.{LeftBracket,RigthBracket,LeftBrace,RigthBrace,LeftParenthesis}
  import lexical.{RigthParenthesis,AssignOp,AssignArithmOp,OtherToken,Colon,Comma,Ellipsis,Wildcard,NumericLongLit}
  import lexical.{NumericDoubleLit,NumericFloatLit,NumericIntLit,NumericLit,OctNumericLit,HexNumericLit,StringLit}
  import lexical.{JavaIdentifier, JavaKeyword, JavaStringLit, Keyword}

  override type Elem = lexical.Token
  
  /**
   * Start symbol of the parser
   */
  def program = compilationUnit

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
  def compilationUnit = opt(opt(annotations) ~ packageDeclaration) ~> rep(importDeclaration) ~> rep(typeDeclaration)

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
  def identifier = elem("identifier", _.isInstanceOf[Identifier])
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
  def typeDeclaration = classOrInterfaceDeclaration | Semicolon()

  /**
   * classOrInterfaceDeclaration
   * :    classDeclaration
   * |   interfaceDeclaration
   * ;
   */
  def classOrInterfaceDeclaration = (
    classDeclaration
  | interfaceDeclaration
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
    | methodDeclaration ^^ { case method => List[Node](method) }
    | classDeclaration ^^ { // Care only about internal classess - there may be some opprotunity do mutate.
        case c: ClassNode => List[Node](c)
        case _ => List[Node](SomeNode())
      }
    | interfaceDeclaration ^^^ List[Node](SomeNode()) //Don't care about internal interface
    )

  /**
   * methodDeclaration
   * :
   *     /* For constructor, return type is null, name is 'init' */
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
      ~> LeftBrace() ~> opt(explicitConstructorInvocation) ~ rep(blockStatement) <~ RigthBrace()) ^^ {
    	case id ~ formalParams ~ block => MethodNode(id.chars, Type.CONSTRUCT, BlockNode())
    	// TODO: Implement serious dealing with constructor body (this mutator opportunity)
      }
      
    |
    /* member methods */
    modifiers ~> opt(typeParameters) ~> (typeExpr | keyword("void") ^^^ Type.VOID) ~ identifier ~ formalParameters
      ~ (rep(LeftBracket() ~ RigthBracket()) ~> opt(keyword("throws") ~> qualifiedNameList) ~> (block | Semicolon() ^^^ BlockNode())) ^^ {
    	// TODO: Implement serious dealing with parameters and block
        case t ~ id ~ params ~ block => MethodNode(id.chars, t, BlockNode())  
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
	 		fields = FieldNode(name._1.chars, t) :: fields
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
  def classDeclaration = (
    normalClassDeclaration
  | enumDeclaration
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
  def formalParameters = LeftParenthesis() ~ opt(formalParameterDecls) ~ RigthParenthesis()

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
  def formalParameterDecls = (
    ellipsisParameterDecl
    | rep1sep(normalParameterDecl, Comma())
    | rep1(normalParameterDecl <~ Comma()) <~ Comma() ~> ellipsisParameterDecl
    )

  /**
   * normalParameterDecl
   * :   variableModifiers type IDENTIFIER
   *     ('[' ']'
   *     )*
   * ;
   */
  def normalParameterDecl = variableModifiers ~ typeExpr ~ identifier ~ rep(LeftBracket() ~ RigthBracket())

  /**
   * ellipsisParameterDecl
   * :   variableModifiers
   *     type  '...'
   *     IDENTIFIER
   * ;
   */
  def ellipsisParameterDecl = variableModifiers ~ typeExpr ~ Ellipsis() ~ identifier

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
  def block =  LeftBrace() ~> rep(blockStatement) <~ RigthBrace()

  /**
   * blockStatement
   * :   localVariableDeclarationStatement
   * |   classOrInterfaceDeclaration
   * |   statement
   * ;
   */
  def blockStatement: Parser[Any] = (
      localVariableDeclarationStatement
    | classOrInterfaceDeclaration
    | statement
    )

  /**
   * localVariableDeclarationStatement
   * :   localVariableDeclaration
   *     ';'
   * ;
   */
   def localVariableDeclarationStatement = localVariableDeclaration <~ Semicolon()

  /**
   * localVariableDeclaration
   * :   variableModifiers type
   *     variableDeclarator
   *     (',' variableDeclarator
   *     )*
   * ;
   */
  def localVariableDeclaration = variableModifiers ~ typeExpr ~ variableDeclarator <~
    rep(Comma() ~> variableDeclarator)

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
  def statement :  Parser[Any] = (
      block
    | keyword("assert") ~ expression ~ opt(Colon() ~ expression) <~ Semicolon()
    | keyword("if") ~ parExpression ~ statement ~ opt(keyword("else") ~ statement)
    | forStatement
    | keyword("while") ~ parExpression ~ statement
    | keyword("do") ~ statement ~ keyword("while") ~ parExpression <~ Semicolon()
    | tryStatement
    | keyword("switch") ~ parExpression <~ LeftBrace() ~> switchBlockStatementGroups <~ RigthBrace()
    | keyword("synchronized") ~ parExpression ~ block
    | keyword("return") ~ opt(expression) <~ Semicolon()
    | keyword("throw") ~ expression <~ Semicolon()
    | keyword("break") ~ opt(identifier) <~ Semicolon()
    | keyword("continue") ~ opt(identifier) <~ Semicolon()
    | expression <~ Semicolon()
    | identifier <~ Colon() ~> statement
    | Semicolon()
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
  def tryStatement = keyword("try") ~ block ~ (
      catches ~ opt(keyword("finally") ~ block)
    | keyword("finally") ~ block
    )

  /**
   * catches
   * :   catchClause
   *     (catchClause
   *     )*
   * ;
   */
  def catches = catchClause ~ rep(catchClause)

  /**
   * catchClause
   * :   'catch' '(' formalParameter
   *     ')' block
   * ;
   */
  def catchClause = keyword("catch") ~> LeftParenthesis() ~> formalParameter <~ RigthParenthesis() ~> block

  /**
   * formalParameter
   * :   variableModifiers type IDENTIFIER
   *     ('[' ']'
   *     )*
   * ;
   */
  def formalParameter = variableModifiers ~ typeExpr ~ identifier ~ rep(LeftBracket() ~ RigthBracket())

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
  def expressionList = rep1sep(expression, Comma())

  /**
   * parExpression
   * :   '(' expression ')'
   * ;
   */
  def parExpression = LeftParenthesis() ~ expression ~ RigthParenthesis()

  /**
   *  expression
   * :   conditionalExpression
   *     (assignmentOperator expression
   *     )?
   * ;
   */
  def expression : Parser[Any]= conditionalExpression ~ opt(assignmentOperator ~ expression)

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
  def assignmentOperator = ( AssignOp("=") | AssignOp("+=") | AssignOp("-=") | AssignOp("*=") | AssignOp("/=") |
    AssignOp("&=") | AssignOp("|=") | AssignOp("^=") | AssignOp("%=") | AssignOp("") | BitwiseOp("<<") ~ AssignOp("=")
    | BitwiseOp(">>>") ~ AssignOp("=") | BitwiseOp(">>") ~ AssignOp("=")
    )


  /**
   *   conditionalExpression
   * :   conditionalOrExpression
   *     ('?' expression ':' conditionalExpression
   *     )?
   * ;
   */
  def conditionalExpression: Parser[Any] = conditionalOrExpression ~ opt(accept(QuestMark()) ~> expression ~ Colon() ~ conditionalExpression)
    /**
   * conditionalOrExpression
   * :   conditionalAndExpression
   *     ('||' conditionalAndExpression
   *     )*
   * ;
   */
  def conditionalOrExpression = rep1sep(conditionalAndExpression, accept(LogicalOp("||")))

  /**
   * conditionalAndExpression
   * :   inclusiveOrExpression
   *     ('&&' inclusiveOrExpression
   *     )*
   * ;
   */
  def conditionalAndExpression = rep1sep(inclusiveOrExpression, accept(LogicalOp("&&")))

  /**
   * inclusiveOrExpression
   * :   exclusiveOrExpression
   *     ('|' exclusiveOrExpression
   *     )*
   * ;
   */
  def inclusiveOrExpression = rep1sep(exclusiveOrExpression, accept(BitwiseOp("|")))

  /**
   * exclusiveOrExpression
   * :   andExpression
   *     ('^' andExpression
   *     )*
   * ;
   */
  def exclusiveOrExpression = rep1sep(andExpression, accept(BitwiseOp("^")))

  /**
   * andExpression
   * :   equalityExpression
   *     ('&' equalityExpression
   *     )*
   * ;
   */
  def andExpression = rep1sep(equalityExpression, accept(BitwiseOp("&")))

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
  def equalityExpression = instanceOfExpression ~ opt((accept(RelationalOp("==")) | accept(RelationalOp("!="))) ~ instanceOfExpression)

  /**
   * instanceOfExpression
   *   :   relationalExpression
   *     ('instanceof' type
   *     )?
   * ;
   */
  def instanceOfExpression = relationalExpression ~ opt(keyword("instanceof") ~ typeExpr)

  /**
   * relationalExpression
   * :   shiftExpression
   *     (relationalOp shiftExpression
   *     )*
   * ;
   */
  def relationalExpression = shiftExpression ~ rep(relationalOp ~ shiftExpression)

  /**
   * relationalOp
   * :    '<' '='
   * |    '>' '='
   * |   '<'
   * |   '>'
   * ;
   */
  def relationalOp = (
    accept(RelationalOp("<=")) | accept(RelationalOp(">="))
    | accept(RelationalOp("<")) | accept(RelationalOp(">"))
  )

  /**
   * shiftExpression
   * :   additiveExpression
   *     (shiftOp additiveExpression
   *     )*
   * ;
   */
  def shiftExpression = additiveExpression ~ rep(shiftOp ~ additiveExpression)

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
  def additiveExpression = multiplicativeExpression ~ rep((accept(ArithmOp("+")) | accept(ArithmOp("-"))) ~ multiplicativeExpression)

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
  def multiplicativeExpression = unaryExpression ~
        rep((accept(ArithmOp("*")) | accept(ArithmOp("/")) | accept(ArithmOp("%"))) ~ unaryExpression)

  /**
   * unaryExpression
   * :   '+'  unaryExpression
   * |   '-' unaryExpression
   * |   '++' unaryExpression
   * |   '--' unaryExpression
   * |   unaryExpressionNotPlusMinus
   * ;
   */
  def unaryExpression : Parser[Any] = (
   ArithmOp("+") ~ unaryExpression
    | ArithmOp("-") ~  unaryExpression
    | ArithmOp("++") ~ unaryExpression
    | ArithmOp("--") ~ unaryExpression
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
  def unaryExpressionNotPlusMinus = (
     BitwiseOp("~") ~ unaryExpression
    | LogicalOp("!") ~ unaryExpression
    | castExpression
    | primary ~ rep(selector) ~ opt(ArithmOp("++") | ArithmOp("--"))
    )

  /**
   * castExpression
   * :   '(' primitiveType ')' unaryExpression
   * |   '(' type ')' unaryExpressionNotPlusMinus
   * ;
   */
  def castExpression: Parser[Any] = (
    LeftParenthesis() ~ primitiveType ~ RigthParenthesis() ~ unaryExpression
    | LeftParenthesis() ~ typeExpr ~ RigthParenthesis() ~ unaryExpressionNotPlusMinus
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
  def primary = (
    parExpression
    | keyword("this") ~ rep(Dot() ~ identifier) ~ opt(identifierSuffix)
    | identifier ~ rep(Dot() ~ identifier) ~ opt(identifierSuffix)
    | keyword("super") ~ superSuffix
    | literal
    | creator
    | primitiveType ~ rep(accept(LeftBracket()) ~ accept(RigthBracket())) ~ Dot() ~ keyword("class")
    | keyword("void") ~ accept(Dot()) ~ keyword("class")
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
  def identifierSuffix = (
    rep1(LeftBracket() ~ RigthBracket()) ~ Dot() ~ keyword("class")
    | rep1(LeftBracket() ~ expression ~ RigthBracket())
    | arguments
    | Dot() ~ keyword("class")
    | Dot() ~ nonWildcardTypeArguments ~ identifier ~ arguments
    | Dot() ~ keyword("this")
    | Dot() ~ keyword("super") ~ arguments
    | innerCreator
    )

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
  def literal = (
    acceptIf(_.isInstanceOf[NumericIntLit])("Not a literal: " + _.toString())
    | acceptIf(_.isInstanceOf[NumericLongLit])("Not a literal: " + _.toString())
    | acceptIf(_.isInstanceOf[NumericFloatLit])("Not a literal: " + _.toString())
    | acceptIf(_.isInstanceOf[NumericDoubleLit])("Not a literal: " + _.toString())
    | acceptIf(_.isInstanceOf[CharLit])("Not a literal: " + _.toString())
    | acceptIf(_.isInstanceOf[JavaStringLit])("Not a literal: " + _.toString())
    | keyword("true")
    | keyword("false")
    | keyword("null")
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
}