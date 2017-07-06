/*
 * [The "BSD license"]
 *  Copyright (c) 2014 Terence Parr
 *  Copyright (c) 2014 Sam Harwell
 *  Copyright (c) 2017 Ivan Kochurkin, Positive Technologies
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * A Java 8 grammar for ANTLR 4 derived from the Java Language Specification
 */
parser grammar Java8Parser;

options { tokenVocab=Java8Lexer; }

// Packages

compilationUnit
    : packageDeclaration? importDeclaration* typeDeclaration* EOF
    ;

packageDeclaration
    : annotation* 'package' Identifier ('.' Identifier)* ';'
    ;

importDeclaration
    : 'import' (typeName | qualifiedName '.' '*') ';'
    | 'import' 'static' typeName '.' (Identifier | '*') ';'
    ;

typeDeclaration
    : classDeclaration
    | interfaceDeclaration
    | ';'
    ;

// Classes

classDeclaration
    : normalClassDeclaration
    | enumDeclaration
    ;

normalClassDeclaration
    : classModifier* 'class' Identifier typeParameters? superclass? superinterfaces? classBody
    ;

classModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'final'
    | 'strictfp'
    ;

typeParameters
    : '<' typeParameter (',' typeParameter)* '>'
    ;

superclass
    : 'extends' classType
    ;

superinterfaces
    : 'implements' interfaceTypeList
    ;

classBody
    : '{' classBodyDeclaration* '}'
    ;

classBodyDeclaration
    : classMemberDeclaration
    | instanceInitializer=block
    | staticInitializer
    | constructorDeclaration
    ;

classMemberDeclaration
    : fieldDeclaration
    | methodDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

fieldDeclaration
    : fieldModifier* unannType variableDeclaratorList ';'
    ;

fieldModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'static'
    | 'final'
    | 'transient'
    | 'volatile'
    ;

variableDeclaratorList
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    : Identifier dims?
    ;

variableInitializer
    : expression
    | arrayInitializer
    ;

unannType
    : numericTypeOrBoolean
    | unannReferenceType
    ;

unannReferenceType
    : unannClassOrInterfaceType
    | unannArrayType
    | Identifier
    ;

unannClassOrInterfaceType
    : Identifier typeArguments? unannClassType_lf_unannClassOrInterfaceType*
    ;

unannClassType
    : Identifier typeArguments?
    | unannClassOrInterfaceType '.' annotation* Identifier typeArguments?
    ;

unannClassType_lf_unannClassOrInterfaceType
    : '.' annotation* Identifier typeArguments?
    ;

unannArrayType
    : (numericTypeOrBoolean | unannClassOrInterfaceType | Identifier) dims
    ;

methodDeclaration
    : methodModifier* methodHeader methodBody
    ;

methodModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'final'
    | 'synchronized'
    | 'native'
    | 'strictfp'
    ;

methodHeader
    : (typeParameters annotation*)? result methodDeclarator throws_?
    ;

result
    : unannType
    | 'void'
    ;

methodDeclarator
    : Identifier '(' formalParameterList? ')' dims?
    ;

formalParameterList
    : (formalParameters ',')? lastFormalParameter
    ;

formalParameters
    : (formalParameter | receiverParameter) (',' formalParameter)*
    ;

formalParameter
    : variableModifier* unannType variableDeclaratorId
    ;

variableModifier
    : annotation
    | 'final'
    ;

lastFormalParameter
    : variableModifier* unannType annotation* '...' variableDeclaratorId
    | formalParameter
    ;

receiverParameter
    : annotation* unannType (Identifier '.')? 'this'
    ;

throws_
    : 'throws' exceptionType (',' exceptionType)*
    ;

exceptionType
    : classType
    | typeVariable
    ;

typeVariable
    : annotation* Identifier
    ;

methodBody
    : block
    | ';'
    ;

staticInitializer
    : 'static' block
    ;

constructorDeclaration
    : constructorModifier* constructorDeclarator throws_? constructorBody
    ;

constructorModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    ;

constructorDeclarator
    : typeParameters? simpleTypeName '(' formalParameterList? ')'
    ;

simpleTypeName
    : Identifier
    ;

constructorBody
    : '{' explicitConstructorInvocation? blockStatements? '}'
    ;

explicitConstructorInvocation
    : (typeArguments? ('this' | 'super')
      | (qualifiedName | primary) '.' typeArguments? 'super')
      '(' argumentList? ')' ';'
    ;

enumDeclaration
    : classModifier* 'enum' Identifier superinterfaces? enumBody
    ;

enumBody
    : '{' enumConstantList? ','? (';' classBodyDeclaration*)? '}'
    ;

enumConstantList
    : enumConstant (',' enumConstant)*
    ;

enumConstant
    : annotation* Identifier ('(' argumentList? ')')? classBody?
    ;

// Interfaces

interfaceDeclaration
    : normalInterfaceDeclaration
    | annotationTypeDeclaration
    ;

normalInterfaceDeclaration
    : interfaceModifier* 'interface' Identifier typeParameters? extendsInterfaces? interfaceBody
    ;

interfaceModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'strictfp'
    ;

extendsInterfaces
    : 'extends' interfaceTypeList
    ;

interfaceTypeList
    : interfaceType (',' interfaceType)*
    ;

interfaceBody
    : '{' interfaceMemberDeclaration* '}'
    ;

interfaceMemberDeclaration
    : constantDeclaration
    | interfaceMethodDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

constantDeclaration
    : constantModifier* unannType variableDeclaratorList ';'
    ;

constantModifier
    : annotation
    | 'public'
    | 'static'
    | 'final'
    ;

interfaceMethodDeclaration
    : interfaceMethodModifier* methodHeader methodBody
    ;

interfaceMethodModifier
    : annotation
    | 'public'
    | 'abstract'
    | 'default'
    | 'static'
    | 'strictfp'
    ;

annotationTypeDeclaration
    : interfaceModifier* '@' 'interface' Identifier annotationTypeBody
    ;

annotationTypeBody
    : '{' annotationTypeMemberDeclaration* '}'
    ;

annotationTypeMemberDeclaration
    : annotationTypeElementDeclaration
    | constantDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

annotationTypeElementDeclaration
    : annotationTypeElementModifier* unannType Identifier '(' ')' dims? defaultValue? ';'
    ;

annotationTypeElementModifier
    : annotation
    | 'public'
    | 'abstract'
    ;

defaultValue
    : 'default' elementValue
    ;

annotation
    : normalAnnotation
    | '@' typeName ('(' elementValue ')')?
    ;

normalAnnotation
    : '@' typeName '(' (elementValuePair (',' elementValuePair)*)? ')'
    ;

elementValuePair
    : Identifier '=' elementValue
    ;

elementValue
    : conditionalExpression
    | elementValueArrayInitializer
    | annotation
    ;

elementValueArrayInitializer
    : '{' (elementValue (',' elementValue)*)? ','? '}'
    ;

// Blocks and Statements

block
    : '{' blockStatements? '}'
    ;

blockStatement
    : localVariableDeclarationStatement
    | classDeclaration
    | statement
    ;

localVariableDeclarationStatement
    : localVariableDeclaration ';'
    ;

localVariableDeclaration
    : variableModifier* unannType variableDeclaratorList
    ;

statement
    : statementWithoutTrailingSubstatement
    | labeledStatement
    | ifThenStatement
    | ifThenElseStatement
    | whileStatement
    | forStatement
    ;

statementNoShortIf
    : statementWithoutTrailingSubstatement
    | labeledStatementNoShortIf
    | ifThenElseStatementNoShortIf
    | whileStatementNoShortIf
    | forStatementNoShortIf
    ;

statementWithoutTrailingSubstatement
    : block
    | emptyStatement
    | expressionStatement
    | assertStatement
    | switchStatement
    | doStatement
    | breakStatement
    | continueStatement 
    | returnStatement
    | synchronizedStatement
    | throwStatement
    | tryStatement
    ;

emptyStatement
    : ';'
    ;

labeledStatement
    : Identifier ':' statement
    ;

labeledStatementNoShortIf
    : Identifier ':' statementNoShortIf
    ;

expressionStatement
    : statementExpression ';'
    ;

statementExpression
    : assignment
    | preIncDecExpression
    | postIncDecExpression
    | methodInvocation
    | classInstanceCreationExpression
    ;

ifThenStatement
    : 'if' '(' expression ')' statement
    ;

ifThenElseStatement
    : 'if' '(' expression ')' statementNoShortIf 'else' statement
    ;

ifThenElseStatementNoShortIf
    : 'if' '(' expression ')' statementNoShortIf 'else' statementNoShortIf
    ;

assertStatement
    : 'assert' expression (':' expression)? ';'
    ;

switchStatement
    : 'switch' '(' expression ')' switchBlock
    ;

switchBlock
    : '{' switchBlockStatementGroup* switchLabel* '}'
    ;

switchBlockStatementGroup
    : switchLabel+ blockStatements
    ;

switchLabel
    : 'case' (expression | Identifier) ':'
    | 'default' ':'
    ;

blockStatements
    : blockStatement+
    ;

whileStatement
    : 'while' '(' expression ')' statement
    ;

whileStatementNoShortIf
    : 'while' '(' expression ')' statementNoShortIf
    ;

doStatement
    : 'do' statement 'while' '(' expression ')' ';'
    ;

forStatement
    : basicForStatement
    | enhancedForStatement
    ;

forStatementNoShortIf
    : basicForStatementNoShortIf
    | enhancedForStatementNoShortIf
    ;

basicForStatement
    : 'for' '(' forInit? ';' expression? ';' statementExpressionList? ')' statement
    ;

basicForStatementNoShortIf
    : 'for' '(' forInit? ';' expression? ';' statementExpressionList? ')' statementNoShortIf
    ;

forInit
    : statementExpressionList
    | localVariableDeclaration
    ;

statementExpressionList
    : statementExpression (',' statementExpression)*
    ;

enhancedForStatement
    : 'for' '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statement
    ;

enhancedForStatementNoShortIf
    : 'for' '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statementNoShortIf
    ;

breakStatement
    : 'break' Identifier? ';'
    ;
    
continueStatement
    : 'continue' Identifier? ';'
    ;

returnStatement
    : 'return' expression? ';'
    ;

throwStatement
    : 'throw' expression ';'
    ;

synchronizedStatement
    : 'synchronized' '(' expression ')' block
    ;

tryStatement
    : 'try' block (catches | catches? finally_)
    | tryWithResourcesStatement
    ;

catches
    : catchClause+
    ;

catchClause
    : 'catch' '(' catchFormalParameter ')' block
    ;

catchFormalParameter
    : variableModifier* catchType variableDeclaratorId
    ;

catchType
    : unannClassType ('|' classType)*
    ;

finally_
    : 'finally' block
    ;

tryWithResourcesStatement
    : 'try' resourceSpecification block catches? finally_?
    ;

resourceSpecification
    : '(' resourceList ';'? ')'
    ;

resourceList
    : resource (';' resource)*
    ;

resource
    : variableModifier* unannType variableDeclaratorId '=' expression
    ;

// Expressions

primary
    : (primaryNoNewArray_lfno_primary | arrayCreationExpression) primaryNoNewArray_lf_primary*
    ;

primaryNoNewArray
    : literal
    | (typeName ('[' ']')* | 'void') '.' 'class'
    | (typeName '.')? 'this'
    | '(' expression ')'
    | classInstanceCreationExpression
    | fieldAccess
    | methodInvocation
    | methodReference
    | arrayAccess
    ;

primaryNoNewArray_lfno_arrayAccess
    : literal
    | (typeName ('[' ']')* | 'void') '.' 'class'
    | (typeName '.')? 'this'
    | '(' expression ')'
    | classInstanceCreationExpression
    | fieldAccess
    | methodInvocation
    | methodReference
    ;

primaryNoNewArray_lf_primary
    : classInstanceCreationExpression_lf_primary
    | '.' Identifier
    | methodInvocation_lf_primary
    | methodReference_lf_primary
    | arrayAccess_lf_primary
    ;

primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
    : classInstanceCreationExpression_lf_primary
    | '.' Identifier
    | methodInvocation_lf_primary
    | methodReference_lf_primary
    ;

primaryNoNewArray_lfno_primary
    : literal
    | ((typeName | numericTypeOrBoolean) ('[' ']')* | 'void') '.' 'class'
    | (typeName '.')? 'this'
    | '(' expression ')'
    | classInstanceCreationExpression_lfno_primary
    | fieldAccess_lfno_primary
    | methodInvocation_lfno_primary
    | methodReference_lfno_primary
    | arrayAccess_lfno_primary
    ;

primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary
    : literal
    | (typeName | numericTypeOrBoolean) ('[' ']')* '.' 'class'
    | (typeName '.')? 'this'
    | '(' expression ')'
    | classInstanceCreationExpression_lfno_primary
    | fieldAccess_lfno_primary
    | methodInvocation_lfno_primary
    | methodReference_lfno_primary
    ;

classInstanceCreationExpression
    : ('new' typeArguments? annotation* Identifier ('.' annotation* Identifier)* |
      (qualifiedName | primary) '.' 'new' typeArguments? annotation* Identifier)
      typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

classInstanceCreationExpression_lf_primary
    : '.' 'new' typeArguments? annotation* Identifier
      typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

classInstanceCreationExpression_lfno_primary
    : ('new' typeArguments? annotation* Identifier ('.' annotation* Identifier)*
      | qualifiedName '.' 'new' typeArguments? annotation* Identifier)
      typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

typeArgumentsOrDiamond
    : typeArguments
    | '<' '>'
    ;

fieldAccess
    : (primary | (typeName '.')? 'super') '.' Identifier
    ;

fieldAccess_lfno_primary
    : (typeName '.')? 'super' '.' Identifier
    ;

arrayAccess
    : (qualifiedName | primaryNoNewArray_lfno_arrayAccess) ('[' expression ']')+
    ;

arrayAccess_lf_primary
    : primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary ('[' expression ']')+
    ;

arrayAccess_lfno_primary
    : (qualifiedName | primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary) ('[' expression ']')+
    ;

methodInvocation
    : ((typeName | qualifiedName | (typeName '.')? 'super' | primary) '.' typeArguments?)?
      Identifier '(' argumentList? ')'
    ;

methodInvocation_lf_primary
    : '.' typeArguments? Identifier '(' argumentList? ')'
    ;

methodInvocation_lfno_primary
    : ((typeName | qualifiedName | (typeName '.')? 'super') '.' typeArguments?)?
      Identifier '(' argumentList? ')'
    ;

argumentList
    : expression (',' expression)*
    ;

methodReference
    : (qualifiedName | referenceType | (typeName '.')? 'super' | primary) '::' typeArguments? Identifier
    | classType '::' typeArguments? 'new'
    | arrayType '::' 'new'
    ;

methodReference_lf_primary
    : '::' typeArguments? Identifier
    ;

methodReference_lfno_primary
    : (qualifiedName | referenceType | (typeName '.')? 'super') '::' typeArguments? Identifier
    | classType '::' typeArguments? 'new'
    | arrayType '::' 'new'
    ;

arrayCreationExpression
    : 'new' (primitiveType | classOrInterfaceType) (dimExprs dims? | dims arrayInitializer)
    ;

dimExprs
    : dimExpr+
    ;

dimExpr
    : annotation* '[' expression ']'
    ;

expression
    : lambdaExpression
    | conditionalExpression
    | assignment
    ;

lambdaParameters
    : Identifier
    | '(' formalParameterList? ')'
    | '(' Identifier (',' Identifier)* ')'
    ;

lambdaBody
    : expression
    | block
    ;

leftHandSide
    : qualifiedName
    | fieldAccess
    | arrayAccess
    ;

assignment
    : leftHandSide assignmentOperator expression
    ;

assignmentOperator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    ;

conditionalExpression
    : unaryExpression

    | conditionalExpression op=('*' | '/' | '%') conditionalExpression
    | conditionalExpression op=('+' | '-') conditionalExpression
    | conditionalExpression ('<' '<' | '>' '>' | '>' '>' '>') conditionalExpression
    | conditionalExpression op=('<' | '>' | '<=' | '>=') conditionalExpression
    | conditionalExpression op='instanceof' referenceType
    | conditionalExpression op=('==' | '!=') conditionalExpression
    | conditionalExpression op='&' conditionalExpression
    | conditionalExpression op='^' conditionalExpression
    | conditionalExpression op='|' conditionalExpression
    | conditionalExpression op='&&' conditionalExpression
    | conditionalExpression op='||' conditionalExpression
    | conditionalExpression op='?' expression ':' conditionalExpression
    ;

unaryExpression
    : preIncDecExpression
    | op=('+' | '-') unaryExpression
    | unaryExpressionNotPlusMinus
    ;
    
preIncDecExpression
    : op=('++' | '--') postfixExpression
    ;
    
postIncDecExpression
    : postfixExpression op=('++' | '--')
    ;

unaryExpressionNotPlusMinus
    : postfixExpression
    | op=('~' | '!') conditionalExpression
    | castExpression
    ;
    
postfixExpression
    : (primary | qualifiedName) op=('++' | '--')*
    ;
    
castExpression
    : '(' primitiveType ')' unaryExpression
    | '(' referenceType additionalBound* ')' (unaryExpressionNotPlusMinus | lambdaExpression)
    ;

lambdaExpression
    : lambdaParameters '->' lambdaBody
    ;

// Arrays

arrayInitializer
    : '{' (variableInitializer (',' variableInitializer)*)? ','? '}'
    ;

// Names

typeName
    : (qualifiedName '.')? Identifier
    ;

qualifiedName
    : Identifier ('.' Identifier)*
    ;

// Types, Values, and Variables

type
    : primitiveType
    | referenceType
    ;

primitiveType
    : annotation* numericTypeOrBoolean
    ;

numericTypeOrBoolean
    : 'boolean'
    | integralType
    | floatingPointType
    ;

integralType
    : 'byte'
    | 'short'
    | 'int'
    | 'long'
    | 'char'
    ;

floatingPointType
    : 'float'
    | 'double'
    ;

referenceType
    : classOrInterfaceType
    | typeVariable
    | arrayType
    ;

arrayType
    : (primitiveType | classOrInterfaceType | typeVariable) dims
    ;

dims
    : annotation* '[' ']' (annotation* '[' ']')*
    ;

typeParameter
    : annotation* Identifier ('extends' (typeVariable | classOrInterfaceType additionalBound*))?
    ;

classOrInterfaceType
    : classType_lfno_classOrInterfaceType ('.' classType_lfno_classOrInterfaceType)*
    ;

classType_lfno_classOrInterfaceType
    : annotation* Identifier typeArguments?
    ;

additionalBound
    : '&' interfaceType
    ;
    
interfaceType
    : classType
    ;

classType
    : (classOrInterfaceType '.')? annotation* Identifier typeArguments?
    ;

typeArguments
    : '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    : referenceType
    | annotation* '?' (('extends' | 'super') referenceType)?
    ;

// Lexical Structure

literal
    : IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | StringLiteral
    | NullLiteral
    ;

