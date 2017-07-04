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
    : 'import' (typeName | packageOrTypeName '.' '*') ';'
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
    : '<' typeParameterList '>'
    ;

typeParameterList
    : typeParameter (',' typeParameter)*
    ;

superclass
    : 'extends' classType
    ;

superinterfaces
    : 'implements' interfaceTypeList
    ;

interfaceTypeList
    : interfaceType (',' interfaceType)*
    ;

classBody
    : '{' classBodyDeclaration* '}'
    ;

classBodyDeclaration
    : classMemberDeclaration
    | instanceInitializer
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
    : unannPrimitiveType
    | unannReferenceType
    ;

unannPrimitiveType
    : numericType
    | 'boolean'
    ;

unannReferenceType
    : unannClassOrInterfaceType
    | unannTypeVariable
    | unannArrayType
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

unannTypeVariable
    : Identifier
    ;

unannArrayType
    : unannPrimitiveType dims
    | unannClassOrInterfaceType dims
    | unannTypeVariable dims
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
    : result methodDeclarator throws_?
    | typeParameters annotation* result methodDeclarator throws_?
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

instanceInitializer
    : block
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
    : typeArguments? 'this' '(' argumentList? ')' ';'
    | typeArguments? 'super' '(' argumentList? ')' ';'
    | expressionName '.' typeArguments? 'super' '(' argumentList? ')' ';'
    | primary '.' typeArguments? 'super' '(' argumentList? ')' ';'
    ;

enumDeclaration
    : classModifier* 'enum' Identifier superinterfaces? enumBody
    ;

enumBody
    : '{' enumConstantList? ','? enumBodyDeclarations? '}'
    ;

enumConstantList
    : enumConstant (',' enumConstant)*
    ;

enumConstant
    : enumConstantModifier* Identifier ('(' argumentList? ')')? classBody?
    ;

enumConstantModifier
    : annotation
    ;

enumBodyDeclarations
    : ';' classBodyDeclaration*
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
    | markerAnnotation
    | singleElementAnnotation
    ;

normalAnnotation
    : '@' typeName '(' elementValuePairList? ')'
    ;

elementValuePairList
    : elementValuePair (',' elementValuePair)*
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
    
markerAnnotation
    : '@' typeName
    ;

singleElementAnnotation
    : '@' typeName '(' elementValue ')'
    ;

// Arrays

arrayInitializer
    : '{' (variableInitializer (',' variableInitializer)*)? ','? '}'
    ;

// Blocks and Statements

block
    : '{' blockStatements? '}'
    ;

blockStatements
    : blockStatement+
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
    : 'try' block catches
    | 'try' block catches? finally_
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
    | typeName ('[' ']')* '.' 'class'
    | 'void' '.' 'class'
    | 'this'
    | typeName '.' 'this'
    | '(' expression ')'
    | classInstanceCreationExpression
    | fieldAccess
    | arrayAccess
    | methodInvocation
    | methodReference
    ;

primaryNoNewArray_lfno_arrayAccess
    : literal
    | typeName ('[' ']')* '.' 'class'
    | 'void' '.' 'class'
    | 'this'
    | typeName '.' 'this'
    | '(' expression ')'
    | classInstanceCreationExpression
    | fieldAccess
    | methodInvocation
    | methodReference
    ;

primaryNoNewArray_lf_primary
    : classInstanceCreationExpression_lf_primary
    | fieldAccess_lf_primary
    | arrayAccess_lf_primary
    | methodInvocation_lf_primary
    | methodReference_lf_primary
    ;

primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
    : classInstanceCreationExpression_lf_primary
    | fieldAccess_lf_primary
    | methodInvocation_lf_primary
    | methodReference_lf_primary
    ;

primaryNoNewArray_lfno_primary
    : literal
    | typeName ('[' ']')* '.' 'class'
    | unannPrimitiveType ('[' ']')* '.' 'class'
    | 'void' '.' 'class'
    | 'this'
    | typeName '.' 'this'
    | '(' expression ')'
    | classInstanceCreationExpression_lfno_primary
    | fieldAccess_lfno_primary
    | arrayAccess_lfno_primary
    | methodInvocation_lfno_primary
    | methodReference_lfno_primary
    ;

primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary
    : literal
    | typeName ('[' ']')* '.' 'class'
    | unannPrimitiveType ('[' ']')* '.' 'class'
    | 'void' '.' 'class'
    | 'this'
    | typeName '.' 'this'
    | '(' expression ')'
    | classInstanceCreationExpression_lfno_primary
    | fieldAccess_lfno_primary
    | methodInvocation_lfno_primary
    | methodReference_lfno_primary
    ;

classInstanceCreationExpression
    : 'new' typeArguments? annotation* Identifier ('.' annotation* Identifier)* typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    | expressionName '.' 'new' typeArguments? annotation* Identifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    | primary '.' 'new' typeArguments? annotation* Identifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

classInstanceCreationExpression_lf_primary
    : '.' 'new' typeArguments? annotation* Identifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

classInstanceCreationExpression_lfno_primary
    : 'new' typeArguments? annotation* Identifier ('.' annotation* Identifier)* typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    | expressionName '.' 'new' typeArguments? annotation* Identifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
    ;

typeArgumentsOrDiamond
    : typeArguments
    | '<' '>'
    ;

fieldAccess
    : primary '.' Identifier
    | 'super' '.' Identifier
    | typeName '.' 'super' '.' Identifier
    ;

fieldAccess_lf_primary
    : '.' Identifier
    ;

fieldAccess_lfno_primary
    : (typeName '.')? 'super' '.' Identifier
    ;

arrayAccess
    : (expressionName | primaryNoNewArray_lfno_arrayAccess) ('[' expression ']')+
    ;

arrayAccess_lf_primary
    : primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary ('[' expression ']')+
    ;

arrayAccess_lfno_primary
    : (expressionName | primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary) ('[' expression ']')+
    ;

methodInvocation
    : Identifier '(' argumentList? ')'
    | typeName '.' typeArguments? Identifier '(' argumentList? ')'
    | expressionName '.' typeArguments? Identifier '(' argumentList? ')'
    | primary '.' typeArguments? Identifier '(' argumentList? ')'
    | 'super' '.' typeArguments? Identifier '(' argumentList? ')'
    | typeName '.' 'super' '.' typeArguments? Identifier '(' argumentList? ')'
    ;

methodInvocation_lf_primary
    : '.' typeArguments? Identifier '(' argumentList? ')'
    ;

methodInvocation_lfno_primary
    : Identifier '(' argumentList? ')'
    | typeName '.' typeArguments? Identifier '(' argumentList? ')'
    | expressionName '.' typeArguments? Identifier '(' argumentList? ')'
    | 'super' '.' typeArguments? Identifier '(' argumentList? ')'
    | typeName '.' 'super' '.' typeArguments? Identifier '(' argumentList? ')'
    ;

argumentList
    : expression (',' expression)*
    ;

methodReference
    : expressionName '::' typeArguments? Identifier
    | referenceType '::' typeArguments? Identifier
    | primary '::' typeArguments? Identifier
    | 'super' '::' typeArguments? Identifier
    | typeName '.' 'super' '::' typeArguments? Identifier
    | classType '::' typeArguments? 'new'
    | arrayType '::' 'new'
    ;

methodReference_lf_primary
    : '::' typeArguments? Identifier
    ;

methodReference_lfno_primary
    : expressionName '::' typeArguments? Identifier
    | referenceType '::' typeArguments? Identifier
    | 'super' '::' typeArguments? Identifier
    | typeName '.' 'super' '::' typeArguments? Identifier
    | classType '::' typeArguments? 'new'
    | arrayType '::' 'new'
    ;

arrayCreationExpression
    : 'new' primitiveType dimExprs dims?
    | 'new' classOrInterfaceType dimExprs dims?
    | 'new' primitiveType dims arrayInitializer
    | 'new' classOrInterfaceType dims arrayInitializer
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

lambdaExpression
    : lambdaParameters '->' lambdaBody
    ;

lambdaParameters
    : Identifier
    | '(' formalParameterList? ')'
    | '(' inferredFormalParameterList ')'
    ;

inferredFormalParameterList
    : Identifier (',' Identifier)*
    ;

lambdaBody
    : expression
    | block
    ;

assignment
    : leftHandSide assignmentOperator expression
    ;

leftHandSide
    : expressionName
    | fieldAccess
    | arrayAccess
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
    : (primary | expressionName) op=('++' | '--')*
    ;
    
castExpression
    : '(' primitiveType ')' unaryExpression
    | '(' referenceType additionalBound* ')' (unaryExpressionNotPlusMinus | lambdaExpression)
    ;

// Names

packageName
    : Identifier ('.' Identifier)*
    ;

typeName
    : (packageOrTypeName '.')? Identifier
    ;

packageOrTypeName
    : Identifier ('.' Identifier)*
    ;

expressionName
    : Identifier ('.' Identifier)*
    ;

// Types, Values, and Variables

type
    : primitiveType
    | referenceType
    ;

primitiveType
    : annotation* (numericType | 'boolean')
    ;

numericType
    : integralType
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

classOrInterfaceType
    : classType_lfno_classOrInterfaceType ('.' classType_lfno_classOrInterfaceType)*
    ;

classType
    : (classOrInterfaceType '.')? annotation* Identifier typeArguments?
    ;
    
classType_lfno_classOrInterfaceType
    : annotation* Identifier typeArguments?
    ;

arrayType
    : primitiveType dims
    | classOrInterfaceType dims
    | typeVariable dims
    ;

dims
    : annotation* '[' ']' (annotation* '[' ']')*
    ;

typeParameter
    : annotation* Identifier ('extends' (typeVariable | classOrInterfaceType additionalBound*))?
    ;

additionalBound
    : '&' interfaceType
    ;
    
interfaceType
    : classType
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

