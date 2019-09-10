/*
 * [The "BSD license"]
 Copyright (c) 2014 Terence Parr
 All rights reserved.

 Redistribution and use in source and
 * binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 1.
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following
 * disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions
 * and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 3. The
 * name of the author may not be used to endorse or promote products
 derived from this software without specific prior
 * written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE

 Converted from Apple's doc, http://tinyurl.com/n8rkoue, to
 * ANTLR's
 meta-language.
 */

lexer grammar SwiftLexer;

// Declarations and Type Keywords

ASSOCIATED_TYPE	:	'associatedtype';
CLASS			:	'class';
DEINIT			:	'deinit';
DESTRUCTOR		:	'destructor';
ENUM			:	'enum';
EXTENSION		:	'extension';
FILEPRIVATE		:	'fileprivate';
FUNC			:	'func';
IMPORT			:	'import';
INIT			:	'init';
INOUT			:	'inout';
INTERNAL		:	'internal';
LET				:	'let';
OPEN			:	'open';
OPERATOR		:	'operator';
PRIVATE			:	'private';
PROTOCOL		:	'protocol';
PUBLIC			:	'public';
STATIC			:	'static';
STRUCT			:	'struct';
SUBSCRIPT		:	'subscript';
TYPE_CAPITAL	:	'Type';
TYPEALIAS		:	'typealias';
VAR				:	'var';

// Statements Keywords
BREAK		:	'break';
CASE		:	'case';
CONTINUE	:	'continue';
DEFAULT		:	'default';
DEFER		:	'defer';
DO			:	'do';
ELSE		:	'else';
FALLTHROUGH	:	'fallthrough';
FOR			:	'for';
GUARD		:	'guard';
IF			:	'if';
IN			:	'in';
REPEAT		:	'repeat';
RETURN		:	'return';
SWITCH		:	'switch';
THEN		:	'then';
WHERE		:	'where';
WHILE		:	'while';

ANY				:	'any';
AS				:	'as';
CATCH			:	'catch';
IS				:	'is';
NEW				:	'new';
RETHROWS		:	'rethrows';
SELF_CAPITAL	:	'Self';
SELF_LOWER		:	'self';
SUPER			:	'super';
THROW			:	'throw';
THROWS			:	'throws';
TRY				:	'try';
TYPE_LOWER		:	'type';

// Directives Keywords

IF_DIRECTIVE		:	'#if';
ELSEIF_DIRECTIVE	:	'#elseif';
ELSE_DIRECTIVE		:	'#else';
ENDIF_DIRECTIVE		:	'#endif';

// Punctuation

DOT			:	'.';
LCURLY		:	'{';
LPAREN		:	'(';
LBRACK		:	'[';
RCURLY		:	'}';
RPAREN		:	')';
RBRACK		:	']';
COMMA		:	',';
COLON		:	':';
SEMI		:	';';
LT			:	'<';
GT			:	'>';
UNDERSCORE	:	'_';
BANG		:	'!';
QUESTION	:	'?';
AT			:	'@';
AND			:	'&';
SUB			:	'-';
ASSIGN		:	'=';
OR			:	'|';
DIV			:	'/';
ADD			:	'+';
MUL			:	'*';
MOD			:	'%';
CARET		:	'^';
TILDE		:	'~';

// Hidden tokens

COMMENT			:	'/*' (COMMENT | .)*? '*/'		-> channel(HIDDEN);
LINE_COMMENT	:	'//' ~[\r\n]*					-> channel(HIDDEN);
TERMINATOR		:	[\r\n]+							-> channel(HIDDEN);
WS				:	[ \n\r\t\u000B\u000C\u0000]+	-> channel(HIDDEN);

IDENTIFIER_TOKEN:
	'`'? IDENTIFIER_HEAD IDENTIFIER_CHARACTERS? '`'?
	| '$' PURE_DECIMAL_DIGIT+
	;

// Number literals
BINARY_LIT	:	'0b' PURE_BINARY_DIGIT BINARY_DIGIT*;
DECIMAL_LIT	:	'0'? PURE_DECIMAL_DIGIT DECIMAL_DIGIT*;
OCTAL_LIT	:	'0o' PURE_OCTAL_DIGIT OCTAL_DIGIT*;
HEX_LIT		:	'0x' PURE_HEX_DIGIT HEX_DIGIT*;

NIL_LIT : 'nil';

RAW_STRING_LIT : '"' ESCAPED_VALUE* '"';

// Fragments

fragment ESCAPED_VALUE:
	'\\' [0\\tnr"']
	| '\\x' HEX_DIGIT HEX_DIGIT
	| '\\u' '{' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT '}'
	| '\\u' '{' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT '}'
	| ~["\n\r\\]
	;

fragment PURE_BINARY_DIGIT	:	[01];
fragment BINARY_DIGIT		:	PURE_BINARY_DIGIT | '_';

fragment PURE_DECIMAL_DIGIT	:	[0-9];
fragment DECIMAL_DIGIT		:	[0-9] | '_';

fragment PURE_HEX_DIGIT	:	[0-9a-fA-F];
fragment HEX_DIGIT		:	PURE_HEX_DIGIT | '-';

fragment PURE_OCTAL_DIGIT	:	[0-7];
fragment OCTAL_DIGIT		:	PURE_OCTAL_DIGIT | '_';

fragment IDENTIFIER_HEAD:
	[a-zA-Z]
	| '_'
	| '\u00A8'
	| '\u00AA'
	| '\u00AD'
	| '\u00AF'
	| [\u00B2-\u00B5]
	| [\u00B7-\u00BA]
	| [\u00BC-\u00BE]
	| [\u00C0-\u00D6]
	| [\u00D8-\u00F6]
	| [\u00F8-\u00FF]
	| [\u0100-\u02FF]
	| [\u0370-\u167F]
	| [\u1681-\u180D]
	| [\u180F-\u1DBF]
	| [\u1E00-\u1FFF]
	| [\u200B-\u200D]
	| [\u202A-\u202E]
	| [\u203F-\u2040]
	| '\u2054'
	| [\u2060-\u206F]
	| [\u2070-\u20CF]
	| [\u2100-\u218F]
	| [\u2460-\u24FF]
	| [\u2776-\u2793]
	| [\u2C00-\u2DFF]
	| [\u2E80-\u2FFF]
	| [\u3004-\u3007]
	| [\u3021-\u302F]
	| [\u3031-\u303F]
	| [\u3040-\uD7FF]
	| [\uF900-\uFD3D]
	| [\uFD40-\uFDCF]
	| [\uFDF0-\uFE1F]
	| [\uFE30-\uFE44]
	| [\uFE47-\uFFFD]
	/*
	 | U+10000-U+1FFFD | U+20000-U+2FFFD | U+30000-U+3FFFD | U+40000-U+4FFFD
 | U+50000-U+5FFFD |
 U+60000-U+6FFFD
 |
	 U+70000-U+7FFFD | U+80000-U+8FFFD
 | U+90000-U+9FFFD | U+A0000-U+AFFFD |
 U+B0000-U+BFFFD | U+C0000-U+CFFFD
 |
	 U+D0000-U+DFFFD or U+E0000-U+EFFFD
	 */
	;

fragment IDENTIFIER_CHARACTER:
	[0-9]
	| [\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE20-\uFE2F]
	| IDENTIFIER_HEAD
	;

fragment IDENTIFIER_CHARACTERS : IDENTIFIER_HEAD IDENTIFIER_CHARACTERS?;