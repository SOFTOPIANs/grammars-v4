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

parser grammar SwiftParser;

options {
	tokenVocab = SwiftLexer;
	superClass = SwiftBaseParser;
}

top_level : statements? EOF;

statements : statement+;

code_block : '{' statements? '}';

// Statements
statement: //: expression ';'?
	declaration ';'?
	/*| loop_statement ';'?
 | branch_statement ';'?
 | labeled_statement ';'?
 | control_transfer_statement ';'?
 |
	 defer_statement ';'?
 | do_statement ';'?
 | compiler_control_statement ';'? // proper logic with semicolons is
	 not
 supported yet. compiler_control_statement should be separated with a newline, but not with a semicolon
	 */
	;

// Declarations
declaration : variable_declaration;

variable_declaration : VAR identifier ASSIGN literal;

// Expressions
expression_list : expression (',' expression)*;

expression : try_operator?; //prefix_expression binary_expressions?;

/*
 type:
 array_type # the_array_type
 | dictionary_type # the_dictionary_type
 | function_type # the_function_type
 |
 type_identifier # the_type_identifier
 | tuple_type # the_tuple_type
 | type '?' # the_optional_type
 | type '!' #
 the_implicitly_unwrapped_optional_type
 | protocol_composition_type # the_protocol_composition_type
 | type '.' 'Type'
 # the_metatype_type_type
 | type '.' 'Protocol' # the_metatype_protocol_type
 | 'Any' # the_any_type
 | 'Self' #
 the_self_type
 ;
 */

// Operators

try_operator : TRY (QUESTION | BANG)?;

// Identifiers and Literals

identifier : IDENTIFIER_TOKEN | BREAK;

identifier_list : identifier (DOT identifier)*;

integer : BINARY_LIT | DECIMAL_LIT | OCTAL_LIT | HEX_LIT;

literal : BOOL_LIT | numeric_literal | NIL_LIT | string;

// TODO : Fix predicate for correct processing for negative numbers
numeric_literal : SUB? (integer | FLOAT_LIT);

// TODO: Add multiline string support
string : RAW_STRING_LIT;

