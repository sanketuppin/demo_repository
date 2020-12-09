%{

#include <cstdio>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "ast_headers.h"
#include "functions.c"
using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;


%}

%token	IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	TYPEDEF_NAME ENUMERATION_CONSTANT
%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY 
%token	STRUCT UNION ENUM ELLIPSIS
%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL


%start translation_unit
%%

primary_expression
	: IDENTIFIER 						{ $$ = createnode_two(NULL,NULL,IDEN,(char *)"ID");} 
	| constant
	| string
	| '(' expression ')'
	| generic_selection
	;

constant
	: I_CONSTANT		/* includes character_constant */
	| F_CONSTANT
	| ENUMERATION_CONSTANT	/* after it has been defined as such */
	;

enumeration_constant		/* before it has been defined as such */
	: IDENTIFIER
	;

string
	: STRING_LITERAL
	| FUNC_NAME
	;

generic_selection
	: GENERIC '(' assignment_expression ',' generic_assoc_list ')'
	;

generic_assoc_list
	: generic_association
	| generic_assoc_list ',' generic_association
	;

generic_association
	: type_name ':' assignment_expression
	| DEFAULT ':' assignment_expression
	;

postfix_expression
	: primary_expression 						{ $$ = createnode_two($1,NULL,NONE,(char *)"");} 
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}'
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

unary_expression
	: postfix_expression 						{ $$ = createnode_two($1,NULL,NONE,(char *)"");} 
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	| ALIGNOF '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

cast_expression
	: unary_expression 								{ $$ = createnode_two($1,NULL,NONE,(char *)"");} 
	| '(' type_name ')' cast_expression
	;

multiplicative_expression
	: cast_expression 								{ $$ = createnode_two($1,NULL,NONE,(char *)"");} 
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

additive_expression
	: multiplicative_expression						{ $$ = createnode_two($1,NULL,NONE,(char *)"");} 
	| additive_expression '+' multiplicative_expression   { 
													ast_node *addop = createnode_two(NULL,NULL,ADDOP,(char *)" + ");
													$$ = createnode_three($1,addop,$3,NONE,(char *)"");
													}
	| additive_expression '-' multiplicative_expression  { 
													ast_node *subop = createnode_two(NULL,NULL,SUBOP,(char *)" - ");
													$$ = createnode_three($1,subop,$3,NONE,(char *)"");
													}

shift_expression
	: additive_expression  				{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	;

relational_expression
	: shift_expression 					{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	;

equality_expression
	: relational_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

and_expression
	: equality_expression 				{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| and_expression '&' equality_expression
	;

exclusive_or_expression
	: and_expression 					{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| exclusive_or_expression '^' and_expression
	;

inclusive_or_expression
	: exclusive_or_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| inclusive_or_expression '|' exclusive_or_expression
	;

logical_and_expression
	: inclusive_or_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| logical_and_expression AND_OP inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| logical_or_expression OR_OP logical_and_expression
	;

conditional_expression
	: logical_or_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expression
	: assignment_expression 					{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| expression ',' assignment_expression 
	;

constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	| static_assert_declaration
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers
	| storage_class_specifier
	| type_specifier declaration_specifiers 
	| type_specifier 							{ $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)"");}
	| type_qualifier declaration_specifiers
	| type_qualifier
	| function_specifier declaration_specifiers
	| function_specifier
	| alignment_specifier declaration_specifiers
	| alignment_specifier
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator '=' initializer
	| declarator
	;

storage_class_specifier
	: TYPEDEF	                            { $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"TYPEDEF ");}      
	| EXTERN 								{ $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"EXTERN ");}
	| STATIC 								{ $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"STATIC ");}
	| THREAD_LOCAL 							{ $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"THREAD_LOCAL ");}
	| AUTO 									{ $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"AUTO ");}
	| REGISTER 								{ $$ = createnode_two(NULL,NULL,STRG_CLASS_T,(char *)"REGISTER ");}
	;

type_specifier
	: VOID 									{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"VOID ");}
	| CHAR 									{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"CHAR ");}
	| SHORT 								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"SHORT ");}
	| INT 									{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"INT ");}
	| LONG 									{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"LONG ");}
	| FLOAT 								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"FLOAT ");}
	| DOUBLE 								
	| SIGNED
	| UNSIGNED
	| BOOL
	| COMPLEX
	| IMAGINARY
	| atomic_type_specifier                 { $$ = createnode_two($1,NULL,TYPE_SPEC_NT,(char *)"");}
	| struct_or_union_specifier 			{ $$ = createnode_two($1,NULL,TYPE_SPEC_NT,(char *)"");}
	| enum_specifier 						{ $$ = createnode_two($1,NULL,TYPE_SPEC_NT,(char *)"");}
	| TYPEDEF_NAME		                    { $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"TYPE_DEFNAME");}
	;

struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list ';'
	| static_assert_declaration
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| declarator ':' constant_expression
	| declarator
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC '(' type_name ')'
	;

type_qualifier
	: CONST  								{ $$ = createnode_two(NULL,NULL,TYPE_QUAL_T,(char *)"CONST ");}
	| RESTRICT 								{ $$ = createnode_two(NULL,NULL,TYPE_QUAL_T,(char *)"RESTRICT ");}
	| VOLATILE 								{ $$ = createnode_two(NULL,NULL,TYPE_QUAL_T,(char *)"VOLATILE ");}
	| ATOMIC 								{ $$ = createnode_two(NULL,NULL,TYPE_QUAL_T,(char *)"ATOMIC ");}
	;

function_specifier
	: INLINE 					 			{ $$ = createnode_two(NULL,NULL,FUNC_SPEC_T,(char *)"INLINE ");}	
	| NORETURN 								{ $$ = createnode_two(NULL,NULL,FUNC_SPEC_T,(char *)"NORETURN ");}
	; 

alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	;

declarator
	: pointer direct_declarator
	| direct_declarator 					{ $$ = createnode_two($1,NULL,DECL,(char *)"");}
	;

direct_declarator
	: IDENTIFIER 							{ $$ = createnode_two(NULL,NULL,IDEN,(char *)"ID"); }
	| '(' declarator ')'
	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']'
	| direct_declarator '(' parameter_type_list ')'    {  
							ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
							ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
							$$ = createnode_four($1,open,$3,close,DIRECT_DECL,(char *)"");
						}
	| direct_declarator '(' ')'
	| direct_declarator '(' identifier_list ')'
	;

pointer
	: '*' type_qualifier_list pointer
	| '*' type_qualifier_list
	| '*' pointer
	| '*'
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list ',' ELLIPSIS
	| parameter_list 							{$$ = createnode_two($1,NULL,PARAM_TYPE_LIST,(char *)"");}
	;

parameter_list
	: parameter_declaration          {
										$$ = createnode_two($1,NULL,PARAM_LIST,(char *)"");
									 }
	| parameter_list ',' parameter_declaration  { 
							ast_node *comma = createnode_two(NULL,NULL,COMMA,(char *)",");
							$$ = createnode_three($1,comma,$3,PARAM_LIST,(char *)"");
							}
	;


parameter_declaration
	: declaration_specifiers declarator        {
													$$ = createnode_two($1,$2,PARAM_DECL,(char *)"");
												}
	| declaration_specifiers abstract_declarator {
													$$ = createnode_two($1,$2,PARAM_DECL,(char *)"");
												}
	| declaration_specifiers                   { $$ = createnode_two($1,NULL,PARAM_DECL,(char *)""); }
	;


identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' '*' ']'
	| '[' STATIC type_qualifier_list assignment_expression ']'
	| '[' STATIC assignment_expression ']'
	| '[' type_qualifier_list STATIC assignment_expression ']'
	| '[' type_qualifier_list assignment_expression ']'
	| '[' type_qualifier_list ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' '*' ']'
	| direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	| assignment_expression
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list ',' designation initializer
	| initializer_list ',' initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' IDENTIFIER
	;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;

statement
	: labeled_statement 		{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| compound_statement 		{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| expression_statement		{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| selection_statement  		{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| iteration_statement 		{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| jump_statement 			{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	;

labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}' 						{ ast_node *open = createnode_two(NULL,NULL,OPEN_CURLPARAN,(char *)"{");
									  ast_node *close = createnode_two(NULL,NULL,CLOSE_CURLPARAN,(char *)"}");
									  $$ = createnode_two(open,close,CMPD_STMT,(char *)"");
									 }
	| '{'  block_item_list '}'       { ast_node *open = createnode_two(NULL,NULL,OPEN_CURLPARAN,(char *)"{");
									   ast_node *close = createnode_two(NULL,NULL,CLOSE_CURLPARAN,(char *)"}");
									   $$ = createnode_three(open,$2,close,CMPD_STMT,(char *)"");
									 }
	;

block_item_list
	: block_item 					{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| block_item_list block_item    { $$ = createnode_two($1,$2,NONE,(char *)"");}
	;

block_item             				
	: declaration                   { $$ = createnode_two($1,NULL,NONE,(char *)"");}
	| statement 					{ $$ = createnode_two($1,NULL,NONE,(char *)"");}
	;


expression_statement
	: ';'							{ $$ = createnode_two(NULL,NULL,SEMICOLON,(char *)";");}
	| expression ';'        		{ 
	 								  ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
									  $$ = createnode_two($1,semicolon,NONE,(char *)"");
									}
	;


selection_statement
	: IF '(' expression ')' statement ELSE statement
	| IF '(' expression ')' statement
	| SWITCH '(' expression ')' statement
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement
	;

jump_statement
	: GOTO IDENTIFIER ';'      {
									ast_node *goto1 = createnode_two(NULL,NULL,GOTO1,(char *)"GOTO");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
									ast_node *iden = createnode_two(NULL,NULL,IDEN,(char *)"IDEN");
								    $$ = createnode_three(goto1,iden,semicolon,NONE,(char *)"");
								}
	| CONTINUE ';' 				{
									ast_node *continue1 = createnode_two(NULL,NULL,CONT,(char *)"CONTINUE");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(continue1,semicolon,NONE,(char *)"");
								}
	| BREAK ';'					{
									ast_node *break1 = createnode_two(NULL,NULL,BRK,(char *)"BREAK");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(break1,semicolon,NONE,(char *)"");
								}
	| RETURN ';'				{
									ast_node *return1 = createnode_two(NULL,NULL,RET,(char *)"RETURN ");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(return1,semicolon,NONE,(char *)"");
								}
	| RETURN expression ';'     { ast_node *return1 = createnode_two(NULL,NULL,RET,(char *)"RETURN ");
								  ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								  $$ = createnode_three(return1,$2,semicolon,NONE,(char *)"");
								}
	;

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement {
	 											ast_node *func_def = createnode_four($1,$2,$3,$4,FUNCDEF,(char *)"");
	 											dump_ast(func_def);
	   											}
	| declaration_specifiers declarator compound_statement {
	 											ast_node *func_def = createnode_three($1,$2,$3,FUNCDEF,(char *)"");
	 											dump_ast(func_def);
	   											}
	; 

declaration_list
	: declaration
	| declaration_list declaration
	;

%%


