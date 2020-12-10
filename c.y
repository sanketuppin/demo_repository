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
	| constant 							{ $$ = createnode_two($1,NULL,PRIM_EXP,(char *)"");}
	| string 							{ $$ = createnode_two($1,NULL,PRIM_EXP,(char *)"");}
	| '(' expression ')'  				{ ast_node *openbrace = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
										  ast_node *closebrace = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
										  $$ = createnode_three(openbrace,$2,closebrace,PRIM_EXP,(char *)"");
										}
	| generic_selection 				{ $$ = createnode_two($1,NULL,PRIM_EXP,(char *)"");}
	;

constant
	: I_CONSTANT						{ $$ = createnode_two(NULL,NULL,ICONST,(char *)"I_CONSTANT");}
	| F_CONSTANT 						{ $$ = createnode_two(NULL,NULL,FCONST,(char *)"F_CONSTANT");}
	| ENUMERATION_CONSTANT	 			{ $$ = createnode_two(NULL,NULL,ENUM_CONST,(char *)"ENUM_CONSTANT");}


enumeration_constant		/* before it has been defined as such */
	: IDENTIFIER 						{ $$ = createnode_two(NULL,NULL,IDEN,(char *)"ID");} 
	;

string
	: STRING_LITERAL 					{ $$ = createnode_two(NULL,NULL,STRING1,(char *)"STRING_LITERAL");}
	| FUNC_NAME 						{ $$ = createnode_two(NULL,NULL,FUNCNAME,(char *)"STRING_LITERAL");}
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
	: primary_expression							{ $$ = createnode_two($1,NULL,POSTFIX_EXP,(char *)"");}

	| postfix_expression '[' expression ']'         { ast_node *open_sqbrace = 
														createnode_two(NULL,NULL,OPEN_SQPARAN,(char *)"[");
											  	  	  ast_node *close_sqbrace = 
											  	  	  	createnode_two(NULL,NULL,CLOSE_SQPARAN,(char *)"]");
									$$ = createnode_four($1,open_sqbrace,$3,close_sqbrace,POSTFIX_EXP,(char *)"");
													}

	| postfix_expression '(' ')'		{ ast_node *open_brace =
													createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
										  ast_node *close_brace = 
										  			createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
										  $$ = createnode_three($1,open_brace,close_brace,POSTFIX_EXP,(char *)"");
										}
	| postfix_expression '(' argument_expression_list ')'
								{ast_node *open_brace = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
								 ast_node *close_brace = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								 $$ = createnode_four($1,open_brace,$3,close_brace,POSTFIX_EXP,(char *)"");
								}
	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}'
	;

argument_expression_list
	: assignment_expression 				{ $$ = createnode_two($1,NULL,ARG_EXPR_LIST,(char *)"");}
	| argument_expression_list ',' assignment_expression 
											{ ast_node *comma = createnode_two(NULL,NULL,COMMA,(char *)",");
											  $$ = createnode_three($1,comma,$3,ARG_EXPR_LIST,(char *)"");
											}
	;

unary_expression
	: postfix_expression 						{ $$ = createnode_two($1,NULL,UNARY_EXPR,(char *)"");} 
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
	: cast_expression 								{ $$ = createnode_two($1,NULL,MUL_EXPR,(char *)"");}

	| multiplicative_expression '*' cast_expression { ast_node *mulop = createnode_two(NULL,NULL,MULOP,(char *)"*");
										  			  $$ = createnode_three($1,mulop,$3,MUL_EXPR,(char *)"");
													}

	| multiplicative_expression '/' cast_expression { ast_node *divop = createnode_two(NULL,NULL,DIVOP,(char *)"/");
										  			  $$ = createnode_three($1,divop,$3,MUL_EXPR,(char *)"");
													}    

	| multiplicative_expression '%' cast_expression { 
													ast_node *modop = createnode_two(NULL,NULL,MODOP,(char *)" % ");
													$$ = createnode_three($1,modop,$3,MUL_EXPR,(char *)"");
													}
	;


additive_expression
	: multiplicative_expression						{ $$ = createnode_two($1,NULL,ADD_EXPR,(char *)"");} 
	| additive_expression '+' multiplicative_expression   { 
													ast_node *addop = createnode_two(NULL,NULL,ADDOP,(char *)" + ");
													$$ = createnode_three($1,addop,$3,ADD_EXPR,(char *)"");
													}
	| additive_expression '-' multiplicative_expression  { 
													ast_node *subop = createnode_two(NULL,NULL,SUBOP,(char *)" - ");
													$$ = createnode_three($1,subop,$3,ADD_EXPR,(char *)"");
													}


shift_expression
	: additive_expression						    { $$ = createnode_two($1,NULL,SHIFT_EXPR,(char *)"");}
	| shift_expression LEFT_OP additive_expression	{ 
										ast_node *leftop = createnode_two(NULL,NULL,LEFTOP,(char *)" LEFT_OP ");
										$$ = createnode_three($1,leftop,$3,SHIFT_EXPR,(char *)"");
													}
	| shift_expression RIGHT_OP additive_expression { 
										ast_node *rightop = createnode_two(NULL,NULL,RIGHTOP,(char *)" RIGHT_OP");
										$$ = createnode_three($1,rightop,$3,SHIFT_EXPR,(char *)"");
													}
	;


relational_expression
	: shift_expression								{ $$ = createnode_two($1,NULL,RELTN_EXPR,(char *)"");}
	| relational_expression '<' shift_expression    { 
											ast_node *lessthan = createnode_two(NULL,NULL,LESSTHAN,(char *)" < ");
											$$ = createnode_three($1,lessthan,$3,RELTN_EXPR,(char *)"");
													}
	| relational_expression '>' shift_expression	{ 
											ast_node *greaterthan = createnode_two(NULL,NULL,GREATERTHAN,(char *)" > ");
											$$ = createnode_three($1,greaterthan,$3,RELTN_EXPR,(char *)"");
													}
	| relational_expression LE_OP shift_expression 	{ 
											ast_node *le_op = createnode_two(NULL,NULL,OP_LE,(char *)" LE_OP ");
											$$ = createnode_three($1,le_op,$3,RELTN_EXPR,(char *)"");
													}
	| relational_expression GE_OP shift_expression  { 
											ast_node *ge_op = createnode_two(NULL,NULL,OP_GE,(char *)" GE_OP ");
											$$ = createnode_three($1,ge_op,$3,RELTN_EXPR,(char *)"");
													}
	;


equality_expression
	: relational_expression						{ $$ = createnode_two($1,NULL,EQU_EXPR,(char *)"");}
	| equality_expression EQ_OP relational_expression { 
												ast_node *eq_op = createnode_two(NULL,NULL,OP_EQ,(char *)" EQ_OP");
												$$ = createnode_three($1,eq_op,$3,EQU_EXPR,(char *)"");
												}
	| equality_expression NE_OP relational_expression { 
												ast_node *ne_op = createnode_two(NULL,NULL,OP_NE,(char *)" NE_OP");
												$$ = createnode_three($1,ne_op,$3,EQU_EXPR,(char *)"");
													  }
	;


and_expression
	: equality_expression					{ $$ = createnode_two($1,NULL,AND_EXPR,(char *)"");}
	| and_expression '&' equality_expression		{ 
											ast_node *bit_AND = createnode_two(NULL,NULL,OP_BIT_AND,(char *)" & ");
											$$ = createnode_three($1,bit_AND,$3,AND_EXPR,(char *)"");
													}
	;


exclusive_or_expression
	: and_expression 						{ $$ = createnode_two($1,NULL,EXOR_EXPR,(char *)"");}		  
	| exclusive_or_expression '^' and_expression 	{ 
											ast_node *bit_XOR = createnode_two(NULL,NULL,OP_BIT_EXOR,(char *)"^");
											$$ = createnode_three($1,bit_XOR,$3,EXOR_EXPR,(char *)"");
													}	
	;


inclusive_or_expression
	: exclusive_or_expression 						{ $$ = createnode_two($1,NULL,INCL_EXPR,NULL);}
	| inclusive_or_expression '|' exclusive_or_expression { 
												ast_node *bit_OR = createnode_two(NULL,NULL,OP_BIT_OR,(char *)" | ");
												$$ = createnode_three($1,bit_OR,$3,INCL_EXPR,(char *)"");
													}
	;


logical_and_expression 
	: inclusive_or_expression 		{ $$ = createnode_two($1,NULL,LOG_AND_EXPR,NULL);}
	| logical_and_expression AND_OP inclusive_or_expression
									{ 
									  ast_node *log_AND = createnode_two(NULL,NULL,OP_AND,(char *)" AND_OP ");
									  $$ = createnode_three($1,log_AND,$3,LOG_AND_EXPR,(char *)"");
									}
	;


logical_or_expression
	: logical_and_expression 		{ $$ = createnode_two($1,NULL,LOG_OR_EXP,NULL);}
	| logical_or_expression OR_OP logical_and_expression
									{ 
									  ast_node *log_OR = createnode_two(NULL,NULL,OP_OR,(char *)" OR_OP ");
									  $$ = createnode_three($1,log_OR,$3,LOG_OR_EXP,(char *)"");
									}
	;

conditional_expression
	: logical_or_expression 			{ $$ = createnode_two($1,NULL,COND_EXPR,(char *)"");}
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression 			{ $$ = createnode_two($1,NULL,ASSGN_EXPR,(char *)"");}
	| unary_expression assignment_operator assignment_expression
	;


assignment_operator
	: '=' 							{ $$ = createnode_two(NULL,NULL,ASSGN,(char *)"=");}
	| MUL_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_MUL,(char *)"MUL_ASSIGN");}
	| DIV_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_DIV,(char *)"DIV_ASSIGN");}
	| MOD_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_MOD,(char *)"MOD_ASSIGN");}
	| ADD_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_ADD,(char *)"ADD_ASSIGN");}
	| SUB_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_SUB,(char *)"SUB_ASSIGN");}
	| LEFT_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_LEFT,(char *)"LEFT_ASSIGN");}
	| RIGHT_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_RIGHT,(char *)"RIGHT_ASSIGN");}
	| AND_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_AND,(char *)"AND_ASSIGN");}
	| XOR_ASSIGN					{ $$ = createnode_two(NULL,NULL,ASSGN_XOR,(char *)"XOR_ASSIGN");}
	| OR_ASSIGN						{ $$ = createnode_two(NULL,NULL,ASSGN_OR,(char *)"OR_ASSIGN");}
	;

expression
	: assignment_expression 						{ $$ = createnode_two($1,NULL,EXPR,(char *)"");}
	| expression ',' assignment_expression 			{ 
														ast_node *comma = createnode_two(NULL,NULL,COMMA,(char *)",");
														$$ = createnode_three($1,comma,$3,EXPR,(char *)"");
													}
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
	: storage_class_specifier declaration_specifiers { $$ = createnode_two($1,$2,DECL_SPEC_NT,(char *)"");}
	| storage_class_specifier						 { $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)"");}
	| type_specifier declaration_specifiers			 { $$ = createnode_two($1,$2,DECL_SPEC_NT,(char *)"");}
	| type_specifier                                 { $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)" ");}
	| type_qualifier declaration_specifiers 		 { $$ = createnode_two($1,$2,DECL_SPEC_NT,(char *)"");}
	| type_qualifier                                 { $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)"");}
	| function_specifier declaration_specifiers      { $$ = createnode_two($1,$2,DECL_SPEC_NT,(char *)"");}
	| function_specifier 							 { $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)"");}
	| alignment_specifier declaration_specifiers     { $$ = createnode_two($1,$2,DECL_SPEC_NT,(char *)"");}
	| alignment_specifier 							 { $$ = createnode_two($1,NULL,DECL_SPEC_NT,(char *)"");}
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
	| DOUBLE 								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"DOUBLE ");}
	| SIGNED								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"SIGNED ");}
	| UNSIGNED								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"UNSIGNED ");}
	| BOOL									{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"BOOL ");}
	| COMPLEX								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"COMPLEX ");}
	| IMAGINARY								{ $$ = createnode_two(NULL,NULL,TYPE_SPEC_T,(char *)"IMAGINARY ");}
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
	: pointer direct_declarator 			{ $$ = createnode_two($1,$2,DECL,(char *)"");}
	| direct_declarator 					{ $$ = createnode_two($1,NULL,DECL,(char *)"");}
	;



direct_declarator
	: IDENTIFIER 							{ $$ = createnode_two(NULL,NULL,IDEN,(char *)"ID"); }
	| '(' declarator ')'					{  
							ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
							ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
							$$ = createnode_three(open,$2,close,DIRECT_DECL,(char *)"");
						}
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
	| direct_declarator '(' ')'  				{  
							ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
							ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
							$$ = createnode_three($1,open,close,DIRECT_DECL,(char *)"");
						}
	| direct_declarator '(' identifier_list ')' 	{  
							ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
							ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
							$$ = createnode_four($1,open,$3,close,DIRECT_DECL,(char *)"");
						}
	;




pointer
	: '*' type_qualifier_list pointer       { ast_node *star = createnode_two(NULL,NULL,POINTER,(char *)"*");
											  $$ = createnode_three(star,$2,$3,POINTER_NT,(char *)"");
											}
	| '*' type_qualifier_list				{ ast_node *star = createnode_two(NULL,NULL,POINTER,(char *)"*");
											  $$ = createnode_two(star,$2,POINTER_NT,(char *)"");
											}
	| '*' pointer							{ ast_node *star = createnode_two(NULL,NULL,POINTER,(char *)"*");
											  $$ = createnode_two(star,$2,POINTER_NT,(char *)"");
											}
	| '*'									{ $$ = createnode_two(NULL,NULL,POINTER,(char *)"*");}
	;


type_qualifier_list
	: type_qualifier 							{ $$ = createnode_two($1,NULL,TYPE_QUAL_LIST,(char *)"");}						
	| type_qualifier_list type_qualifier 		{ $$ = createnode_two($1,$2,TYPE_QUAL_LIST,(char *)"");}
	;



parameter_type_list
	: parameter_list ',' ELLIPSIS 				{ ast_node * comma = createnode_two(NULL,NULL,COMMA,(char *)",");
									ast_node * ellipsis = createnode_two(NULL,NULL,ELLIPS,(char *)"ELLIPSIS");
									$$ = createnode_three($1,comma,ellipsis,PARAM_TYPE_LIST,(char *)"");
												}
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
	: IDENTIFIER 								{ $$ = createnode_two(NULL,NULL,IDEN,(char *)"ID");}  		
	| identifier_list ',' IDENTIFIER 			{ 
										ast_node *comma = createnode_two(NULL,NULL,COMMA,(char *)",");
										ast_node *iden = createnode_two(NULL,NULL,IDEN,(char *)"ID");
										$$ = createnode_three($1,comma,iden,IDEN_LIST,(char *)"");
									}
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
	: labeled_statement 		{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
	| compound_statement 		{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
	| expression_statement		{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
	| selection_statement  		{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
	| iteration_statement 		{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
	| jump_statement 			{ $$ = createnode_two($1,NULL,STMT,(char *)"");}
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
	: 
	 IF '(' expression ')' statement  			{ 
									ast_node *if1 = createnode_two(NULL,NULL,IF1,(char *)"IF ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_five(if1,open,$3,close,$5,SELEC_STMT,(char *)"");
							 }
							
	| IF '(' expression ')' statement ELSE statement  {
									ast_node *if1 = createnode_two(NULL,NULL,IF1,(char *)"IF ");
									ast_node *else1 = createnode_two(NULL,NULL,ELSE1,(char *)"ELSE ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_seven(if1,open,$3,close,$5,else1,$7,SELEC_STMT,(char *)"");
							}


	| SWITCH '(' expression ')' statement  		{   
									ast_node *switch1 = createnode_two(NULL,NULL,SWITCH1,(char *)"SWITCH");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_five(switch1,open,$3,close,$5,SELEC_STMT,(char *)"");
							}
	;


iteration_statement 
	: WHILE '(' expression ')' statement 				{ 
									ast_node *while1 = createnode_two(NULL,NULL,WHILE1,(char *)"WHILE ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_five(while1,open,$3,close,$5,ITER_STMT,(char *)"");
								}

	| DO statement WHILE '(' expression ')' ';'    		{ 
									ast_node *do1 = createnode_two(NULL,NULL,DO1,(char *)"DO ");
									ast_node *while1 = createnode_two(NULL,NULL,WHILE1,(char *)"WHILE ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_seven(do1,$2,while1,open,$5,close,semicolon,ITER_STMT,(char *)"");
								}

	| FOR '(' expression_statement expression_statement ')' statement    { 
									ast_node *for1 = createnode_two(NULL,NULL,FOR1,(char *)"FOR ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_six(for1,open,$3,$4,close,$6,ITER_STMT,(char *)"");
								}

	| FOR '(' expression_statement expression_statement expression ')' statement   {
									ast_node *for1 = createnode_two(NULL,NULL,FOR1,(char *)"FOR ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_seven(for1,open,$3,$4,$5,close,$7,ITER_STMT,(char *)"");
								}

	| FOR '(' declaration expression_statement ')' statement {
									ast_node *for1 = createnode_two(NULL,NULL,FOR1,(char *)"FOR ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_six(for1,open,$3,$4,close,$6,ITER_STMT,(char *)"");
								}

	| FOR '(' declaration expression_statement expression ')' statement {
									ast_node *for1 = createnode_two(NULL,NULL,FOR1,(char *)"FOR ");
									ast_node *open = createnode_two(NULL,NULL,OPEN_PARAN,(char *)"(");
									ast_node *close = createnode_two(NULL,NULL,CLOSE_PARAN,(char *)")");
								    $$ = createnode_seven(for1,open,$3,$4,$5,close,$7,ITER_STMT,(char *)"");
								}
	;



jump_statement
	: GOTO IDENTIFIER ';'      {
									ast_node *goto1 = createnode_two(NULL,NULL,GOTO1,(char *)"GOTO");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
									ast_node *iden = createnode_two(NULL,NULL,IDEN,(char *)"IDEN");
								    $$ = createnode_three(goto1,iden,semicolon,JMP_STMT,(char *)"");
								}
	| CONTINUE ';' 				{
									ast_node *continue1 = createnode_two(NULL,NULL,CONT,(char *)"CONTINUE");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(continue1,semicolon,JMP_STMT,(char *)"");
								}
	| BREAK ';'					{
									ast_node *break1 = createnode_two(NULL,NULL,BRK,(char *)"BREAK");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(break1,semicolon,JMP_STMT,(char *)"");
								}
	| RETURN ';'				{
									ast_node *return1 = createnode_two(NULL,NULL,RET,(char *)"RETURN ");
									ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								    $$ = createnode_two(return1,semicolon,JMP_STMT,(char *)"");
								}
	| RETURN expression ';'     { ast_node *return1 = createnode_two(NULL,NULL,RET,(char *)"RETURN ");
								  ast_node *semicolon = createnode_two(NULL,NULL,SEMICOLON,(char *)";");
								  $$ = createnode_three(return1,$2,semicolon,JMP_STMT,(char *)"");
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
	 											printf("\n");
	   											}
	| declaration_specifiers declarator compound_statement {
	 											ast_node *func_def = createnode_three($1,$2,$3,FUNCDEF,(char *)"");
	 											dump_ast(func_def);
	 											printf("\n");
	   											}
	; 


declaration_list
	: declaration 								{ $$ = createnode_two($1,NULL,DECL_LIST,(char *)"");}
	| declaration_list declaration 				{ $$ = createnode_two($1,$2,DECL_LIST,(char *)"");}
	;

%%



