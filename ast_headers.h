
#define MAX_CHILD 7

#define YYSTYPE ast_node *
#define YYSTYPE_IS_DECLARED 1


enum nodetype {IDEN,ICONST,FUNCDEF,NONE,OPEN_PARAN,CLOSE_PARAN,RET,SEMICOLON,ADDOP,SUBOP,COMMA,VOID1,OPEN_SQPARAN,
			   CLOSE_SQPARAN,BRK,CONT,GOTO1,OPEN_CURLPARAN,CLOSE_CURLPARAN,DECL_SPEC_NT,TYPE_SPEC_T,TYPE_SPEC_NT,
			   TYPE_DEFNAME,STRG_CLASS_T,TYPE_QUAL_T,FUNC_SPEC_T,DECL,DIRECT_DECL,PARAM_DECL,PARAM_LIST,
			   PARAM_TYPE_LIST,CMPD_STMT,PRIM_EXP,FCONST,ENUM_CONST,STRING1,FUNCNAME,ARG_EXPR_LIST,UNARY_EXPR,
			   POSTFIX_EXP,MULOP,DIVOP,MODOP,ADD_EXPR,LEFTOP,SHIFT_EXPR,RELTN_EXPR,OP_LE,OP_GE,LESSTHAN,GREATERTHAN,
				ASSGN_EXPR,COND_EXPR,LOG_OR_EXP,OP_OR,OP_AND,LOG_AND_EXPR,INCL_EXPR,OP_BIT_OR,OP_BIT_AND,OP_BIT_EXOR,
			  EXOR_EXPR,AND_EXPR,EQU_EXPR,OP_EQ,MUL_EXPR,RIGHTOP,OP_NE,ASSGN_OR,ASSGN_AND,ASSGN_XOR,
			  ASSGN_LEFT,ASSGN_RIGHT,ASSGN_ADD,ASSGN_SUB,ASSGN_MUL,ASSGN_DIV,ASSGN_MOD,ASSGN,STMT,POINTER_NT,POINTER,
			  TYPE_QUAL_LIST,ELLIPS,EXPR,IDEN_LIST,JMP_STMT,DECL_LIST,ITER_STMT,SELEC_STMT,IF1,ELSE1,SWITCH1,DO1,FOR1,WHILE1,
			  INIT_DECL_LIST,DECLTN,CONST_EXPR,INIT_DECL,EQUAL,EXT_DECL};



typedef struct ast_node {

    char *token;
    enum nodetype type;
    struct ast_node *child[MAX_CHILD];

} ast_node;



typedef struct symtable {

  	char *token_value;
  	enum nodetype token_type;             
  	struct symtable *next;

} symtable;


