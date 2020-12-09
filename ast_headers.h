
#define MAX_CHILD 5

enum nodetype {IDEN,ICONST,FUNCDEF,NONE,OPEN_PARAN,CLOSE_PARAN,RET,SEMICOLON,ADDOP,SUBOP,COMMA,VOID1,OPEN_SQPARAN,
			   CLOSE_SQPARAN,BRK,CONT,GOTO1,OPEN_CURLPARAN,CLOSE_CURLPARAN,DECL_SPEC_NT,TYPE_SPEC_T,TYPE_SPEC_NT,
			   TYPE_DEFNAME,STRG_CLASS_T,TYPE_QUAL_T,FUNC_SPEC_T,DECL,DIRECT_DECL,PARAM_DECL,PARAM_LIST,
			   PARAM_TYPE_LIST,CMPD_STMT};

typedef struct ast_node {

    char *token;
    enum nodetype type;
    struct ast_node *child[MAX_CHILD];

} ast_node;


#define YYSTYPE ast_node *
#define YYSTYPE_IS_DECLARED 1




