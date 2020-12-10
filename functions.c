
void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}


void null_assign (ast_node *ptr, int num){
    
    for(int i = 0; i< num; i++){
        ptr->child[i] = NULL;
    }
    
}

ast_node *createnode_two( ast_node *C1, ast_node *C2, enum nodetype type, char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->type = type;

	return tmp;
}



ast_node *createnode_three( ast_node *C1, ast_node *C2, ast_node *C3, enum nodetype type, char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->child[2] = C3;
	tmp->type = type;

	return tmp;
}


ast_node *createnode_four( ast_node *C1, ast_node *C2, ast_node *C3, ast_node *C4, enum nodetype type, char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->child[2] = C3;
	tmp->child[3] = C4;
	tmp->type = type;

	return tmp;
}


ast_node *createnode_five( ast_node *C1, ast_node *C2, ast_node *C3, ast_node *C4, ast_node *C5,enum nodetype type,char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->child[2] = C3;
	tmp->child[3] = C4;
	tmp->child[4] = C5;
	tmp->type = type;

	return tmp;
}


ast_node *createnode_six( ast_node *C1, ast_node *C2, ast_node *C3, ast_node *C4,
	                      ast_node *C5, ast_node *C6, enum nodetype type,char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->child[2] = C3;
	tmp->child[3] = C4;
	tmp->child[4] = C5;
	tmp->child[5] = C6;
	tmp->type = type;

	return tmp;
}



ast_node *createnode_seven( ast_node *C1, ast_node *C2, ast_node *C3, ast_node *C4, ast_node *C5,
						   ast_node *C6, ast_node *C7, enum nodetype type,char *token){

	ast_node *tmp = (ast_node *)malloc(sizeof(ast_node));
	null_assign(tmp,MAX_CHILD);
	tmp->token = (char *)token;
	tmp->child[0] = C1;
	tmp->child[1] = C2;
	tmp->child[2] = C3;
	tmp->child[3] = C4;
	tmp->child[4] = C5;
	tmp->child[5] = C6;
	tmp->child[6] = C7;
	tmp->type = type;

	return tmp;
}


void dump_ast( ast_node *root ){

   if( root->child[0] != NULL )
   		 dump_ast(root->child[0]);
   if( root->child[1] != NULL )
   		 dump_ast(root->child[1]);
   if( root->child[2] != NULL )
   		 dump_ast(root->child[2]);
   if( root->child[3] != NULL )
   		 dump_ast(root->child[3]);
   if( root->child[4] != NULL )
   		 dump_ast(root->child[4]);

   if( root->token != NULL ){
   		printf("%s", root->token );
   }
   	
}
