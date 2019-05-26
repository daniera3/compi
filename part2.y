%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "lex.yy.c"

	typedef enum {false,true}bool;
	typedef struct node
	{
		char *token;
		struct node *left;
		struct node *right;
	} node;

	typedef struct Arguments
	{
		char * name;
		char * type;
		char * len;
	}Arguments;

    typedef struct Function 
	{
        char * name;
		struct Arguments * args;
        char *returnType; 
		int countArgs;
		int findreturn;
    } Function;
	

	typedef struct Varaiables
	{	int isArg;
		char *name;
		char *value;
		char *type;
		char * len;
	}Varaiable;

		typedef struct code
	{	
		int place;
		char *name;
		Varaiable * var;
		int countvar;
		int countfunc;
		Function ** func;
		struct code * nextLVL;
		struct code * beforeLVL;
	}code;
	char * exprtype(node *,code*);
	Arguments * mkArgs(node *,int *);
	void addFunc(char * name,Arguments * args,node *returnType,int countArgs,code*);
	void addvar(Arguments * args,int,int,code * CODEscope);
	code* mkcode(char *);
	node* mknode(char* token, node *left, node *right);
	void Printtree(node *tree);
	void printTabs(int n);
	int yylex();
	int yyerror(char *e);
	void push(code* from,char*);
	int printlevel=0;
	code* mycode=NULL;
	code* lestcode(code * codey);
	static int scope=0;
	void syntaxMKscope(node *tree,code * scope);
	char* findfunc(node * tree,code * CODEscope,int* count);
	char *findvar(node * tree,code * CODEscope);
	Arguments * callfuncargs(code *,node *tree,int * count);
	int flagMain=false;

	//part 3
	int POPParams(Arguments * args,int count);
	static int t=0;
	static int l=0;



	
	
%}
%union
{
    struct node *node;
    char *string;
}



%token <string> COMMENT WHILE IF ELSE FOR 
%token <string> RETURN
%token <string> BOOL STRING CHARPTR CHAR INT INTPTR PROCEDUR
%token <string> AND ADDRESS EQL ASSINGMENT OR LENGTH GREATEREQL GREATER LESSEQL LESS NOTEQL NOT
%token <string> DIVISION PLUS MINUS MULTI VARIABLE
%token <string> STRING_LTL REAL_LTL CHAR_LTL NULLL
%token <string> MAIN IDENTIFIER SEMICOLON COMMA OPENPAREN CLOSEPAREN OPENBRACKET CLOSEBRACKET OPENBRACE CLOSEBRACE
%token <string> DECIMAL_LTL HEX_LTL BOOLTRUE BOOLFALSE  REAL REALPTR FUNCTION COLON  DEREFRENCE 

%left  NOTEQL LESS LESSEQL GREATEREQL GREATER OR AND
%left PLUS MINUS RETURN
%left MULTI DIVISION
%left SEMICOLON EQL
%right NOT CLOSEBRACE

%nonassoc IDENTIFIER 
%nonassoc OPENPAREN
%nonassoc IF
%nonassoc ELSE 


%type <node> address_expr stmnts stmnt_block derefrence_expr expr_list call_func 
%type <node> expr lhs assmnt_stmnt new_block 
%type <node> stmnt type_pro type_id var_id declear paren_expr
%type <node> pro_body para_list para_pro procedure procedures
%type <node> main program project declears 
%%
 //Main project
 
project: cmmnt program { syntaxMKscope($2,mycode); }; 

program: procedures main{$$=mknode("CODE",$1,$2);}

 //comments
cmmnt: COMMENT cmmnt {;}| ;

 //this is the main
main: PROCEDUR MAIN OPENPAREN CLOSEPAREN cmmnt OPENBRACE pro_body CLOSEBRACE
{
$$=mknode("Main",mknode("ARGS",NULL,$7),NULL);
};

//functions
procedures: procedures  procedure {$$=mknode("",$1,$2);}
	| {$$=NULL;};

//function
procedure: FUNCTION IDENTIFIER OPENPAREN para_pro CLOSEPAREN cmmnt RETURN type_pro  OPENBRACE  pro_body CLOSEBRACE
{ 
		$$=mknode("FUNC",mknode($2,mknode(" ",NULL,NULL),mknode("ARGS",$4,mknode("Return",$8,NULL))),mknode("",$10,NULL));

}
| PROCEDUR IDENTIFIER OPENPAREN para_pro CLOSEPAREN  OPENBRACE  pro_body CLOSEBRACE
{
	$$=mknode("PROC",mknode($2,mknode("",NULL,NULL),NULL),mknode("ARGS",$4,$7));
};


//list of parameter for function or not
para_pro: para_list {$$=$1;}
| {$$=NULL;};

//list of parameter

para_list: var_id COLON type_id {$$=mknode("(",$3,mknode("",$1,mknode(")",NULL,NULL)));}
	|  para_list SEMICOLON cmmnt  para_list {$$=mknode("",$1,mknode("",$4,NULL));}	;

 //Procedure body
pro_body: cmmnt  procedures declears stmnts 
{
	$$=mknode("BODY", mknode(" ",$2,NULL),mknode(" ",$3,mknode(" ",$4,mknode(" ",NULL,NULL))));
};


//list of declears
declears: declears declear  {$$=mknode("",$1,$2);} | {$$=NULL;}  ;

//declaration of varibals/ 
declear: VARIABLE var_id COLON type_id cmmnt SEMICOLON cmmnt
{
	$$=mknode("var", $4,$2);
};

//list of id like a,b,c/
var_id: IDENTIFIER COMMA var_id {$$=mknode($1, mknode(" ", $3, NULL),NULL);}
	| IDENTIFIER {$$=mknode($1, NULL, NULL);} ;


//types without string/
type_id: BOOL {$$=mknode("boolean", NULL, NULL);}
	| STRING OPENBRACKET DECIMAL_LTL CLOSEBRACKET {$$=mknode("string", mknode("[",mknode("$3",NULL,NULL),NULL), NULL);}
	| CHAR {$$=mknode("char", NULL, NULL);}
	| INT {$$=mknode("int", NULL, NULL);}
	| REAL {$$=mknode("real", NULL, NULL);}
	| INTPTR {$$=mknode("int*", NULL, NULL);}
	| CHARPTR {$$=mknode("char*", NULL, NULL);}
	| REALPTR {$$=mknode("real*", NULL, NULL);};



//type for returning from a function
type_pro: BOOL {$$=mknode("boolean", NULL, NULL);}
 	| STRING {$$=mknode("string", NULL, NULL);}
	| CHAR {$$=mknode("char", NULL, NULL);}
	| INT {$$=mknode("int", NULL, NULL);}
	| REAL {$$=mknode("real", NULL, NULL);}
	| INTPTR {$$=mknode("int*", NULL, NULL);}
	| CHARPTR {$$=mknode("char*", NULL, NULL);}
	| REALPTR {$$=mknode("real*", NULL, NULL);};
	

//Statments
stmnts: stmnts stmnt {$$=mknode("",$1,$2);} | {$$=NULL;};

//stmnt_block
stmnt_block: stmnt {$$=$1;}|declear {$$=$1;}|procedure {$$=$1;} |SEMICOLON  {$$=mknode("",NULL,NULL);};

//new block in stmnts
new_block: OPENBRACE procedures cmmnt declears stmnts CLOSEBRACE cmmnt
{
	$$=mknode("{",$2,mknode("", $4,mknode("", $5,("}",NULL,NULL))));
};

//Statment
stmnt: IF OPENPAREN expr CLOSEPAREN  stmnt_block 
{
	$$=mknode("if",
	mknode("(", $3, 
	mknode(")",NULL,NULL)),$5);
}%prec IF
| IF OPENPAREN expr CLOSEPAREN   stmnt_block    ELSE  stmnt_block  
{
	$$=mknode("if-else",
	mknode("", $3, 
	mknode("",NULL,NULL)),
	mknode("",$5,
	mknode("",$7,NULL)));
}
| WHILE cmmnt OPENPAREN expr CLOSEPAREN  stmnt_block  
{
	$$=mknode("while",
	mknode("(", $4, 
	mknode(")",NULL,NULL)),$6);
}
| FOR cmmnt OPENPAREN assmnt_stmnt SEMICOLON expr SEMICOLON assmnt_stmnt CLOSEPAREN stmnt_block 
{
		$$= mknode("for",
			mknode("(",
			mknode("",$4,$6),
			mknode("",$8,
			mknode(")",NULL,NULL))),$10);		
}
| assmnt_stmnt SEMICOLON cmmnt {$$=mknode("",$1,NULL);}
| expr SEMICOLON cmmnt {$$=$1;}
| RETURN expr SEMICOLON cmmnt {$$=mknode("return",$2,NULL);}
| new_block {$$=$1;};




//assiment statment
assmnt_stmnt: lhs ASSINGMENT expr 
{
	$$=mknode("=",$1,$3);
};


//lefd hand side id
lhs: IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
{
	$$=mknode($1, mknode("[",$3,mknode("]",NULL,NULL)), NULL);
} 
| IDENTIFIER {$$=mknode($1,NULL,NULL);}
| address_expr {$$=$1;}
| derefrence_expr{$$=$1;} ;


	
//Expresion
expr:  OPENPAREN expr CLOSEPAREN {$$=mknode("(",$2,mknode(")",NULL,NULL));}|
//bool oper
    expr EQL expr {$$=mknode("==",$1,$3);}
	| expr NOTEQL expr {$$=mknode("!=",$1,$3);}
	| expr GREATEREQL expr {$$=mknode(">=",$1,$3);}
	| expr GREATER expr {$$=mknode(">",$1,$3);}
	| expr LESSEQL expr {$$=mknode("<=",$1,$3);}
	| expr LESS expr {$$=mknode("<",$1,$3);}
	| expr AND expr {$$=mknode("&&",$1,$3);}
	| expr OR expr {$$=mknode("||",$1,$3);}
//aritmetical operator
	| expr PLUS expr {$$=mknode("+",$1,$3);}
	| expr MINUS expr {$$=mknode("-",$1,$3);}
	| expr MULTI expr {$$=mknode("*",$1,$3);}
	| expr DIVISION expr {$$=mknode("/",$1,$3);}
//not operator
	| NOT expr {$$=mknode("!",$2,NULL);}
	| address_expr {$$=$1;}
	| derefrence_expr {$$=$1;}
	| call_func cmmnt {$$=$1;}
	| DECIMAL_LTL {$$=mknode($1,mknode("INT",NULL,NULL),NULL);}
	| HEX_LTL {$$=mknode($1,mknode("HEX", NULL, NULL),NULL);}
	| CHAR_LTL {$$=mknode($1,mknode("CHAR", NULL, NULL),NULL);}
	| REAL_LTL {$$=mknode($1,mknode("REAL", NULL, NULL),NULL);}
	| STRING_LTL {$$=mknode($1,mknode("STRING", NULL, NULL),NULL);}
	| BOOLFALSE {$$=mknode($1,mknode("BOOLEAN", NULL, NULL),NULL);}
	| BOOLTRUE {$$=mknode($1,mknode("BOOLEAN", NULL, NULL),NULL);}
	| LENGTH IDENTIFIER LENGTH 
	{
		$$=mknode("|",
		mknode($2,NULL,NULL),
		mknode("|",NULL,NULL));
	}
	| IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mknode("solovar",mknode($1,mknode("[",$3,mknode("]",NULL,NULL)),NULL),NULL);}
	| IDENTIFIER {$$=mknode("solovar",mknode($1,NULL,NULL),NULL);}
	| NULLL {$$=mknode("null",NULL,NULL);};

//address expression like &id

//address_exprs:ADDRESS address_exprs {$$=mknode($1,$2,NULL);} | address_expr {$$=$1;};

address_expr: ADDRESS IDENTIFIER {$$=mknode("&",mknode($2,NULL,NULL),NULL);}
	| ADDRESS OPENPAREN IDENTIFIER CLOSEPAREN {$$=mknode("&",mknode("(",mknode($3,NULL,NULL),NULL),mknode(")",NULL,NULL));}
	| ADDRESS IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mknode("&", mknode($2,mknode("[",$4,mknode("]",NULL,NULL)),NULL),NULL);}
	| ADDRESS OPENPAREN IDENTIFIER OPENBRACKET expr CLOSEBRACKET CLOSEPAREN 
	{
		$$=mknode("&",
		mknode("(", 
		mknode($3,mknode("[",$5,mknode("]",NULL,NULL)),NULL)
		,mknode(")",NULL,NULL)),NULL);
	};
//value expression like ^id
//derefrence_exprs:DEREFRENCE derefrence_exprs {$$=mknode($1,$2,NULL);} | derefrence_expr {$$=$1;};

	derefrence_expr: DEREFRENCE IDENTIFIER {$$=mknode("^",mknode($2,NULL,NULL),NULL);}
	| DEREFRENCE OPENPAREN expr CLOSEPAREN {$$=mknode("^",mknode("(",$3,NULL),mknode(")",NULL,NULL));}
	| DEREFRENCE IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mknode($1, mknode($2,mknode("[",$4,mknode("]",NULL,NULL)),NULL), NULL);};

	//list of expreession
expr_list: expr COMMA expr_list {$$=mknode("",$1,mknode(",",$3,NULL));} 
	| expr {$$=mknode("",$1,NULL);}
	| {$$=NULL;};

paren_expr:OPENPAREN expr_list CLOSEPAREN {$$=$2;};
//call func rul 
call_func: IDENTIFIER paren_expr {$$=mknode("Call func",mknode($1,NULL,NULL),mknode("ARGS",$2,NULL));} ;
%%



int main()
{
	int x= yyparse();
	if(x==0)
	{
	printf("syntax valid\n"); 
	printf("Semantics valid\n");
	}
	return x;
	

	
	
}
Arguments * callfuncargs(code * CODEscope,node *tree,int * count)
{
	Arguments  *arr=NULL,ar[50];
	char* type,*len;
	while(tree!=NULL)
	{
		ar[(*count)++].type=exprtype(tree->left,CODEscope);
		//printf("%d %s, ",*count,tree->left->token);
		if(tree->right!=NULL)
			tree=tree->right->left;
		else
			tree=NULL;

	}
	arr=(Arguments*)malloc(sizeof(Arguments)*(*count));
	for(int i=0;i<*count;i++)
		arr[i].type=ar[i].type;
	return arr;
}
char* findfunc(node * tree,code * CODEscope,int * countParams)
{
	code*temp=CODEscope;
	Arguments* args;
	int find=false,flag=true;
	while(temp!=NULL)
	{
		for(int i=0;i<temp->countfunc;i++)
		if(strcmp(tree->left->token,temp->func[i]->name)==0)
		{
			find=true;
			flag=true;
			int count=0;
			args=callfuncargs(CODEscope,tree->right->left,&count);
			//printf("%d %d ",count,temp->func[i]->countArgs);
			if(count==temp->func[i]->countArgs)
			{
				for(int j=0,t=count-1;j<count;j++,t--)
				{
					//printf("%s %s %s",args[j].type,temp->func[i]->args[t].type, temp->func[i]->args[t].name);
					if(strcmp(args[j].type,temp->func[i]->args[t].type)!=0)
						flag=false;
				}
				if(flag==true){
					if(countParams!= NULL)
						*countParams = POPParams(args,count);
					return temp->func[i]->returnType;
				}
			}
		}
		temp=temp->beforeLVL;
	}
	printf("ERORR,func %s not find call in scope %s in func/proc %s\n",tree->left->token,CODEscope->name,mycode->func[mycode->countfunc-1]->name);
	if(find==true)
		printf("but find func with some name but other args\n");
		exit(1);
}
char *findvar(node * tree,code * CODEscope)
{
	code*temp=CODEscope;
	if(strcmp(tree->token,"solovar")==0)
		tree=tree->left;
	while(temp!=NULL)
	{
		for(int i=0;i<temp->countvar;i++)
		if(strcmp(tree->token,temp->var[i].name)==0)
		{
			
			if(tree->left!=NULL && strcmp(tree->left->token,"[")==0)
			{
				if(strcmp(temp->var[i].type,"string")==0)
					if(strcmp(exprtype(tree->left->left,CODEscope),"int")==0)
					{
						return "char";
					}
					else
					{
						printf("ERORR, index in string can be only int (<string>[<int>])in scope %s in func/proc %s\n",CODEscope->name,mycode->func[mycode->countfunc-1]->name);
						exit(1);
					}
				else
				{
					printf("ERORR,you can use index only on string type (<string>[<int>]) in scope %s in func/proc %s\n",CODEscope->name,mycode->func[mycode->countfunc-1]->name);
					exit(1);
				}

			}
			else
			return temp->var[i].type;

		}
		temp=temp->beforeLVL;
	}
	printf("ERORR,var %s not find in scope %s in func/proc %s\n ",tree->token,CODEscope->name,mycode->func[mycode->countfunc-1]->name);
	exit(1);
	
}
char * exprtype(node * tree,code* CODEscope){
	char* msg=(char*)malloc(sizeof(char)*7);
	msg="";
	if(strcmp(tree->token,"null")==0)
		msg="NULL";
	else
	if(tree->left!=NULL){
		if(strcmp(tree->left->token,"INT")==0)
			msg= "int";
		if(strcmp(tree->left->token,"HEX")==0)
			msg= "hex";
		if(strcmp(tree->left->token,"CHAR")==0)
			msg= "char";
		if(strcmp(tree->left->token,"REAL")==0)
			msg= "real";
		if(strcmp(tree->left->token,"STRING")==0)
			msg= "string";
		if(strcmp(tree->left->token,"BOOLEAN")==0)
			msg= "boolean";
		if(strcmp(tree->token,"!")==0)
		if(strcmp(exprtype(tree->left,CODEscope),"boolean")==0)
			msg="boolean";
		else{
			printf("Erorr op ! you can use only on boolean type");
			exit(1);
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(exprtype(tree->left,CODEscope),"string")==0)
		msg="int";
		else{
			printf("Erorr op | you can use only on string type in func/proc %s",mycode->func[mycode->countfunc-1]->name);
			exit(1);
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope))==0&&strcmp(exprtype(tree->right,CODEscope),"string")!=0)
			msg="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope),mycode->func[mycode->countfunc-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(exprtype(tree->left,CODEscope),"int")==0||strcmp(exprtype(tree->left,CODEscope),"real")==0)&&(strcmp(exprtype(tree->right,CODEscope),"int")==0||strcmp(exprtype(tree->right,CODEscope),"real")==0))
			msg="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope),mycode->func[mycode->countfunc-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope))==0&&strcmp(exprtype(tree->right,CODEscope),"boolean")==0)
			msg="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope),mycode->func[mycode->countfunc-1]->name);
				exit(1);
			}
			

		}
		if(strcmp(tree->token,"-")==0||strcmp(tree->token,"+")==0)
		{
			if((strcmp(exprtype(tree->left,CODEscope),"int")==0||strcmp(exprtype(tree->left,CODEscope),"real")==0)&&(strcmp(exprtype(tree->right,CODEscope),"int")==0||strcmp(exprtype(tree->right,CODEscope),"real")==0))
			{
			if(strcmp(exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope))==0&&strcmp(exprtype(tree->left,CODEscope),"int")==0)
			msg="int";
			else
			msg="real";
			}

			if(strcmp(exprtype(tree->right,CODEscope),"int")==0&&(strcmp(exprtype(tree->left,CODEscope),"char*")==0||strcmp(exprtype(tree->right,CODEscope),"int*")==0||strcmp(exprtype(tree->right,CODEscope),"real*")==0)){
				msg=exprtype(tree->left,CODEscope);
			}
			else if(strcmp(msg,"")==0)
			{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope),mycode->func[mycode->countfunc-1]->name);
				exit(1);
			}

		}
		if(strcmp(tree->token,"*")==0||strcmp(tree->token,"/")==0)
		{
			if((strcmp(exprtype(tree->left,CODEscope),"int")==0||strcmp(exprtype(tree->left,CODEscope),"real")==0)&&(strcmp(exprtype(tree->right,CODEscope),"int")==0||strcmp(exprtype(tree->right,CODEscope),"real")==0))
			{
			if(strcmp(exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope))==0&&strcmp(exprtype(tree->left,CODEscope),"int")==0)
			msg="int";
			else
			msg="real";
			}
			else
			{
				printf("ERORR, you cant do %s between %s and %s\n",tree->token,exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope));
				exit(1);
			}
		}
		if(strcmp(tree->token,"&")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=exprtype(tree->left->left,CODEscope);
			else{
				msg=exprtype(tree->left,CODEscope);
				
				}
			if(strcmp(msg,"char")==0)
			msg="char*";
			else
			if(strcmp(msg,"int")==0)
			msg="int*";
			else
			if(strcmp(msg,"real")==0)
			msg="real*";
			else
			{
				printf("ERORR, you cant do %s on %s \n",tree->token,msg);
				exit(1);
			}
		}
		if(strcmp(tree->token,"^")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=exprtype(tree->left->left,CODEscope);
			else
				msg=exprtype(tree->left,CODEscope);
			
			if(strcmp(msg,"char*")==0)
			msg="char";
			else
			if(strcmp(msg,"int*")==0)
			msg="int";
			else
			if(strcmp(msg,"real*")==0)
			msg="real";
			else
			{
				printf("ERORR, you cant do %s on %s \n",tree->token,msg);
				exit(1);
			}

		}
		if(strcmp(tree->token,"(")==0)
			msg=exprtype(tree->left,CODEscope);
		if(strcmp(tree->token,"Call func")==0)
			msg=findfunc(tree,CODEscope,NULL);
		
	}
	if(strcmp(msg,"")==0)
		msg=findvar(tree,CODEscope);

	
	

	return msg;
}
void push(code* from,char* name)
{
	code * point;
	if(mycode==NULL)
		mycode=mkcode(name);
	else{
	point=mycode;
	while(point->nextLVL!=NULL)
		point=point->nextLVL;
	point->nextLVL=mkcode(name);
	point->nextLVL->beforeLVL=from;
	}
}
code* mkcode(char* name)
{	
	code *newlvl = (code*)malloc(sizeof(code));
	newlvl->place=++scope;
	newlvl->name=name;
	newlvl->var=NULL;
	newlvl->countvar=0;
	newlvl->func=NULL;
	newlvl->countfunc=0;
	newlvl->nextLVL=NULL;
	newlvl->beforeLVL=NULL;
	return newlvl;
}


void addvar(Arguments * args,int countvars,int isArg,code * CODEscope){
	if(countvars==0)
	return;
	Varaiable* temp;
	code * codey=CODEscope;

	for(int i=0;i<countvars;i++)
		for(int j=0;j<countvars;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("sorry you can't some vars %s in one declear",args[i].name);
		code * t=codey->beforeLVL;
		while(t->beforeLVL!=NULL && t->beforeLVL->countfunc==0)
			t=t->beforeLVL;
		if(t->func!=NULL)
		printf(",in func %s\n",t->func[t->countfunc-1]->name);
			else
		printf("\n");
		exit(1);
	}
	if(codey->var==NULL)
	{ 
		codey->var=(Varaiable*) malloc(sizeof(Varaiable)*countvars);
	}
	else
	{
		temp=codey->var;
		codey->var=(Varaiable*) malloc(sizeof(Varaiable)*(codey->countvar+countvars));
		for(int i=0;i<codey->countvar;i++)
		{
			for(int j=0;j<countvars;j++)
			{
				if(strcmp(temp[i].name,args[j].name)==0 )
				{
					printf("sorry you can't some var %s in some scope",temp[i].name);
					code * t=codey->beforeLVL;
					while(t->beforeLVL!=NULL && t->beforeLVL->countfunc==0)
						t=t->beforeLVL;
					if(t->func!=NULL)
					printf(",in func %s\n",t->func[t->countfunc-1]->name);
					else
					printf("\n");
					exit(1);
				}
			}
			codey->var[i]=temp[i];	
		}
	}
	for(int j=0;j<countvars;j++)
	{

		codey->var[codey->countvar].name=args[j].name;
		codey->var[codey->countvar].value=NULL;
		codey->var[codey->countvar].isArg=isArg;
		codey->var[codey->countvar].len=args[j].len;
		codey->var[(codey->countvar)++].type=args[j].type;
	}
	/*printf("vars in scope %s \n",codey->name);
	for(int i=0;i<codey->countvar;i++)
	{
		printf("%s %s,", codey->var[i].name,codey->var[i].type);
	}printf("\nend vars scope %d\n",codey->place);*/
}

void addFunc(char * name,Arguments * args,node *returnType,int countArgs,code * CODEscope){
	Function** temp;
	code * codey=CODEscope;
	for(int i=0;i<countArgs;i++)
		for(int j=0;j<countArgs;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("sorry you can't some Arguments %s in func %s\n",args[i].name,name);
		exit(1);
	}
	if(codey->func==NULL)
	{ 
		codey->func=(Function**) malloc(sizeof(Function*));
	}
	else
	{
		temp=codey->func;
		codey->func=(Function**) malloc(sizeof(Function*)*(codey->countfunc+1));
		for(int i=0;i<codey->countfunc;i++)
		{
				if(strcmp(temp[i]->name,name)==0 )
				{
					printf("sorry you can't some func %s in some scope \n",temp[i]->name);
					exit(1);
				}
				codey->func[i]=temp[i];
		}
	}
		codey->func[codey->countfunc]=(Function*) malloc(sizeof(Function));
		codey->func[codey->countfunc]->name=name;
		codey->func[codey->countfunc]->args=args;
		if(returnType==NULL)
		codey->func[codey->countfunc]->returnType=NULL;
		else{
		if(strcmp(returnType->token,"string")==0)
			{
				printf("ERORR,return type func %s cant be string\n",name);
				exit(1);
			}
		codey->func[codey->countfunc]->returnType=returnType->token;
		}
		codey->func[codey->countfunc]->countArgs=countArgs;
		codey->func[codey->countfunc]->findreturn=false;
		++(codey->countfunc); 

		
	
	/*printf("start %s in scope %d\n",name,codey->place);
	for(int i=0;i<countArgs;i++)
	{
		printf("%s %s,", codey->func[codey->countfunc-1]->args[i].name,codey->func[codey->countfunc-1]->args[i].type);
	}printf("end %s\n",name);*/
}

Arguments * mkArgs(node *tree,int *count){
	Arguments  *arr=NULL,ar[50];
	char* type,*len;
	if(tree!=NULL)
	{
		node * temp1=tree,*temp=tree;
		do{
		if(strcmp(temp1->token, "")==0)
		{
			temp=temp1->right->left;
			temp1=temp1->left;
			
			
			if(strcmp(temp->token, "(")==0||strcmp(temp->token, "var")==0)
		{
			type=temp->left->token;
			if(temp->left->left!=NULL)
			len=temp->left->left->left->token;
			node * treee;
			treee=temp->right->left;
			do{
			ar[*count].name=treee->token;
			ar[*count].type=type;
			ar[*count].len=len;
			(*count)++;
			if(treee->left==NULL)
				treee=NULL;
			else
				treee=treee->left->left;
			}while(treee!=NULL);
		}
		}
		}while(strcmp(temp1->token, "(")!=0&&strcmp(temp->token, "var")!=0);
		temp=temp1;
		if(strcmp(temp->token, "(")==0||strcmp(temp->token, "var")==0)
		{
			type=temp->left->token;
			node * treee;
			if(strcmp(temp->token, "var")==0)
			treee=temp->right;
			else
			treee=temp->right->left;
			if(temp->left->left!=NULL)
			len=temp->left->left->left->token;
			do{
			ar[*count].name=treee->token;
			ar[*count].type=type;
			ar[*count].len=len;
			(*count)++;
			if(treee->left==NULL)
				treee=NULL;
			else
				treee=treee->left->left;
			}while(treee!=NULL);
		}
		arr=(Arguments*)malloc(sizeof(Arguments)*(*count));
		for(int i=0;i<*count;i++)
		{
			for(int j=0;j<*count;j++){
			}
			arr[i].name=ar[i].name;
			arr[i].type=ar[i].type;
			//printf("%s %s,", arr[i].name,arr[i].type);
		}
		//printf("\n");
	}
	return arr;
}

/* allocation for node*/
node* mknode (char *token, node *left, node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;
	return newnode;
}

void printTabs(int n)
{
	int i;
	for(i=0;i<n/9;i++)
		printf(" ");
}
void Printtree(node* tree)
{
	int flag = 4;
	printTabs(printlevel); 
	if(strcmp(tree->token, "var") == 0)
	{
		printf("(DECLARE ");
		flag=2;	
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		printf("(IF\n");
		flag = 1;
		
		
	}
		else if(strcmp(tree->token, "while") == 0)
	{
		printf("(WHILE\n");
		flag = 1;
		
		
	}
			else if(strcmp(tree->token, "for") == 0)
	{
		printf("(FOR\n");
		flag = 1;
		
		
	}
         else if(strcmp(tree->token, "FUNC") == 0 ){
            printf("(%s \n\t",tree->token);
		flag= 2;
          }
         else if(strcmp(tree->token, "PROC") == 0){
                printf("\t(%s \n\t",tree->token);
				flag= 2;
				
           }

		else if(strcmp(tree->token, "Call func") == 0)
	{
		printf("(%s \n",tree->token);
		flag= 1;
		
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{
		printf("(%s \n",tree->token);
		flag= 2;
	}
          else if(strcmp(tree->token, "BODY") == 0){
                
               
		if(tree->left)
		{
			printf("(%s \n",tree->token);
		}
		else{
			flag =1;
		}
               
           }
               
           
		else if(strcmp(tree->token, "ARGS") == 0)
	{
		printf("(ARGS ");
		if(tree->left)
		{
			flag = 2;
			printf("\n\t");
			
		}
		else{
			printf(" NONE)\n"); 
		}
	

	}
		else if(strcmp(tree->token, "if-else") == 0)
	{
		printf("\n(IF-ELSE ");
		flag = 2;
	}
			else if(strcmp(tree->token, "return") == 0)
	{
		printf("(RET ");
		flag = 2;
               
	}
	else if(strcmp(tree->token, "{") == 0)
	{
                printf("(BLOCK");
			printf("\n");
				
				
	}
	else if(strcmp(tree->token, "}") == 0)
	{
                       flag =2;
                      
        }
	else if(strcmp(tree->token, "") == 0||strcmp(tree->token, "BOOLEAN") == 0||strcmp(tree->token, "STRING") == 0||strcmp(tree->token, "REAL") == 0||strcmp(tree->token, "CHAR") == 0||strcmp(tree->token, "INT") == 0||strcmp(tree->token, "HEX") == 0);
	else if(strcmp(tree->token, "(") == 0)
			printf("(");
		else if(strcmp(tree->token, "solovar") == 0 );
	else if(strcmp(tree->token, "\n") == 0)
			printf("\n");
	else if(strcmp(tree->token, ")") == 0)
			printf(")\n");
	else if(strcmp(tree->token, ",") == 0)
			printf(",");
	else if(strcmp(tree->token, ";") == 0)
			printf("\n");
	else if(strcmp(tree->token, "&&") == 0 ||
strcmp(tree->token, "/") == 0 || 
strcmp(tree->token, "=") == 0 || 
strcmp(tree->token, "==") == 0 || 
strcmp(tree->token, ">") == 0 || 
strcmp(tree->token, ">=") == 0 || 
strcmp(tree->token, "<") == 0 || 
strcmp(tree->token, "<=") == 0 || 
strcmp(tree->token, "-") == 0 || 
strcmp(tree->token, "!") == 0 || 
strcmp(tree->token, "!=") == 0 || 
strcmp(tree->token, "||") == 0 || 
strcmp(tree->token, "+") == 0 || 
strcmp(tree->token, "*") == 0 || 
strcmp(tree->token, "&") == 0 || 
strcmp(tree->token, "^") == 0 || 
strcmp(tree->token, "|") == 0 || 
strcmp(tree->token, ",") == 0 )
	{
			printf("(%s",tree->token);
			flag=2;
			if(strcmp(tree->token, "=") == 0)
				flag=1;
				
	}
	else
	{
		if(tree && (!tree->left && !tree->right)
		||strcmp(tree->token, "Main") == 0)
		{
			printf("%s ", tree->token);
			
		}
		else
		{
                   printf("%s", tree->token);
			
		
		}
	}
	if (tree->left) 
	{
		printlevel++;
		Printtree(tree->left);
		printlevel--;
	}
	
	if (tree->right)
	{
		printlevel++;
		Printtree(tree->right);
		printlevel--;
		
	}
	if(flag == 2)
		printf(")\n");
	
	if(flag == 1)
		printf(")");
	if(flag == 0)
		printf("\n)");
}
int yyerror(char *e)
{
	int yydebug=1; 
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "does not accept '%s'\n",yytext);
	
	return 0;
}
code* lestcode(code * codey)
{
	code * CODEscope=codey;
	if(CODEscope!=NULL)
	while(CODEscope->nextLVL!=NULL)
		CODEscope=CODEscope->nextLVL;
	return CODEscope;
}


void syntaxMKscope(node *tree,code * CODEscope){

	if(strcmp(tree->token, "=") == 0 )
	{
		if(!(strcmp(exprtype(tree->right,CODEscope),"NULL")==0&& (strcmp(exprtype(tree->left,CODEscope),"real*")==0||strcmp(exprtype(tree->left,CODEscope),"int*")==0||strcmp(exprtype(tree->left,CODEscope),"char*")==0)))
		if(strcmp(exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope))!=0)
		{
			printf("ERORR, you can't do = between %s and %s in scope %s in func/proc %s\n",exprtype(tree->left,CODEscope),exprtype(tree->right,CODEscope),CODEscope->name,mycode->func[mycode->countfunc-1]->name);
			exit(1);
		}
	}
	else if(strcmp(tree->token, "var") == 0)
	{
		int countvar=0;
		Arguments * var=mkArgs(tree,&countvar);
		addvar(var,countvar,0,CODEscope);
		
		
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		if(strcmp(exprtype(tree->left->left,CODEscope),"boolean")!=0)
		{
			printf("ERORR, in if expr most be type boolean\n");
			exit(1);
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			push(CODEscope,tree->token);
			if (tree->left) 
				syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
			if (tree->right)
				syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
        	scope--;
			return;
		}
		
		
		
	}
		else if(strcmp(tree->token, "while") == 0)
	{
		if(strcmp(exprtype(tree->left->left,CODEscope),"boolean")!=0)
		{
			printf("ERORR, in while expr most be type boolean\n");
			exit(1);
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			push(CODEscope,tree->token);
			if (tree->left) 
				syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
			if (tree->right)
				syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
        	scope--;
			return;
		}
		
		
		
	}
			else if(strcmp(tree->token, "for") == 0)
	{

	 if(strcmp(exprtype(tree->left->left->right,CODEscope),"boolean")!=0)
		{
			printf("ERORR, in for expr most be type boolean\n");
			exit(1);
		}

		syntaxMKscope(tree->left->left->left,CODEscope);

		syntaxMKscope(tree->left->right->left,CODEscope);

		if(strcmp(tree->right->token,"{")!=0)
		{

			push(CODEscope,tree->token);

			if (tree->left) 
				syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
			if (tree->right)
				syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
        	scope--;
			return;
		}

		
		
	}
	else if(strcmp(tree->token, "FUNC") == 0 )
	{
        int count=0;
		Arguments * arg=mkArgs(tree->left->right->left,&count);
		addFunc(tree->left->token,arg,tree->left->right->right->left,count,CODEscope);
		printf("%s:\n",tree->left->token);
		printf("\tBeginFunc‬‬\n");
		push(CODEscope,tree->token);
		addvar(arg,count,1,lestcode(CODEscope));
	if (tree->left) 
		syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
	if (tree->right)
		syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
		if(CODEscope->func[CODEscope->countfunc-1]->findreturn==false)
		{
			printf("ERORR,in func %s not find return\n",tree->left->token);
			exit(1);
		}
        scope--;		
		return;
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{
		
        int count=0;
		Arguments * arg=mkArgs(tree->right->left,&count);
		addFunc(tree->left->token,arg,NULL,count,CODEscope);

		printf("%s:\n",tree->left->token);
		printf("\tBeginFunc‬‬\n");

		push(CODEscope,tree->token);
		addvar(arg,count,1,lestcode(CODEscope));
	if (tree->left) 
		syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
	if (tree->right)
		syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
		scope--;	
		return;
    }

	else if(strcmp(tree->token, "Call func") == 0)
	{
		int count=0;
		findfunc(tree,CODEscope,&count);
		printf("\tt%d = CALL _%s\n",t++,tree->left->token);
		printf("\t‫‪PopParams‬‬‬‬ %d\n",count);

		
		
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{
		//Printtree(tree);
		push(NULL,tree->token);
	if (tree->left) 
		syntaxMKscope(tree->left,mycode);
	
	if (tree->right)
		syntaxMKscope(tree->right,mycode);
		scope--;
		return;
	}
    else if(strcmp(tree->token, "BODY") == 0)
	{     
    }
	else if(strcmp(tree->token, "ARGS") == 0)
	{     
    }
    else if(strcmp(tree->token, "Main") == 0)
	{
		if(flagMain==true && strcmp(CODEscope->name,"CODE")==0)
		{
			printf("Main needs to be one anad only and not inside a func/proc\n");
			exit(1);
		}
		flagMain=true;
		addFunc(tree->token,NULL,NULL,0,CODEscope);
		push(CODEscope,tree->token);

	if (tree->left) 
		syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
	if (tree->right)
		syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
        scope--;
		return;
               
    }       
	else if(strcmp(tree->token, "if-else") == 0)
	{
		if(strcmp(exprtype(tree->left->left,CODEscope),"boolean")!=0)
		{
			printf("ERORR, in if expr most be type boolean\n");
			exit(1);
		}

		if(strcmp(tree->right->left->token,"{")!=0)
		{
			push(CODEscope,tree->token);
			syntaxMKscope(tree->right->left,lestcode( CODEscope->nextLVL));
			scope--;
			push(CODEscope,tree->token);
			syntaxMKscope(tree->right->right->left,lestcode( CODEscope->nextLVL));
        	scope--;
			return;
		}
		
		
		
	}
	else if(strcmp(tree->token, "return") == 0)
	{
		code * temp= CODEscope;
		int flag=true;
		while(strcmp(temp->name,"FUNC")!=0&&strcmp(temp->name,"PROC")!=0&&strcmp(temp->name,"CODE")!=0)
		{
			temp=temp->beforeLVL;
			flag=false;
		}
		if(flag==false)
		{
			if(strcmp(exprtype(tree->left,CODEscope),temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->returnType))
			{
			printf("ERORR,return no some type in func %s \n",temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->name);
			printf("%s ,%s %s\n",exprtype(tree->left,CODEscope),temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->returnType,temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->name);
			exit(1);
			}
		}
		else{
		if(temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->returnType!=NULL){
		if(0==strcmp(exprtype(tree->left,CODEscope),temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->returnType)){
			temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->findreturn=true;
		}
		else{
			printf("ERORR,return no some type in func %s \n",temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->name);
			printf("%s ,%s %s\n",exprtype(tree->left,CODEscope),temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->returnType,temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->name);
			exit(1);
		}
		}
		else
		{
			printf("ERORR,return cant be in proc %s\n",temp->beforeLVL->func[temp->beforeLVL->countfunc-1]->name);
			exit(1);
		}  
		}  

	}
	else if(strcmp(tree->token, "{") == 0)
	{
    push(CODEscope,tree->token);
	if (tree->left) 
		syntaxMKscope(tree->left,lestcode( CODEscope->nextLVL));
	
	if (tree->right)
		syntaxMKscope(tree->right,lestcode( CODEscope->nextLVL));
        scope--;
		return;
			
				
				
	}
	else if(strcmp(tree->token, "}") == 0)
	{
                      
                      
    }
	else if(strcmp(tree->token, "") == 0);
	else if(strcmp(tree->token, "(") == 0)
			;//printf("(");
	else if(strcmp(tree->token, ")") == 0)
			;//printf(")\n");
	else if(strcmp(tree->token, ",") == 0)
			;//printf(",");
	else if(strcmp(tree->token, ";") == 0)
			;//printf("\n");
	else if(strcmp(tree->token, "&&") == 0 ||
strcmp(tree->token, "/") == 0 || 
strcmp(tree->token, "==") == 0 || 
strcmp(tree->token, ">") == 0 || 
strcmp(tree->token, ">=") == 0 || 
strcmp(tree->token, "<") == 0 || 
strcmp(tree->token, "<=") == 0 || 
strcmp(tree->token, "-") == 0 || 
strcmp(tree->token, "!") == 0 || 
strcmp(tree->token, "!=") == 0 || 
strcmp(tree->token, "||") == 0 || 
strcmp(tree->token, "+") == 0 || 
strcmp(tree->token, "*") == 0 || 
strcmp(tree->token, "&") == 0 || 
strcmp(tree->token, "^") == 0 || 
strcmp(tree->token, "|") == 0 || 
strcmp(tree->token, ",") == 0 )
	{
			//printf("(%s",tree->token);
				
				
	}
	else if(strcmp(tree->token, "solovar") == 0 )
	{
		findvar(tree->left,CODEscope);
	}
	if (tree->left) 
		syntaxMKscope(tree->left,CODEscope);
	
	if (tree->right)
		syntaxMKscope(tree->right,CODEscope);

}

int POPParams(Arguments * args,int count){
	int size=0;
	for(int i =0;i<count;i++)
	{
		if(strcmp(args[i].type,"int")==0)
			size += 4;
		else if(strcmp(args[i].type,"char")==0)
			size += 1;
		else if(strcmp(args[i].type,"real")==0)
			size += 8;
		else if(strcmp(args[i].type,"string")==0)
			size += atoi(args[i].len);
		else if(strcmp(args[i].type,"boolean")==0)
			size += 4;
		else if(strcmp(args[i].type,"int*")==0)
			size += 4;
		else if(strcmp(args[i].type,"char*")==0)
			size += 4;
		else if(strcmp(args[i].type,"real*")==0)
			size += 4;
	}
	return size;
}