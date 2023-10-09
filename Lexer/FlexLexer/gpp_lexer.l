%{
	/*
	UFUKCAN ERDEM
	1901042686
	CSE 341 FALL 2022 HW#2 PART-1
	*/

	#include <stdio.h>
	#include <stdlib.h>
	#include <ctype.h>
	#include <string.h>
	#include <stddef.h>
	
	int oc_cc_counter = 0;
%} 

%%

and|or|not|equal|less|nil|list|append|concat|set|deffun|for|if|exit|load|disp|true|false {
	int cnt = 0;
	char * toPrintUpper = (char*)malloc((cnt+1) * sizeof(char));
	while(yytext[cnt]){
		toPrintUpper[cnt] = (toupper(yytext[cnt]));
		cnt++;
		toPrintUpper = realloc(toPrintUpper, (cnt+1) * sizeof(char)); 
	}
	printf("%s: KW_%s\n", yytext, toPrintUpper);
	free(toPrintUpper);
}

"\"".*"\""		{	printf("\": OP_OC\n");
					int k=0;
					while(yytext[k]) {
						if(yytext[k] != '\"') {
							printf("%c",yytext[k]);
						}
						k++;
					}
					printf(": VALUESTR\n");
					printf("\": OP_CC\n");
				}

"+"				{printf("%s: OP_PLUS\n",yytext);}
"-"				{printf("%s: OP_MINUS\n",yytext);}
"/"				{printf("%s: OP_DIV\n",yytext);}
"*"				{printf("%s: OP_MULT\n",yytext);}
"("				{printf("%s: OP_OP\n",yytext);}
")"				{printf("%s: OP_CP\n",yytext);}
"**"			{printf("%s: OP_DBLMULT\n",yytext);}
"\""		    {	if(oc_cc_counter == 0) {
						printf("%s: OP_OC\n",yytext);
						oc_cc_counter=1;
						} 
					else{
						printf("%s: OP_CC\n",yytext);
						oc_cc_counter=0;
					}  
				}
","				{printf("%s: OP_COMMA\n",yytext);}

"0" 							{printf("%s: VALUEI\n",yytext);}
[-]*[1-9]*[0-9]+ 				{printf("%s: VALUEI\n",yytext);}
[0-9]*"f"[1-9][0-9]* 			{printf("%s: VALUEF\n",yytext);}
[a-zA-Z_][a-zA-Z0-9_]* 			{printf("%s: IDENTIFIER\n",yytext);}
[0-9]+[a-zA-Z][a-zA-Z0-9]*		{printf("%s: LEXICAL ERROR \n",yytext);}
[ \t\n]+ 						// Finds,ignores white spaces
";;".*$ 						{printf("%s: COMMENT\n",yytext);}
"finish".*$ 					{exit(0); }
%% 

int yywrap(){} 

void gppinterpreter(char * path){
	
	FILE *file_pointer;

	if(path != NULL) {
		file_pointer = fopen(path,"r");
		if(!file_pointer){
			printf("FILE DOES NOT EXIST! ERROR!\n");
			exit(-1);
		}
		yyin = file_pointer;
		yylex();
		fclose(file_pointer);
	}

	else{
		printf("!!!THE REPL HAS STARTED, ENTER YOUR CODE LINE BY LINE IN TERMINAL!!! YOU CAN EXIT WITH WRITING 'finish' TO TERMINAL!!!\n");
		yylex();
	}
}
int main(int argc,char *argv[]){
	gppinterpreter(argv[1]);
	return 0; 
}