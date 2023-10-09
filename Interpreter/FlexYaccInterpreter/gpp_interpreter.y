%{
/*
Ufukcan Erdem
1901042686
CSE 341 HW#3 PART-1
*/

#include<stdio.h> 
#include<math.h>

extern int yylex();
extern int yyparse();

int flag=0;
int endf=0;
int listf=0;
int binaryf=0;

int listValues[999];
int listIndex=0;
int tempIndex=0;

%}

%start START
%token COMMENT
%token AND
%token OR
%token NOT
%token EQUAL
%token LESS
%token NIL
%token LIST
%token APPEND
%token CONCAT
%token SET
%token DEFFUN
%token DEFVAR
%token FOR
%token IF 
%token EXIT 
%token LOAD 
%token FLE
%token DISP
%token TRUE 
%token FALSE
%token PLUS 
%token MINUS
%token DIV
%token MULT
%token OP
%token CP
%token DBLMULT
%token OC
%token CC
%token COMMA
%token NUMBER
%token IDENTIFIER
%token LISTOP
%token WHILE

/* Rules */
%% 

START:  | INPUT{
        if(!flag && listf){
            printf("Syntax OK.\n");
            printf("Result:"); 
            
            
            listIndex=0;
            
            tempIndex=0;
            listf=0;
        }
        else if(!flag && binaryf){
            printf("Syntax OK.\n");
            if($$==1)
                printf("Result:T",$$);
            else
                printf("Result:NIL",$$);
            binaryf=0;
        }
        else if(!flag){
            printf("Syntax OK.\n");
            printf("Result:%d",$$); 
        }
        return 0;
        };

INPUT: EXPI | EXPLISTI | EXPB{ binaryf=1; }
;

EXPI: OP DEFVAR IDENTIFIER EXPI CP { $$=$4; } 
| OP SET IDENTIFIER EXPI CP { $$=$4; } 
| OP PLUS EXPI EXPI CP { $$=$3+$4; } 
| OP MINUS EXPI EXPI CP { $$=$3-$4; } 
| OP MULT EXPI EXPI CP { $$=$3*$4; } 
| OP DBLMULT EXPI EXPI CP { $$=pow($3,$4); } 
| OP DIV EXPI EXPI CP { $$=$3/$4; } 
| OP IDENTIFIER EXPLISTI CP { 
    $$=$3; 
    listf=1;
}
| OP DEFFUN IDENTIFIER IDLIST EXPLISTI CP{
    $$=$5;
    listf=1;
}
| OP IF EXPB EXPLISTI CP { 
    $$=$3; 
    if(!$3){
        listIndex=0;
        listValues[0]=NULL;
    }
    listf=1;
}
| OP IF EXPB EXPLISTI EXPLISTI CP{ 
    $$=$3;
    if($3){
        listIndex=tempIndex;
    }
    else{
        listIndex=listIndex-tempIndex;
        for(int i=0;i<listIndex;++i){
            listValues[i]=listValues[tempIndex+i];
        }
    }
    listf=1;
}
| OP WHILE EXPB EXPLISTI CP{ 
    $$=$3; 
    if(!$3){
        listIndex=0;
        listValues[0]=NULL;
    }
    listf=1;
}
| OP FOR OP IDENTIFIER EXPI EXPI CP EXPLISTI CP{
    listf=1;
}
| OP LIST VALUES CP{
    $$=1; 
    listf=1;
}
| OP EXIT CP {
    endf=1;
    printf(" GoodBye \n");
    return 0;
}
| OP LOAD OC FLE OC CP{ $$=1; }
| OP DISP EXPI CP{ $$=1; }
| IDENTIFIER { $$=1; }
| NUMBER { $$=$1; }
| COMMENT { 
    printf("\n COMMENT"); 
    return 0;
};


EXPB:  OP AND EXPB EXPB CP { $$=$3&&$4; } 
| OP OR EXPB EXPB CP { $$=$3||$4; } 
| OP NOT EXPB CP { $$=!$3; } 
| OP EQUAL EXPB EXPB CP { $$=($3==$4); } 
| OP EQUAL EXPI EXPI CP { $$=($3==$4); } 
| OP LESS EXPI EXPI CP { $$=($3<$4); } 
| BinaryValue{$$=$1;};

EXPLISTI: OP CONCAT EXPLISTI EXPLISTI CP{
    $$=1; 
    listf=1;
}

| LISTVALUE{$$=1;}
;

IDLIST: OP ILIST CP;

ILIST: ILIST IDENTIFIER | IDENTIFIER;

LISTVALUE: LISTOP VALUES CP{
    listf=1;
    if(tempIndex==0)
        tempIndex=listIndex;
    
}
| LISTOP CP {
    $$=0;
    listf=1;
    listIndex=0;
}
| NIL{$$=0;}
;

VALUES: VALUES NUMBER  {
    listValues[listIndex]=$2;
    ++listIndex;
}
| NUMBER {
    listValues[listIndex]=$1;
    ++listIndex;
}
| NIL{ $$=0; };

BinaryValue: TRUE { $$=1; }
| FALSE { $$=0; };

%% 

int yyerror(const char * ch) 
{ 
    printf("\nSYNTAX_ERROR Expression not recognized"); 
    flag=1;
    endf=1;
}

int main(){ 
    printf("-> g++ Interpreter\n");
    while(!endf)   
        yyparse();

    return 0; 
}