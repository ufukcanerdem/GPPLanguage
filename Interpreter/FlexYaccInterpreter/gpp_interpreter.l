%{  
    /*
    UFUKCAN ERDEM
    1901042686
    CSE341 HW3-PART1
    */ 
    #include<stdio.h> 
    #include "y.tab.h" 
    extern int yylval; 
    int counter = 0; 
%}

    COMMENT ";;".*
    VALUE [1-9][0-9]*|"0"
    IDENTIFIER [a-zA-Z][a-zA-Z0-9]*

%%
    {COMMENT} { return COMMENT; }
    and { return AND; }
    or { return OR; }
    not { return NOT; }
    equal { return EQUAL; }
    less { return LESS; }
    nil { return NIL; }
    list { return LIST; }
    append { return APPEND; }
    concat { return CONCAT; }
    set { return SET; }
    deffun { return DEFFUN; }
    defvar { return DEFVAR; }
    for { return FOR; }
    while { return WHILE; }
    if { return IF; }
    exit { return EXIT; }
    load { return LOAD; }
    disp { return DISP; }
    true { return TRUE; }
    false { return FALSE; }
    "'(" { return LISTOP; }
    "+" { return PLUS; }
    "-" { return MINUS; }
    "/" { return DIV; }
    "*" { return MULT; }
    "(" { return OP; }
    ")" { return CP;}
    "**" { return DBLMULT; }
    "\"" {if(!counter%2)
            return OC;
        else
            return CC;
        ++counter;}
    "," { return COMMA; }

    {VALUE} {
        yylval=atoi(yytext); 
        return NUMBER; 
    }

    {IDENTIFIER} { return IDENTIFIER; }

    " "|"\t" { } 
    "\n" {printf("%s\n",yytext); return 0; }
    .	 {printf("SYNTAX ERROR, %s IS NOT RECOGNIZED",yytext); 
          return 0;} 
    %%

    int yywrap() { 
        return 1; 
    } 
