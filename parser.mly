/* Ocamlyacc parser for ShapeShifter */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token UNION INTERSECT DIFFERENCE
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE BREAK
%token INT BOOL DBL STRING VOID
%token SHAPE SPHERE CUBE TETRA CONE CYLINDER
%token SPHERE_OBJ CUBE_OBJ CYLINDER_OBJ TETRA_OBJ CONE_OBJ
%token <int> INT_LIT
%token <string> ID
%token <string> STR_LIT
%token <float> DBL_LIT
%token NULL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left UNION INTERSECT DIFFERENCE
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
 | decls EOF { $1 }

decls:
 | /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
 | typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8 } }

formals_opt:
  | /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
  | typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
  | INT { Int }
  | DBL { Dbl }
  | BOOL { Bool }
  | STRING { String }
  | SHAPE { Shape }
  | SPHERE { Sphere }
  | CUBE { Cube }
  | TETRA { Tetra }
  | CONE { Cone }
  | CYLINDER { Cylinder }
  | VOID { Void }

vdecl_list:
  | /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
  | /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI { Break }

expr_opt:
  | /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  | INT_LIT          { IntLit($1) }
  | DBL_LIT          { DblLit($1) }
  | STR_LIT          { StrLit($1) }
  | SPHERE_OBJ       { SphereObj }
  | CUBE_OBJ         { CubeObj }
  | CYLINDER_OBJ     { CylinderObj }
  | TETRA_OBJ        { TetraObj }
  | CONE_OBJ         { ConeObj }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | NULL             { Null }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr UNION  expr { Binop($1, Union, $3) }
  | expr INTERSECT expr { Binop($1, Intersect, $3) }
  | expr DIFFERENCE expr { Binop($1, Difference, $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
  | /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
