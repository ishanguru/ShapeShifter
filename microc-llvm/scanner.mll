(* Ocamllex scanner for MicroC *)

{ open Parser }

(* 
	Stuff we need to add: 
	
	'.' -> Shape.properties [dot notation] (look up from a language like DICE)


	Data structures we're supporting: 

	Preallocated arrays
	Lists

	Primitives: 

	int
	double
	vector (vec3d or vec3i)
	strings
	c-style arrays
	points (?) -> Incase we want to do corners, but not sure how we're representing that

	Tasks for this week: 

	Need to have the scanner and parser done

 *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '.'	   { DOT } (* ADDED BY US *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "UU"	   { UNION } (* ADDED BY US *)
| "NU"	   { INTERSECT } (* ADDED BY US *)
| "if"     { IF }
| "elif"   { ELIF } (* ADDED BY US *)
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "double" { DOUBLE } (* ADDED BY US *)
| "bool"   { BOOL }
| "shape"  { SHAPE } (* ADDED BY US *)
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
| string 	{ STRING_LITERAL(str) } (* ADDED BY US *)

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
