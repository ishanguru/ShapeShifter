(* Ocamllex scanner for ShapeShifter *)

{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let integer = digit+
let double = digit+ '.' digit+
let str_lit = ['\"'] [^'\"']* ['\"']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | whitespace      { token lexbuf } (* Whitespace *)
  | "/*"            { comment lexbuf } (* Comments *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ';'             { SEMI }
  | ','             { COMMA }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '='             { ASSIGN }
  | '.'             { DOT }         (* ADDED BY US *)
  | "=="            { EQ }
  | "!="            { NEQ }
  | '<'             { LT }
  | "<="            { LEQ }
  | ">"             { GT }
  | ">="            { GEQ }
  | "&&"            { AND }
  | "||"            { OR }
  | "!"             { NOT }
  | "UU"            { UNION }       (* ADDED BY US *)
  | "NU"            { INTERSECT }   (* ADDED BY US *)
  | "if"            { IF }
  | "elif"          { ELIF }        (* ADDED BY US *)
  | "else"          { ELSE }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "return"        { RETURN }
  | "int"           { INT }
  | "double"        { DOUBLE }      (* ADDED BY US *)
  | "string"        { STRING }      (* ADDED BY US *)
  | "bool"          { BOOL }
  | "SHAPE"         { SHAPE }       (* ADDED BY US *)
  | "SPHERE"        { SPHERE }      (* ADDED BY US *)
  | "CUBE"          { CUBE }        (* ADDED BY US *)
  | "TETRA"         { TETRA }       (* ADDED BY US *)
  | "CONE"          { CONE }        (* ADDED BY US *)
  | "CYLINDER"      { CYLINDER }    (* ADDED BY US *)
  | "void"          { VOID }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | integer as lxm  { INTEGER(int_of_string lxm) }
  | double as lxm   { DOUBLE(float_of_string lxm) } (* ADDED BY US *)
  | str_lit as lxm  { STR_LIT(lxm) } (* ADDED BY US *)
  | id as lxm       { ID(lxm) }
  | _ as char       { raise (Failure("illegal character " ^ Char.escaped char)) }
  | eof             { EOF }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }
