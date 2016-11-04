(* Ocamllex scanner for ShapeShifter *)

{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let integer = digit+
let double = digit+ '.' digit+
let str_lit = ['\"'] [^'\"']* ['\"']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | whitespace      { token lexbuf } (* Whitespace *)
  | "//"            { single_line_comment lexbuf }
  | "/*"            { multi_line_comment lexbuf } (* Comments *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '['             { LBRACK }      (* ADDED BY US *)
  | ']'             { RBRACK }      (* ADDED BY US *)
  | ';'             { SEMI }
  | ','             { COMMA }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '='             { ASSIGN }
  | "=="            { EQ }
  | "!="            { NEQ }
  | '<'             { LT }
  | "<="            { LEQ }
  | ">"             { GT }
  | ">="            { GEQ }
  | "&&"            { AND }
  | "||"            { OR }
  | "!"             { NOT }
  | "UN"            { UNION }       (* ADDED BY US *)
  | "IN"            { INTERSECT }   (* ADDED BY US *)
  | "DI"            { DIFFERENCE }  (* ADDED BY US *)
  | "if"            { IF }
  | "else"          { ELSE }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "return"        { RETURN }
  | "break"         { BREAK }
  | "int"           { INT }
  | "double"        { DBL }         (* ADDED BY US *)
  | "string"        { STRING }      (* ADDED BY US *)
  | "bool"          { BOOL }
  | "Shape"         { SHAPE }       (* ADDED BY US *)
  | "Sphere"        { SPHERE }      (* ADDED BY US *)
  | "Cube"          { CUBE }        (* ADDED BY US *)
  | "Tetra"         { TETRA }       (* ADDED BY US *)
  | "Cone"          { CONE }        (* ADDED BY US *)
  | "Cylinder"      { CYLINDER }    (* ADDED BY US *)
  | "SPHERE"        { SPHERE_OBJ }  (* ADDED BY US *)
  | "CUBE"          { CUBE_OBJ }    (* ADDED BY US *)
  | "CYLINDER"      { CYLINDER_OBJ } (* ADDED BY US *)
  | "TETRA"         { TETRA_OBJ }   (* ADDED BY US *)
  | "CONE"          { CONE_OBJ }    (* ADDED BY US *)
  | "NULL"          { NULL }        (* ADDED BY US *)
  | "void"          { VOID }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | integer as lxm  { INT_LIT(int_of_string lxm) }
  | double as lxm   { DBL_LIT(float_of_string lxm) } (* ADDED BY US *)
  | str_lit as lxm  { STR_LIT(lxm) } (* ADDED BY US *)
  | id as lxm       { ID(lxm) }
  | _ as char       { raise (Failure("illegal character " ^ Char.escaped char)) }
  | eof             { EOF }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | _    { single_line_comment lexbuf }

and multi_line_comment = parse
  | "*/" { token lexbuf }
  | _    { multi_line_comment lexbuf }
