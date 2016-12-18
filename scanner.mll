(* Ocamllex scanner for ShapeShifter *)

{ 
  open Parser 
  
  let strip_string s = 
    String.sub s 1 ((String.length s) - 2) 


}

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
  | '['             { LBRACK }      
  | ']'             { RBRACK }      
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
  | "UN"            { UNION }       
  | "IN"            { INTERSECT }   
  | "DI"            { DIFFERENCE }  
  | "if"            { IF }
  | "else"          { ELSE }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "return"        { RETURN }
  | "int"           { INT }
  | "double"        { DBL }         
  | "string"        { STRING }      
  | "bool"          { BOOL }
  | "Shape"         { SHAPE }       
  | "SPHERE"        { SPHERE_PRIM }  
  | "CUBE"          { CUBE_PRIM }    
  | "CYLINDER"      { CYLINDER_PRIM } 
  | "TETRA"         { TETRA_PRIM }   
  | "CONE"          { CONE_PRIM }    
  | "void"          { VOID }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | integer as lxm  { INT_LIT(int_of_string lxm) }
  | double as lxm   { DBL_LIT(float_of_string lxm) }
  | str_lit as lxm  { STR_LIT(strip_string lxm) }
  | id as lxm       { ID(lxm) }
  | _ as char       { raise (Failure("illegal character " ^ Char.escaped char)) }
  | eof             { EOF }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | _    { single_line_comment lexbuf }

and multi_line_comment = parse
  | "*/" { token lexbuf }
  | _    { multi_line_comment lexbuf }
