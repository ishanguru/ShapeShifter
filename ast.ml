(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq |
          Greater |Geq | And | Or | Union | Intersect |
          Difference

type uop = Neg | Not

type typ = Int | Dbl | Bool | String | Shape | Sphere | Cube |
           Tetra | Cone | Cylinder | Void

type bind = typ * string

type expr =
  | IntLit of int
  | DblLit of float
  | StrLit of string
  | BoolLit of bool
  | SphereObj
  | CubeObj
  | CylinderObj
  | TetraObj
  | ConeObj
  | Null
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Union -> "UN" (* ADDED BY US *)
  | Intersect -> "IN" (* ADDED BY US *)
  | Difference -> "DI" (* ADDED BY US *)

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
  | IntLit(l) -> string_of_int l
  | DblLit(l) -> string_of_float l
  | StrLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | SphereObj -> "SPHERE"
  | CubeObj -> "CUBE"
  | CylinderObj -> "CYLINDER"
  | TetraObj -> "TETRA"
  | ConeObj -> "CONE"
  | Null -> "NULL"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Dbl -> "double"
  | String -> "string"
  | Shape -> "Shape"
  | Sphere -> "Sphere"
  | Cube -> "Cube"
  | Tetra -> "Tetra"
  | Cone -> "Cone"
  | Cylinder -> "Cylinder"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
