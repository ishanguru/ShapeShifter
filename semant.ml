(* Semantic checking for the ShapeShifter compiler *)

open Ast

module StringMap = Map.Make(String)

let global_idents:(string, typ) Hashtbl.t = Hashtbl.create 50

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =
  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
    | (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet = rvaluet then lvaluet else
     if rvaluet = Void then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =
      List.fold_left (fun map (key, value) ->
          StringMap.add key value map
      ) StringMap.empty [
          ("Copy", { typ = Void; fname = "Copy"; formals = []; body = [] });
          ("Render", { typ = Void; fname = "Render"; formals = []; body = [] });
          ("Save", { typ = Void; fname = "Save"; formals = []; body = [] });
          ("Copy", { typ = Void; fname = "Copy"; formals = []; body = [] });
          ("Difference", { typ = Void; fname = "Difference"; formals = []; body = [] });
          ("Intersect", { typ = Void; fname = "Intersect"; formals = []; body = [] });
          ("Reflect", { typ = Void; fname = "Reflect"; formals = []; body = [] });
          ("Union", { typ = Void; fname = "Union"; formals = []; body = [] });
          ("Scale", { typ = Void; fname = "Scale"; formals = []; body = [] });
          ("Rotate", { typ = Void; fname = "Rotate"; formals = []; body = [] });
          ("Translate", { typ = Void; fname = "Translate"; formals = []; body = [] });
          ("print", { typ = Void; fname = "print"; formals = [(Int, "x")]; body = [] });
          ("printb", { typ = Void; fname = "printb"; formals = [(Bool, "x")]; body = [] });
      ]
  in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
    built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "scene" in (* Ensure "scene" is defined *)

  let _ =
      let add_symbol (t, n) =
          Hashtbl.add global_idents n t
      in
      List.iter add_symbol globals
  in

  let check_function func =
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (* Type of each variable (global, formal, or local *)
    let local_idents:(string, typ) Hashtbl.t = Hashtbl.create 50 in
    let _ =
        let add_symbol (t, n) =
            Hashtbl.add local_idents n t
        in
        List.iter add_symbol (func.formals)
    in

    let type_of_identifier s =
      try Hashtbl.find local_idents s
      with Not_found -> try Hashtbl.find global_idents s
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	  | IntLit _ -> Int
      | DblLit _ -> Dbl
      | StrLit _ -> String
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
          (match op with
            | Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
            | Add | Sub | Mult | Div when t1 = Dbl && t2 = Dbl -> Dbl
            | Equal | Neq when t1 = t2 -> Bool
            | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
            | Less | Leq | Greater | Geq when t1 = Dbl && t2 = Dbl -> Bool
            | And | Or when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                          string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                          string_of_typ t2 ^ " in " ^ string_of_expr e)))
      | Unop(op, e) as ex -> let t = expr e in
          (match op with
              Neg when t = Int -> Int
            | Not when t = Bool -> Bool
            | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                          string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
          check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				              " = " ^ string_of_typ rt ^ " in " ^ 
				              string_of_expr ex))
      | Call(fname, _) -> let fd = function_decl fname in fd.typ
      | SpherePrim -> Shape
      | CubePrim -> Shape
      | CylinderPrim -> Shape
      | TetraPrim -> Shape
      | ConePrim -> Shape
    in

    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else ()
    in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	  | Block sl -> let rec check_block = function
          | [Return _ as s] -> stmt s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> check_block (sl @ ss)
          | s :: ss -> stmt s; check_block ss
          | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
          raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
      | Local(t, s, e) -> ignore (Hashtbl.add local_idents s t);
          if expr e = Void then () else
          if expr e = t then () else
          raise (Failure ("expression has type " ^ string_of_typ (expr e) ^
          " expected " ^ string_of_typ t))
    in
    stmt (Block func.body)
  in
  List.iter check_function functions
