(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let _ = Random.self_init()

let local_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
let type_map:(string, A.typ) Hashtbl.t = Hashtbl.create 50
(* Store the underlying filenames for each shape *)
let shape_map:(string, string) Hashtbl.t = Hashtbl.create 50

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "ShapeShifter"
  and i32_t    = L.i32_type  context
  and i8_t     = L.i8_type   context
  and i1_t     = L.i1_type   context
  and void_t   = L.void_type context
  and double_t = L.double_type context in 
  (* and i64_t    = L.i64_type  context in *)
  (* let i64_pt   = L.pointer_type i64_t  *)
  (* and i32_pt   = L.pointer_type i32_t  *)
  let i8_pt    = L.pointer_type i8_t in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.String -> i8_pt 
    | A.Dbl -> double_t
    | A.Shape -> i32_t (*Make Shape an int just to be able to test translate *)  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
 
  (* Declare system functions we need *)
  let system_t = L.function_type i32_t [| i8_pt |] in
  let system_func = L.declare_function "system" system_t the_module in

  (* Declare Shapeshifter functions (need to finish enumerating) *)

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = 
      	if fdecl.A.fname = "scene" then "main" else fdecl.A.fname
      and formal_types =
	    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let name = 
      if fdecl.A.fname = "scene" then "main" else fdecl.A.fname in
    let (the_function, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Declare the strings we need *)
    (* Cork executable and commands *)
    let get_cork_cmd func args =  
      let cork_exec = "./graphics/cork/bin/cork" 
      and render_exec = "./graphics/display/sshiftdisplay" in

      let cork_cmd f = 
        (match f with 
          "Translate"         -> cork_exec ^ " -translate"
          | "Reflect"         -> cork_exec ^ " -reflect" 
          | "Rotate"          -> cork_exec ^ " -rotate" 
          | "Scale"           -> cork_exec ^ " -scale" 
          | "Union"           -> cork_exec ^ " -union"
          | "Difference"      -> cork_exec ^ " -diff"
          | "Intersect"       -> cork_exec ^ " -isct"
          | "Save"            -> "cp"
          | "Render"          -> render_exec
          | "Xor"         -> cork_exec ^ " -xor"
          | _ -> raise (Failure "Incorrect cork_cmd")
        )
      in
      (cork_cmd func) ^ " " ^ args
    
    in

    let get_prim_file p = 
      match p with 
        A.ConePrim          -> "./graphics/.primitives/cone.off" 
        | A.CubePrim        -> "./graphics/.primitives/cube.off"
        | A.CylinderPrim    -> "./graphics/.primitives/cylinder.off"
        | A.SpherePrim      -> "./graphics/.primitives/sphere.off"
        | A.TetraPrim       -> "./graphics/.primitives/tetra.off"
        | _ -> raise (Failure "Shape file for specified primitive not found")
    in
    
    let tmp_folder = "./.tmp/" in

    (* Create a temp file for each shape from random int; collisions not checked but unlikely yolo *)
    let add_shape_type ty na= 
      match ty with 
      A.Shape ->              
        let shape_file = tmp_folder ^ string_of_int(Random.int 100000000) ^ ".off" in 
        ignore (Hashtbl.add shape_map na shape_file);
      | _ -> () in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let dbl_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let _ =
      ignore (Hashtbl.clear local_vars);
      ignore (Hashtbl.clear type_map);
      ignore (Hashtbl.clear shape_map);
      let add_formal (t, n) p =
        add_shape_type t n;             
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          ignore (Hashtbl.add local_vars n local);
        in
      List.iter2 add_formal fdecl.A.formals (Array.to_list (L.params the_function))
    in

    (* Add types as necessary *)
    let rec string_of_expr = function 
      | A.Id(s)         -> s 
      | A.DblLit(s)     -> string_of_float s
      | A.IntLit(s)     -> string_of_int s
      | A.StrLit(s)     -> s
      | A.ConePrim      -> "coneprim" 
      | A.CubePrim      -> "cubeprim"
      | A.CylinderPrim  -> "cylinderprim"
      | A.SpherePrim    -> "sphereprim"
      | A.TetraPrim     -> "tetraprim"
      | A.Unop(o, z)    -> 
          (match z with
          | A.DblLit(s) -> A.string_of_uop o ^ string_of_float s 
          | _ -> raise (Failure "Invalid"))
      | A.Binop(e1, _, _) -> string_of_expr e1
      | _ -> raise (Failure "Invalid")
    in
 
    let build_string s n expr = 
	  let string_head = expr builder (A.StrLit s) in 
      let zero_const = L.const_int i32_t 0 in
      let str = L.build_in_bounds_gep string_head [| zero_const |] (n^"_str") builder in
      L.build_call system_func [| str |] n builder
    in

    let lookup n =
      try Hashtbl.find local_vars n
      with Not_found -> StringMap.find n global_vars
    in
                   
    let _ =
      let add_type (t, n) =
        Hashtbl.add type_map n t
      in
      List.iter add_type fdecl.A.formals
    in

    let lookup_type n =
      Hashtbl.find type_map n
    in

    let integer_ops op = 
      (match op with
        A.Add       -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        | _ -> raise (Failure "Integer operator not found")
      )
    in

    let double_ops op = 
      (match op with
        A.Add       -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mult    -> L.build_fmul
        | A.Div     -> L.build_fdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Ult
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
        | _ -> raise (Failure "Double operator not found")
      )
    in
    
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      | A.DblLit d -> L.const_float double_t d
      
      | A.StrLit s -> L.build_global_stringptr s "" builder
	  
	 | A.IntLit i -> L.const_int i32_t i
        
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      
      | A.Noexpr -> L.const_int i32_t 0
     
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	    let e1' = expr builder e1
	    and e2' = expr builder e2 in

	    (match e1 with 
    	  | A.IntLit _ -> (integer_ops op) e1' e2' "tmp" builder
          | A.DblLit _ -> (double_ops op) e1' e2' "tmp" builder
          | A.Id id ->  
          (* We need to match with each type that ID can take, use StringMap for storing types *)
          let variable_type = lookup_type id in
          (
            match variable_type with	  	
            | A.Dbl ->  (double_ops op) e1' e2' "tmp" builder
            | A.Int ->  (integer_ops op) e1' e2' "tmp" builder
            | _ -> raise (Failure "A.Id variable type not found")
          )

          | A.Call (st, [_]) -> (let fdecl = snd (StringMap.find st function_decls)
                                   in match fdecl.A.typ with
                                     | A.Dbl ->  (double_ops op) e1' e2' "tmp" builder
                                     | A.Int ->  (integer_ops op) e1' e2' "tmp" builder
                                     | _ -> raise (Failure "Type for call not found"))
          | _ -> raise (Failure "No match found for specified binop")                           
	    )
  
      | A.Unop(op, e) ->
	    let e' = expr builder e in
	    (match op with
	  	    | A.Neg     -> 
              (match e with
              | A.IntLit _ -> L.build_neg
              | A.DblLit _ -> L.build_fneg
              | A.Id id -> 
                  let variable_type = lookup_type id in
                    (
                      match variable_type with      
                      | A.Dbl -> L.build_fneg 
                      | A.Int -> L.build_neg
                      | _ -> raise (Failure "A.Id type not found for A.Neg")
                    )
              | _ -> raise (Failure "Invalid Operator")
              )
          | A.Not     -> L.build_not) e' "tmp" builder
          (* | _ -> raise (Failure "Unop not supported")) e' "tmp" builder *)
     
      | A.Assign (s, e) -> 
        (* Ugh shapes or something *)
        (match e with 
          A.Id id ->  
            let var_type = lookup_type id in
            (match var_type with    
            A.Shape -> 
              let sf = Hashtbl.find shape_map id in
              ignore (Hashtbl.add shape_map s sf);
              L.const_int i32_t 0
            | _ -> 
              let e' = expr builder e in
              ignore (L.build_store e' (lookup s) builder); e'
            )  
          | _ ->  
            let e' = expr builder e in
            ignore (L.build_store e' (lookup s) builder); e'
        )
     
      | A.Call ("print", [e]) ->
        let print_strlit s =
        let string_head = expr builder (s) in
        let zero_const = L.const_int i32_t 0 in
        let str = L.build_in_bounds_gep string_head [| zero_const |] "str_printf" builder in
        L.build_call printf_func [| str |] "str_printf" builder in  

  	    (match List.hd[e] with 
  		  | A.StrLit _ ->
            print_strlit (List.hd[e])
  		  | A.DblLit _     -> L.build_call printf_func [| dbl_format_str ; (expr builder e) |] "dbl_printf" builder
  		  | A.IntLit _     -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "int_printf" builder
  		  | A.Id id        -> 
  		    let variable_type = lookup_type id in
  		    (
  		      match variable_type with 
  			    | A.Dbl    -> L.build_call printf_func [| dbl_format_str ; (expr builder e) |] "dbl_printf" builder
  				  | A.Int    -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "int_printf" builder
            | A.String -> print_strlit (List.hd[e])
            | _ -> raise (Failure "Print not specified for this type")         
          )
      		  | _ -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "int_printf" builder
	        )
      
      (* Transformation calls *)
      | A.Call ("Reflect", [s; a; b; c]) -> 
        let refle_cmd = get_cork_cmd "Reflect" (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s))); 
                            string_of_expr(a); string_of_expr(b); 
                            string_of_expr(c)]) in
        build_string refle_cmd "reflf" expr;
      
      | A.Call ("Rotate", [s; x; y; z]) -> 
        let rotat_cmd = get_cork_cmd "Rotate" (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s))); 
                            string_of_expr(x); string_of_expr(y); 
                            string_of_expr(z)]) in
        build_string rotat_cmd "rotatef" expr;
      
      | A.Call ("Scale", [s; x; y; z]) -> 
        let scale_cmd = get_cork_cmd "Scale" (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s))); 
                            string_of_expr(x); string_of_expr(y); 
                            string_of_expr(z)]) in
        build_string scale_cmd "scalef" expr;
      
      | A.Call ("Translate", [s; x; y; z]) ->
        let trans_cmd = get_cork_cmd "Translate" (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s))); 
                            string_of_expr(x); string_of_expr(y); 
                            string_of_expr(z)]) in
        build_string trans_cmd "translatef" expr;
      
      | A.Call ("Save", [s; n]) -> 
        let save_cmd = get_cork_cmd "Save" (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s)));  
                            string_of_expr(n)]) in  
        build_string save_cmd "savef" expr; 
      | A.Call ("Render", [s]) -> 
        let rend_cmd = get_cork_cmd "Render" (Hashtbl.find shape_map (string_of_expr(s))) in
        build_string rend_cmd "rendf" expr; 
 
      | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
	 	let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 	let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder

      | _ -> raise (Failure "Match not found in expr builder")

    in


    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	    Some _ -> ()
        | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	  | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
			let bool_val = expr builder predicate in
			let merge_bb = L.append_block context "merge" the_function in

			let then_bb = L.append_block context "then" the_function in
			add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
			(L.build_br merge_bb);

			let else_bb = L.append_block context "else" the_function in
			add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
			(L.build_br merge_bb);

			ignore (L.build_cond_br bool_val then_bb else_bb builder);
			L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
		  let pred_bb = L.append_block context "while" the_function in
		  ignore (L.build_br pred_bb builder);

		  let body_bb = L.append_block context "while_body" the_function in
		  add_terminal (stmt (L.builder_at_end context body_bb) body)
		    (L.build_br pred_bb);

		  let pred_builder = L.builder_at_end context pred_bb in
		  let bool_val = expr pred_builder predicate in

		  let merge_bb = L.append_block context "merge" the_function in
		  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
		  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
      | A.Local (t, n, e) -> 
        let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (Hashtbl.add local_vars n local);
          ignore (Hashtbl.add type_map n t);

          add_shape_type t n;  
            
          let make_prim_cmd p n =
            ( 
              let prim_cmd = get_cork_cmd "Save" (String.concat " "
                            [(get_prim_file p); 
                            (Hashtbl.find shape_map n)]) in
              ignore (build_string prim_cmd ((string_of_expr p )^"f") expr);
              builder 
            )
          in   
 
          let make_boolop_cmd op s1 s2 n = 
            let cmd_str = get_cork_cmd op (String.concat " " 
                            [(Hashtbl.find shape_map (string_of_expr(s1))); 
                            (Hashtbl.find shape_map (string_of_expr(s2)));
                            (Hashtbl.find shape_map n)]) in
            ignore (build_string cmd_str (op^"f") expr);
            builder
          in
 
          match e with
            (* Call primitive constructors *)
            | A.ConePrim        ->
              make_prim_cmd A.ConePrim n
            | A.CubePrim        ->
              make_prim_cmd A.CubePrim n
            | A.CylinderPrim    -> 
              make_prim_cmd A.CylinderPrim n
            | A.SpherePrim      ->
              make_prim_cmd A.SpherePrim n
            | A.TetraPrim       -> 
              make_prim_cmd A.TetraPrim n
            (* Boolean shape operations create a new shape*)
            | A.Call ("Union", [s1; s2]) -> 
              make_boolop_cmd "Union" s1 s2 n
            | A.Call ("Intersect", [s1; s2]) ->
              make_boolop_cmd "Intersect" s1 s2 n 
            | A.Call ("Difference", [s1; s2]) -> 
              make_boolop_cmd "Difference" s1 s2 n
            | A.Call ("Xor", [s1; s2]) -> 
              make_boolop_cmd "Xor" s1 s2 n
            | A.Noexpr -> builder
        (*
            | A.Id id->
              print_string("waitwhat"); 
              let variable_type = lookup_type id in
              (match variable_type with 
                A.Shape -> 
                  (* Set both shapes to have same file representation *)
                  let sf = Hashtbl.find shape_map id in
                  ignore (Hashtbl.add shape_map n sf);
                  print_string(sf);
                  builder
                | _ -> 
                  print_string("uncool");
                  let e' = expr builder e in 
                  ignore (L.build_store e' (lookup n) builder); 
                  builder
              ) 
        *)
            | _ -> 
              let e' = expr builder e in
              ignore (L.build_store e' (lookup n) builder);
              builder
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
