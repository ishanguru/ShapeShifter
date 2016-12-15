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
let shape_map:(string, string) Hashtbl.t = Hashtbl.create 50
(* Store the underlying filenames for each shape *)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "ShapeShifter"
  and i32_t    = L.i32_type  context
  and i8_t     = L.i8_type   context
  and i1_t     = L.i1_type   context
  and void_t   = L.void_type context
  and double_t = L.double_type context
  and i64_t    = L.i64_type  context in
  let i64_pt   = L.pointer_type i64_t 
  and i32_pt   = L.pointer_type i32_t 
  and i8_pt    = L.pointer_type i8_t in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.String -> i8_pt 
    | A.Dbl -> double_t
    | A.Shape -> i8_pt (*Make Shape an int just to be able to test translate *)  in

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
  
    let cork_exec = "./graphics/cork/bin/cork" 
      and cork_trans = "-translate" 
      and render_exec = "./graphics/display/sshiftdisplay"
      and tetra_file = "./graphics/.primitives/tetra.off"
      and cube_file = "./graphics/.primitives/cube.off"
      and tmp_folder = "./.tmp/"
    in


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
    
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      | A.DblLit d -> L.const_float double_t d
      
      | A.StrLit s -> L.build_global_stringptr s "" builder
	  
	  | A.IntLit i -> L.const_int i32_t i
      
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      
      | A.Noexpr -> L.const_int i32_t 0
     
    (* Seems to be redefined later on? *) 
    (*  | A.Id s -> L.build_load (lookup s) s builder *)

      (* Shouldn't ever actually evaluate the primitives like this *)
   (*   | A.TetraPrim -> L.const_int i32_t 0
      | A.CubePrim -> L.const_int i32_t 0    
  *)
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in

	  (match e1 with 

	  	| A.IntLit i -> 
		  (match op with
		    	A.Add     -> L.build_add
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
		  ) e1' e2' "tmp" builder

		| A.DblLit d -> 
		  (match op with
		    	A.Add     -> L.build_fadd
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
		  ) e1' e2' "tmp" builder

		| A.Id id ->  
		  (* We need to match with each type that ID can take, use StringMap for storing types *)
		  let variable_type = lookup_type id in
		  (
		  	match variable_type with
		  	
		  	| A.Dbl ->  (match op with
		    				A.Add     -> L.build_fadd
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
					  	) e1' e2' "tmp" builder
		  	| A.Int ->  (match op with
					    	A.Add     -> L.build_add
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
					  	) e1' e2' "tmp" builder
		  )
			  
	  )
      
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	  	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder

(* It looks like Assign is never invoked; rather you have Local(t, n, e) *) 
(* 
      | A.Assign (s, e) -> 
        (match e with
          A.CubePrim -> 

            let _ = print_string "cubething" in 
            let cmd_list = ["cp"; cube_file; (Hashtbl.find shape_map s)] in
            let cmd_str = String.concat " " cmd_list in 

	        let string_head = expr builder (A.StrLit cmd_str) in 
      	    let zero_const = L.const_int i32_t 0 in
            let str = L.build_in_bounds_gep string_head [| zero_const |] "cubeconstr_str" builder in
            L.build_call system_func [| str |] "cubeconstr" builder


          | _ ->  
            let _ = print_string "not cube thing" in 
            let e' = expr builder e in
	            ignore (L.build_store e' (lookup s) builder); e')
*)
      | A.Call ("print", [e]) ->
            let print_strlit s =
                let string_head = expr builder (s) in
                let zero_const = L.const_int i32_t 0 in
                let str = L.build_in_bounds_gep string_head [| zero_const |] "str_printf" builder in
                L.build_call printf_func [| str |] "str_printf" builder in  

      	(match List.hd[e] with 

      		| A.StrLit s ->
                print_strlit (List.hd[e])
      		| A.DblLit d -> L.build_call printf_func [| dbl_format_str ; (expr builder e) |] "dbl_printf" builder
      		| A.IntLit i -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "int_printf" builder
      		| A.Id id -> 
      				let variable_type = lookup_type id in
      				(
      					match variable_type with 
      					| A.Dbl -> L.build_call printf_func [| dbl_format_str ; (expr builder e) |] "dbl_printf" builder
      					| A.Int -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "int_printf" builder
                        | A.String -> print_strlit (List.hd[e])         
                            	
                    )

	    )

      | A.Call ("Translate", [s; x; y; z]) ->
      		(* Turn x into a string here - also, link file to resource folder *)
            let string_of_expr = function 
              | A.Id(s) -> s 
              | A.DblLit(s) -> string_of_float s in 
            let transa_list = [cork_exec; cork_trans; 
                               (Hashtbl.find shape_map (string_of_expr(s))); 
                               string_of_expr(x); string_of_expr(y); 
                               string_of_expr(z)] in
            let transcmd_str = String.concat " " transa_list in            

	        let string_head = expr builder (A.StrLit transcmd_str) in 
      	    let zero_const = L.const_int i32_t 0 in
            let str = L.build_in_bounds_gep string_head [| zero_const |] "transcall_str" builder in
            L.build_call system_func [| str |] "translatef" builder
      | A.Call ("Save", [s; n]) ->
      		(* Turn x into a string here - also, link file to resource folder *)
            let string_of_expr = function 
              | A.Id(s) -> s 
              | A.StrLit(s) -> s in
            let transa_list = ["cp"; (Hashtbl.find shape_map (string_of_expr(s))); string_of_expr(n)] in  
            let transcmd_str = String.concat " " transa_list in            

	        let string_head = expr builder (A.StrLit transcmd_str) in 
      	    let zero_const = L.const_int i32_t 0 in
            let str = L.build_in_bounds_gep string_head [| zero_const |] "transcall_str" builder in
            L.build_call system_func [| str |] "savef" builder
      | A.Call ("Render", [s]) -> 
            let string_of_expr = function
                A.Id(s) -> s in
            let renda_list = [render_exec; (Hashtbl.find shape_map (string_of_expr(s)))] in
            let rendcmd_str = String.concat " " renda_list in
            let string_head = expr builder (A.StrLit rendcmd_str) in
            let zero_const = L.const_int i32_t 0 in
            let str = L.build_in_bounds_gep string_head [| zero_const |] "rendcall_str" builder in
            L.build_call system_func [| str |] "renderf" builder

      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
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

          match e with
            | A.CubePrim -> 
                let cmd_list = ["cp"; cube_file; (Hashtbl.find shape_map n)] in
                let cmd_str = String.concat " " cmd_list in 

	            let string_head = expr builder (A.StrLit cmd_str) in 
      	        let zero_const = L.const_int i32_t 0 in
                let str = L.build_in_bounds_gep string_head [| zero_const |] "cubeconstr_str" builder in
                L.build_call system_func [| str |] "cubeconstr" builder; 
                builder
            | A.Noexpr -> builder
            | _ -> let e' = expr builder e in
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
