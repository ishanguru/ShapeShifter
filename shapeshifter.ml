 (* Top-level of the ShapeShifter compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | PrettyPrint | LLVM_IR | Compile | Help 


let get_info = (
	"Usage: ./shapeshifter [optional flag] < <source file>\n")

let _ =
  (*try *)
  	let action = 
		if Array.length Sys.argv > 1 then
    			List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
                              		("-p", PrettyPrint); (* Pretty-print the AST *)
                              		("-l", LLVM_IR);  (* Generate LLVM, don't check *)
                              		("-c", Compile); (* Generate, check LLVM IR *)
					                        ("-h", Help) ] (* Usage & option info *)
  		else Compile in
  			
	let lexbuf = Lexing.from_channel stdin in
  	let ast = Parser.program Scanner.token lexbuf in
  	Semant.check ast;
  	
	match action with
    		| Help -> print_string get_info
		    | Ast -> print_string (Ast.string_of_program ast)
    		| PrettyPrint -> print_string (Prettyprint.string_of_program ast)
    		| LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    		| Compile -> let m = Codegen.translate ast in
        				Llvm_analysis.assert_valid_module m;
        				print_string (Llvm.string_of_llmodule m)
 

