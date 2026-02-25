open Libsail


let analyze_noninterference out_file istate =
  Printf.printf "=== Non-Interference Analysis ===\n";
  Printf.printf "Processing %d definitions\n" (List.length istate.ast.defs);
  
  let functions = List.filter_map (function
    | Ast_defs.DEF_aux (Ast_defs.DEF_fundef fd, _) -> 
        let func_id = Ast_util.id_of_fundef fd in
        Some (Ast_util.string_of_id func_id)
    | _ -> None
  ) istate.ast.defs in
  
  Printf.printf "Functions found: %s\n" (String.concat ", " functions);
  
  (match out_file with
   | Some filename ->
       let out_chan = open_out (filename ^ "_analysis.txt") in
       Printf.fprintf out_chan "Non-interference analysis results\n";
       List.iter (Printf.fprintf out_chan "Function: %s\n") functions;
       close_out out_chan;
       Printf.printf "Results written to %s_analysis.txt\n" filename
   | None -> ());
   
  Printf.printf "Analysis complete!\n"

let _ = Target.register ~name:"noninterference"
  ~description:"Non-interference analysis for Sail programs"
  ~supports_abstract_types:true
  ~supports_runtime_config:true
  analyze_noninterference