open Rocqparse

let () =
  let code = Sys.argv.(1) in
  let ast = parse_str code in

  print_endline "Pretty-printed:\n";
  List.iter (fun x -> print_endline @@ layout_vernac x) ast;

  print_endline "\nVernac AST:\n";
  List.iter (fun x -> print_endline @@ layout_vernac_ast x) ast;
