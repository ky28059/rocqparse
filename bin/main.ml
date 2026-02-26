open Rocqparse

let () =
  let code = Sys.argv.(1) in
  let ast = parse_str code in

  List.iter (fun x -> print_endline @@ layout_vernac x) ast
