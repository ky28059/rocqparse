open Parse

let () =
  init_rocq_env ();

  let code = Sys.argv.(1) in
  let ast = parse_str code in

  (* let ast = parse_file "/home/ky28059/rocqparse/data/test2.v" in *)

  List.iter (fun x -> print_endline @@ layout_vernac x) ast
