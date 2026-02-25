open Parse

let () =
  init_rocq_env ();

  let ast = parse_file "/home/ky28059/rocqparse/data/test2.v" in
  let filtered = List.filter_map (fun x -> x) ast in
  print_endline @@ String.concat "\n\n" @@ List.map layout_vernac filtered
