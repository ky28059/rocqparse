let init_rocq_env () =
  Coqinit.init_ocaml ();

  let usage = { 
    Boot.Usage.executable_name = "rocqparse"; 
    extra_args = " [options] file.v";
    extra_options = ""
  } in

  let coqargs, _extra = Coqinit.parse_arguments
    ~parse_extra:(fun _ x -> (), x) (* Identity for extra args *)
    []
  in

  Coqinit.init_runtime ~usage coqargs;

  (* let injections = coqargs.Coqargs.injections in *)
  let top = Names.DirPath.make [Names.Id.of_string "Top"] in
  Coqinit.start_library
    ~intern:Vernacinterp.fs_intern
    ~top
    []
