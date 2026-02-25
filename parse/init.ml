let init_rocq_env () =
  Unix.putenv "OCAMLFIND_CONF" "/dev/null";
  Coqinit.init_ocaml ();

  let coqargs, _extra = Coqinit.parse_arguments
    ~parse_extra:(fun _ x -> (), x)
    []
  in
  let usage = Boot.Usage.{
    executable_name = "rocqparse"; 
    extra_args = "";
    extra_options = ""
  } in
  Coqinit.init_runtime ~usage coqargs;

  (* Coqinit.dirpath_of_top (TopPhysical "...") *)
  let top = Names.DirPath.make [Names.Id.of_string "Top"] in

  Coqinit.start_library
    ~intern:Vernacinterp.fs_intern
    ~top
    coqargs.pre.injections
