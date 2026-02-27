# rocqparse
A simple Rocq AST parser / explorer for Rocq >= 9.0.0, built using the `rocq-runtime` / `coq-core` syntax parser.

```bash
dune exec ./bin/main.exe "Module Axioms. Lemma foo : 1 = 1. Proof. reflexivity. Qed. End Axioms."
```
```
Pretty-printed:

Module Axioms.
Lemma foo : 1 = 1.
Proof.
reflexivity.
Qed.
End Axioms.

Vernac AST:

(VernacSynterp (VernacDefineModule (..., "Axioms", ..., ..., ...))
(VernacSynPure (VernacStartTheoremProof (Lemma, [(("foo", ...), (..., (CNotation (..., (InConstrEntry, "_ = _"), ([(CPrim 1), (CPrim 1)], ..., ..., ...)))))])))
(VernacSynPure (VernacProof (None, None)) (* Proof *))
(VernacSynterp (VernacExtend ...))
(VernacSynPure (VernacEndProof (Proved (Opaque, None))) (* Qed *))
(VernacSynterp (VernacEndSegment "Axioms"))
```
The parser also supports arbitrary notation:
```bash
dune exec ./bin/main.exe "Notation \"x :: y\" := (cons x y) (at level 60, right associativity). Notation \"[]\" := nil. Notation \"[ x ]\" := (cons x nil). Notation \"[ x ; .. ; y ]\" := (cons x .. (cons y nil) ..). Theorem foo : (cons 1 [2; 3]) = [1; 2; 3]. reflexivity. Qed."
```
```
Pretty-printed:

Notation "x :: y" := (cons x y) ( at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x ]" := (cons x nil).
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).
Theorem foo : cons 1 [2; 3] = [1; 2; 3].
reflexivity.
Qed.

Vernac AST:

(VernacSynterp (VernacNotation ...))
(VernacSynterp (VernacNotation ...))
(VernacSynterp (VernacNotation ...))
(VernacSynterp (VernacNotation ...))
(VernacSynPure (VernacStartTheoremProof (Theorem, [(("foo", ...), (..., (CNotation (..., (InConstrEntry, "_ = _"), ([(CApp ((CRef ("cons", ...)), [((CPrim 1), ...), ((CNotation (..., (InConstrEntry, "[ _ ; .. ; _ ]"), ([], ..., ..., ...))), ...)])), (CNotation (..., (InConstrEntry, "[ _ ; .. ; _ ]"), ([], ..., ..., ...)))], ..., ..., ...)))))])))
(VernacSynterp (VernacExtend ...))
(VernacSynPure (VernacEndProof (Proved (Opaque, None))) (* Qed *))
```

### Parsing
The main loop is
```ocaml
let parse_all_ast parsable =
  if not !Init.env_initialized then
    Init.init_rocq_env ();

  let rec f parser =
    let mode = Synterp.get_default_proof_mode () in
    let entry = Pvernac.main_entry (Some mode) in
    match Procq.Entry.parse entry parser with
    | None -> []
    | Some ast ->
      let _ = Synterp.synterp_control ~intern:Vernacinterp.fs_intern ast in
      ast :: f parser
  in
  f parsable
```
using `Pvernac` to parse an entry as a `vernac_control`, and calling `Synterp` on the returned AST to evaluate notation in future entries.

Note that in order for the parser to start up with standard library `Prelude` symbols in scope, we need to initialize the Rocq runtime with injections.
```ocaml
let init_rocq_env () =
  (* ... *)
  Coqinit.init_ocaml ();

  let opts, _ = Coqargs.parse_args ~init:Coqargs.default [] in
  let usage = Boot.Usage.{
    executable_name = "rocqparse"; 
    extra_args = "";
    extra_options = ""
  } in
  Coqinit.init_runtime ~usage opts;
  Coqinit.init_document opts;

  let injections = Coqargs.injection_commands opts in
  let top = Names.DirPath.make [Names.Id.of_string "Top"] in

  Coqinit.start_library
    ~intern:Vernacinterp.fs_intern
    ~top
    injections
```
(much of this was reverse-engineered from the Vsrocq language server [here](https://github.com/rocq-prover/vsrocq/blob/ce10079f46daa423e0a79e184541663b8a027a1f/language-server/tests/common.ml#L34).)
