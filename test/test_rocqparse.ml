open Alcotest

let print = List.iter @@ fun x -> print_endline @@ Rocqparse.layout_vernac x


let test_parse_simple () =
  print @@ Rocqparse.parse_str "Theorem foo : True. Proof. Qed."

let test_parse_simple_proof () =
  print @@ Rocqparse.parse_str "Theorem foo : True. Proof. reflexivity. Qed."

let test_parse_notation_proof () =
  print @@ Rocqparse.parse_str "Lemma foo : 1 = 1. Proof. reflexivity. Qed."

let test_parse_notation_proof2 () =
  print @@ Rocqparse.parse_str "Lemma foo : 1 + (2 * 3) = 7. Proof. lia. Qed."

let test_parse_proof_complex () =
  print @@ Rocqparse.parse_str {|
  Lemma list_hd_no_emp : forall (l : list Z), forall (x : Z), hd l x -> ~emp l.
  Proof.
    intros [| x'] x H; simpl; intros H2; contradiction.
  Qed.
  |}

let () =
  Rocqparse.init_rocq_env ();
  Alcotest.run "Parse" [
    "parse", [
      test_case "Simple" `Quick test_parse_simple;
      test_case "Simple proof" `Quick test_parse_simple_proof;
      test_case "Notation proof" `Quick test_parse_notation_proof;
      test_case "Notation proof 2" `Quick test_parse_notation_proof2;
      test_case "Complex proof" `Quick test_parse_proof_complex;
    ]
  ]
