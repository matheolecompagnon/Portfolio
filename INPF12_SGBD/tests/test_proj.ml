(*
  TESTS EXHAUSTIFS SUR LA FONCTION PROJECTION
*)

(* table de base correcte *)
let proj_table = {
  cols =
    ("A.id",(TInt,true))::
    ("A.name",(TText,true))::
    ("A.age",(TInt,true))::[];
  rows = [
    [VInt 1; VText "Alice"; VInt 20];
    [VInt 2; VText "Bob"; VInt 25];
  ];
};;

(* 1. projection sur un seul champ *)
let test_projection_one_field =
  let t = projection proj_table [("A.id",(TInt,true))] in
  assert (t.cols = [("A.id",(TInt,true))]);
  assert (t.rows = [[VInt 1]; [VInt 2]]);
  t
;;

(* 2. projection sur plusieurs champs *)
let test_projection_multiple_fields =
  let t = projection proj_table [
    ("A.id",(TInt,true));
    ("A.name",(TText,true))
  ] in
  assert (t.cols = [
        ("A.id",(TInt,true));
        ("A.name",(TText,true))
  ]);
  assert (t.rows = [
    [VInt 1; VText "Alice"];
    [VInt 2; VText "Bob"]
  ]);
  t
;;

(* 3. respect de l'ordre des champs *)
let test_projection_order =
  let t = projection proj_table [
    ("A.name",(TText,true));
    ("A.id",(TInt,true))
  ] in
  assert (t.cols = [
    ("A.name",(TText,true));
    ("A.id",(TInt,true))
  ]);
  assert (t.rows = [
    [VText "Alice"; VInt 1];
    [VText "Bob"; VInt 2]
  ]);
  t
;;

(* 4. projection sur tous les champs *)
let test_projection_all_fields =
  let t = projection proj_table proj_table.cols in
  assert (t.cols = proj_table.cols);
  assert (t.rows = proj_table.rows);
  t
;;

(* 5. projection avec champ inexistant *)
let test_projection_unknown_field =
  let t = projection proj_table [("A.unknown",(TInt,true))] in
  assert (t.cols = []);
  assert (t.rows = [[]; []]);
  t
;;

(* 6. projection avec mélange champs valides + invalides *)
let test_projection_partial_invalid =
  let t = projection proj_table [
    ("A.id",(TInt,true));
    ("A.unknown",(TInt,true))
  ] in
  assert (t.cols = [("A.id",(TInt,true))]);
  assert (t.rows = [[VInt 1]; [VInt 2]]);
  t
;;

(* 7. projection avec champs dupliqués *)
let test_projection_duplicates =
  let t = projection proj_table [
    ("A.id",(TInt,true));
    ("A.id",(TInt,true))
  ] in
  assert (t.cols = [
    ("A.id",(TInt,true));
    ("A.id",(TInt,true))
  ]);
  assert (t.rows = [
    [VInt 1; VInt 1];
    [VInt 2; VInt 2]
  ]);
  t
;;

(* 8. projection sur table vide *)
let proj_empty = {
  cols = proj_table.cols;
  rows = [];
};;

let test_projection_empty =
  let t = projection proj_empty [("A.id",(TInt,true))] in
  assert (t.cols = [("A.id",(TInt,true))]);
  assert (t.rows = []);
  t
;;

(* 9. projection avec liste de champs vide *)
let test_projection_no_fields =
  let t = projection proj_table [] in
  assert (t.cols = []);
  assert (t.rows = [[]; []]);
  t
;;

(* 10. table invalide *)
let proj_invalid = {
  cols = [("A.id",(TInt,false))];
  rows = [[VNull]];
};;

let test_projection_invalid_table =
  try
    let _ = projection proj_invalid [("A.id",(TInt,false))] in
    false
  with Failure msg ->
    assert (msg = "tbl n'est pas une table bien formée.");
    true
;;

(*
  DEBUT DES TESTS
*)

let test_projection_run =
  let _ = test_projection_one_field in
  let _ = test_projection_multiple_fields in
  let _ = test_projection_order in
  let _ = test_projection_all_fields in
  let _ = test_projection_unknown_field in
  let _ = test_projection_partial_invalid in
  let _ = test_projection_duplicates in
  let _ = test_projection_empty in
  let _ = test_projection_no_fields in
  let _ = assert test_projection_invalid_table in
  Printf.printf "Tous les tests sur [projection tbl fields] réussis.\n"
;;
