(*
  TESTS SUR LA FONCTION INSERT AVEC ASSERTIONS
*)

(* Table de base correcte *)
let base_table = {
  cols = ("A.id",(TInt,true))::("A.name",(TText,true))::[];
  rows = [];
};;

(* 1. insertion valide *)
let test_insert_valid =
  let tbl = insert base_table [VInt 1; VText "Alice"] in
  assert ((List.length tbl.rows) = 1);
  assert (tbl.rows = [[VInt 1; VText "Alice"]]);
  tbl
;;

(* 2. insertion avec ligne trop courte *)
let test_insert_short_row =
  try
    let _ = insert base_table [VInt 2] in
    false
  with Failure msg ->
    assert (msg = "row n'admet pas le bon nombre de valeurs.");
    true
;;

(* 3. insertion avec ligne trop longue *)
let test_insert_long_row =
  try
    let _ = insert base_table [VInt 3; VText "Bob"; VText "Extra"] in
    false
  with Failure msg ->
    assert (msg = "row n'admet pas le bon nombre de valeurs.");
    true
;;

(* 4. insertion avec mauvais typage *)
let test_insert_wrong_type =
  try
    let _ = insert base_table [VText "Q"; VText "Charlie"] in
    false
  with Failure msg ->
    assert (msg = "row n'admet pas le bon typage.");
    true
;;

(* 5. insertion avec VNull interdit *)
let table_no_null = {
  cols = ("A.id",(TInt,false))::("A.name",(TText,false))::[];
  rows = [];
};;

let test_insert_null_disallowed =
  try
    let _ = insert table_no_null [VInt 4; VNull] in
    false
  with Failure msg ->
    assert (msg = "row n'admet pas le bon typage.");
    true
;;

(* 6. insertion dans table invalide *)
let invalid_table = {
  cols = ("A.id",(TInt,false))::[];
  rows = [
    [VNull]
  ];
};;

let test_insert_invalid_table =
  try
    let _ = insert invalid_table [VInt 5] in
    false
  with Failure msg ->
    assert (msg = "tbl n'est pas une table bien formée.");
    true
;;

(*
  DEBUT DES TESTS
*)

let test_insert_run =
  let _ = test_insert_valid in
  let _ = assert test_insert_short_row in
  let _ = assert test_insert_long_row in
  let _ = assert test_insert_wrong_type in
  let _ = assert test_insert_null_disallowed in
  let _ = assert test_insert_invalid_table in
  Printf.printf "Tous les tests sur [insert tbl row] réussis.\n"
;;
