(*
  TESTS EXHAUSTIFS SUR LA FONCTION PROD
*)

(* Tables de base valides *)
let tbl1 = {
  cols = ("A.id",(TInt,true))::("A.name",(TText,true))::[];
  rows = [[VInt 1; VText "Alice"]; [VInt 2; VText "Bob"]];
};;

let tbl2 = {
  cols = ("B.score",(TInt,true))::[];
  rows = [[VInt 10]; [VInt 20]];
};;

(* 1. Produit cartésien normal *)
let test_prod_valid =
  let t = prod tbl1 tbl2 in
  assert (t.cols = tbl1.cols @ tbl2.cols);
  assert (List.length t.rows = List.length tbl1.rows * List.length tbl2.rows);
  assert (t.rows = [
    [VInt 1; VText "Alice"; VInt 10];
    [VInt 1; VText "Alice"; VInt 20];
    [VInt 2; VText "Bob"; VInt 10];
    [VInt 2; VText "Bob"; VInt 20];
  ]);
  t
;;

(* 2. Une table vide *)
let tbl_empty_rows = { cols = tbl2.cols; rows = [] };;

let test_prod_empty_tbl2 =
  let t = prod tbl1 tbl_empty_rows in
  assert (t.cols = tbl1.cols @ tbl_empty_rows.cols);
  assert (t.rows = []);  (* pas de lignes car tbl2 vide *)
  t
;;

(* 3. Deux tables vides *)
let tbl_empty = { cols = []; rows = [] };;

let test_prod_both_empty =
  let t = prod tbl_empty tbl_empty in
  assert (t.cols = []);
  assert (t.rows = []);
  t
;;

(* 4. Table invalide : tbl1 invalide *)
let tbl1_invalid = { cols = [("A.id",(TInt,false))]; rows = [[VNull]] };;

let test_prod_tbl1_invalid =
  try
    let _ = prod tbl1_invalid tbl2 in
    false
  with Failure msg ->
    assert (msg = "tbl1 n'est pas une table bien formée.");
    true
;;

(* 5. Table invalide : tbl2 invalide *)
let tbl2_invalid = { cols = [("B.score",(TInt,false))]; rows = [[VNull]] };;

let test_prod_tbl2_invalid =
  try
    let _ = prod tbl1 tbl2_invalid in
    false
  with Failure msg ->
    assert (msg = "tbl2 n'est pas une table bien formée.");
    true
;;

(*
  DEBUT DES TESTS
*)

let test_prod_run =
  let _ = test_prod_valid in
  let _ = test_prod_empty_tbl2 in
  let _ = test_prod_both_empty in
  let _ = assert test_prod_tbl1_invalid in
  let _ = assert test_prod_tbl2_invalid in
  Printf.printf "Tous les tests exhaustifs sur [prod tbl1 tbl2] réussis.\n"
;;
