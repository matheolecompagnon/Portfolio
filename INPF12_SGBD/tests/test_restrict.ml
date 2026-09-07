(* ===================== *)
(* OUTILS                *)
(* ===================== *)

let table_eq t1 t2 =
  (t1.cols = t2.cols) && (t1.rows = t2.rows)
;;

(* ===================== *)
(* DONNÉES               *)
(* ===================== *)

let cols_ex =
  [ ("id",(TInt,false));
    ("name",(TText,false));
    ("age",(TInt,true)) ]
;;

let table_ex =
  { cols = cols_ex;
    rows =
      [ [VInt 1; VText "Alice"; VInt 20];
        [VInt 2; VText "Bob"; VNull];
        [VInt 3; VText "Charlie"; VInt 30] ] }
;;

(* ===================== *)
(* TESTS FONCTIONNELS    *)
(* ===================== *)

let test_filter_age =
  let f _ row =
    match row with
    | [_; _; VInt age] -> age > 20
    | _ -> false
  in
  let expected =
    { cols = cols_ex;
      rows = [ [VInt 3; VText "Charlie"; VInt 30] ] }
  in
  table_eq (restrict table_ex f) expected
;;

let test_none =
  let f _ _ = false in
  (restrict table_ex f).rows = []
;;

let test_all =
  let f _ _ = true in
  (restrict table_ex f).rows = table_ex.rows
;;

let test_null =
  let f _ row =
    match row with
    | [_; _; VNull] -> true
    | _ -> false
  in
  let expected =
    { cols = cols_ex;
      rows = [ [VInt 2; VText "Bob"; VNull] ] }
  in
  table_eq (restrict table_ex f) expected
;;

let test_empty =
  let tbl = { cols = cols_ex; rows = [] } in
  (restrict tbl (fun _ _ -> true)).rows = []
;;

let test_cols_used =
  let f cols row =
    match (cols, row) with
    | (("id",_)::_), (VInt id :: _) -> id = 1
    | _ -> false
  in
  let expected =
    { cols = cols_ex;
      rows = [ [VInt 1; VText "Alice"; VInt 20] ] }
  in
  table_eq (restrict table_ex f) expected
;;

(* ===================== *)
(* PROPRIÉTÉS (B)        *)
(* ===================== *)

let test_schema_preserved =
  (restrict table_ex (fun _ _ -> true)).cols = table_ex.cols
;;

let test_subset_property =
  let f _ row =
    match row with
    | [_; _; VInt age] -> age > 20
    | _ -> false
  in
  let res = restrict table_ex f in
  List.for_all (fun r -> List.mem r table_ex.rows) res.rows
;;

(* ===================== *)
(* ORDRE (C)             *)
(* ===================== *)

let test_order_preserved =
  let f _ row =
    match row with
    | [VInt id; _; _] -> id <> 2
    | _ -> false
  in
  let res = restrict table_ex f in
  res.rows = List.filter (fun r -> f table_ex.cols r) table_ex.rows
;;

(* ===================== *)
(* PRÉDICATS (D)         *)
(* ===================== *)

let test_multi_column_predicate =
  let f _ row =
    match row with
    | [VInt id; VText name; VInt age] ->
        id + age > 25 && name <> "Bob"
    | _ -> false
  in
  let res = restrict table_ex f in
  List.for_all (fun row -> f table_ex.cols row) res.rows
;;

let test_partial_predicate =
  let f _ row =
    match row with
    | [_; _; VInt _] -> true
    | _ -> false
  in
  let res = restrict table_ex f in
  List.for_all (f table_ex.cols) res.rows
;;

(* ===================== *)
(* CAS LIMITES (E)       *)
(* ===================== *)

let test_single_row =
  let tbl =
    { cols = cols_ex;
      rows = [ [VInt 1; VText "A"; VInt 10] ] }
  in
  (restrict tbl (fun _ _ -> true)).rows = tbl.rows
;;

let test_one_column =
  let cols = [("id",(TInt,false))] in
  let tbl =
    { cols;
      rows = [ [VInt 1]; [VInt 2] ] }
  in
  let f _ row =
    match row with
    | [VInt x] -> x = 1
    | _ -> false
  in
  (restrict tbl f).rows = [ [VInt 1] ]
;;

let test_minimal_table =
  let cols = [("x",(TInt,false))] in
  let tbl = { cols; rows = [ [VInt 42] ] } in
  (restrict tbl (fun _ _ -> true)).rows = [ [VInt 42] ]
;;

(* ===================== *)
(* AGRÉGATION            *)
(* ===================== *)

let tests_restrict =
  [
    test_filter_age;
    test_none;
    test_all;
    test_null;
    test_empty;
    test_cols_used;

    test_schema_preserved;
    test_subset_property;

    test_order_preserved;

    test_multi_column_predicate;
    test_partial_predicate;

    test_single_row;
    test_one_column;
    test_minimal_table;
  ]
;;

let all_tests_pass =
  List.for_all (fun x -> x) tests_restrict
;;
