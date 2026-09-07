
(*
  TESTS SUR LE TYPAGE
 *)

(* test sur les valeurs VNull quand bool=false
   n'est pas une table : VNull interdit *)
let test_null_false = {
    cols=
      ("A.id",(TInt,false))::[];
    rows= 
      (
        (VNull)::[]
      )::[]
  };;

(* test sur les valeurs VNull quand bool=true
   est une table *)
let test_null_true = {
    cols=
      ("A.id",(TInt,true))::[];
    rows= 
      (
        (VNull)::[]
      )::[]
  };;

(* test sur le typage VInt au lieu de VText
   n'est pas une table *)
let test_vint_replace_vtext = {
    cols=
      ("A.name",(TText,false))::[];
    rows= 
      (
        (VInt 3)::[]
      )::[]
  };;

(* test sur le typage VText au lieu de VInt
   n'est pas une table *)
let test_vtext_replace_vint = {
    cols=
      ("A.id",(TInt,false))::[];
    rows= 
      (
        (VText "Q")::[]
      )::[]
  };;

(* test sur plusieurs erreurs de typage
   n'est pas une table *)
let test_type_error = {
    cols=
      ("A.id",(TInt,false))::
        ("A.name",(TText,false))::[];
    rows= 
      (
        (VText "Q")::(VInt 2)::[]
      )::[]
  };;

(* test sur plusieurs erreurs de VNull
   n'est pas une table *)
let test_null_errors = {
    cols=
      ("A.id",(TInt,false))::
        ("A.name",(TText,false))::[];
    rows= 
      (
        (VNull)::(VNull)::[]
      )::[]
  };;

(* test sur plusieurs erreurs générales : Typage et valeurs nulles
   t5 n'est pas une table *)
let test_errors = {
    cols=
      ("A.id",(TInt,false))::
        ("A.name",(TText,false))::[];
    rows= 
      (
        (VText "E")::(VNull)::[]
      )::[]
  };;

(* test sur une table contenant toutes les limites acceptables
   est une table *)
let test_acceptable_table = {
    cols=
      ("A.id",(TInt,true))::
        ("A.name",(TText,true))::[];
    rows= 
      (
        (VNull)::(VText "A")::[]
      )::[]
  };;

(*
  TESTS SUR LES RESTRICTIONS DE TAILLE DE LA TABLE
 *)

(* test sur la taille d'une ligne plus grande que le nombre de colonnes*)
let test_colsize_higher = {
    cols=
      ("A.id",(TInt,false))::[];
    rows= 
      (
        (VInt 1)::(VInt 2)::[]
      )::[]
  };;

(* test sur la taille d'une ligne plus petite que le nombre de colonnes*)
let test_colsize_lower = {
    cols=
      ("A.id",(TInt,false))::
        ("A.name",(TText,false))::[];
    rows= 
      (
        (VInt 1)::[]
      )::[]
  };;

(* test sur une table vide *)
let test_empty = {
    cols=
      ("A.id",(TInt,false))::[];
    rows= []
  };;

(* test sur une table sans definition de colonnes *)
let test_no_column_table = {
    cols=[];
    rows= 
      (
        (VInt 1)::[]
      )::[]
  };;

(*
  DEBUT DES TESTS
 *)

let test_table_run =
  let _ =
    assert ((check_table test_null_false)=false) in
  let _ =
    assert ((check_table test_null_true)) in
  let _ =
    assert ((check_table test_vint_replace_vtext)=false) in
  let _ =
    assert ((check_table test_vtext_replace_vint)=false) in
  let _ =
    assert ((check_table test_type_error)=false) in
  let _ =
    assert ((check_table test_null_errors)=false) in
  let _ =
    assert ((check_table test_errors)=false) in
  let _ =
    assert ((check_table test_acceptable_table)) in
  let _ =
    assert ((check_table test_colsize_higher)=false) in
  let _ =
    assert ((check_table test_colsize_lower)=false) in
  let _ =
    assert ((check_table test_empty)) in
  let _ =
    assert ((check_table test_no_column_table)=false) in
  Printf.printf "Tests sur [check_table tbl] réussis.\n" 
;;
