(** Le type [dbtype] défini les différents types possiblement présents
   en base dans notre cadre.

   Nous n'aurons ici besoin que de deux types primitifs.

   - les entiers représentés ici par TInt
   - les textes représentés ici par TText
*)
type dbtype =
  | TInt  (* type des entrées entières *)
  | TText (* type des entrées textes   *)
;;

(** Le type [coltype] est le type représentant un champ dans une table
   de notre système.

   Il est composé d'un couple comprenant le type des valeurs présentes
   dans ce champ d'une part et d'un booléen exprimant la possibilité
   (dans le cas où le booléen est à [true]) ou non pour ce champs
   d'adopter la valeur [NULL] *)
type coltype = dbtype * bool ;;

(** Le type [dbvalue] est le type des *VALEURS* présentes en base.

   Nous aurons besoin ici de trois types de valeurs.

   - les valeurs entières munies de leurs valeurs
   - les valeurs textuelles munies de leurs valeurs
   - la valeur null qui pourras être indifféremment considérée de type
   [TInt] et [TText].  *)
type dbvalue =
  | VInt of int     (* valeurs entières   *)
  | VText of string (* valeurs textuelles *)
  | VNull           (* la valeur NULL *)
;;

(** Le schéma d'une table est une liste de couple dont le premier
   élément est le nom du champs et le second est le type du champs de
   type [coltype] *)
type schema = (string*coltype) list ;;


(** Une ligne d'une table est une liste de valeurs.
*)
type row = dbvalue list ;;

(** Une [table] est la donnée d'un schéma et d'une liste de lignes *)
type table = { cols : schema; rows : row list } ;;

(** Le type [fd] représente le type des dépendances fonctionnelles
   d'une table.

   Il est composé d'un couple (lhs,rhs) dont chacun des deux membres
   est une liste de nom de champs.

   La dépendance (lhs,rhs) représente bien évidement la dépendance lhs -> rhs. 
*)
type fd = (string list) * (string list) ;;

(*

  ===================
  EXCEPTIONS
  ===================
  
 *)

exception Invalid_table
exception Invalid_row_size of int*int (* taille réelle, taille attendue*)
exception Invalid_row_type
exception Invalid_field (* sert dans la fonction projection *)
;;

(*
  
  ===================
  CHECK_TABLE TBL
  ===================
  
 *)

(** check_row_size : int -> row list -> bool -> bool
 *  @requires rien
 *  @param n nombre de champs
 *  @param l liste des lignes
 *  @param aux variable auxilière permettant la récursivité terminale
 *  @ensures retourne la condition "toutes les lignes de [l] sont de taille [n]"
 *  @raises aucune exception
 @use mettre aux à true pour retourner le bon résultat
 *)
let rec check_row_size n l aux = match l with
  | [] -> aux
  | t::q -> (*
              On regarde si la ligne t a les bonnes dimensions,
              Puis on fait un appel récursif sur le reste de la table
             *)
     if aux then check_row_size n q ((List.length t)=n)
     else false
;;

(** check_a_row_type : string*coltype list -> dbvalue list -> bool
 *  @requires rien
 *  @param cols liste de champs
 *  @param row ligne à tester
 *  @ensures retourne la condition "la ligne [row] correspond aux champs définis dans [cols]"
 *  @raises aucune exception
*)
let rec check_a_row_type cols row = match cols,row with
  | [],[] -> true
  | _,[] -> false (* Il manque des valeurs dans la ligne *)
  | [],_ -> false (* Il y a trop de valeurs dans la ligne *)
  | (_,
     (col_type, col_able_null)) :: qcols,
    ( VInt _ )::qrow -> 
     (* la valeur de la case est un entier *)
     if (col_type=TInt) then check_a_row_type qcols qrow
     else false (* le champ ne correspond pas *)
  | (_,
     (col_type, col_able_null)) :: qcols,
    ( VText _ )::qrow ->
     (* la valeur de la case est un texte *)
     if (col_type=TText) then check_a_row_type qcols qrow

     else false (* le champ ne correspond pas *)
  | (_,
     (col_type, col_able_null)) :: qcols,
    ( VNull )::qrow ->
     (* la valeur de la case est nulle,
        on regarde si les valeurs nulles sont autorisées *)
     col_able_null && (check_a_row_type qcols qrow)
;;

(** check_rows : ('a * (dbtype * bool)) list -> dbvalue list list -> bool -> bool
 *  @requires rien
 *  @param cols liste de champs
 *  @param l liste de lignes
 *  @param aux paramètre permettant la récursivité terminale 
 *  @ensures retourne la condition "toutes les lignes de [l] correspondent aux champs définis dans [cols]"
 *  @raises aucune exception
 @use mettre aux à true pour retourner le bon résultat
*)
let rec check_rows cols l aux = match l with
  | [] -> aux
  | t::q -> check_rows cols q (aux&&(check_a_row_type cols t)) 
;;

(** cols_to_strings : ('a * 'b) list -> 'a list -> 'a list
    @requires rien
    @param cols champs à récuperer le nom
    @param aux variable auxilière permettant la récursivité terminale
    @ensures retourne la liste des noms des champs de [cols] à l'envers
    @raises rien
    @use mettre aux à true pour retourner le bon résultat
 *)
let rec cols_to_strings cols aux = match cols with
  | [] -> aux
  | (name,_)::q -> cols_to_strings q (name::aux)
;;

(** check_cols : 'a list -> bool -> bool
    @param cols champs à vérifier
    @param aux variable permettant la récursivité terminale  
    @ensures retourne la condition "chaque colonne de [cols] admet un nom qui lui est propre"
    @raises rien
    @use mettre aux à true pour retourner le bon résultat
 *)
let rec check_cols cols aux = match cols with
  | [] -> aux
  | t::q -> check_cols q (aux && (not (List.mem t q)))
;;

(** check_table : table -> bool
 *  @requires rien
 *  @param tbl table à tester
 *  @ensures retourne la condition "[tbl] est une table valide"
 *  @raises aucune exception
*)
let check_table tbl =
  (check_row_size (List.length tbl.cols) (tbl.rows) true)
  &&
    (check_rows tbl.cols tbl.rows true)
  &&
    (check_cols (cols_to_strings tbl.cols []) true)
;;

(*

  ==================
  INSERT TBL ROW
  ==================
  
 *)

(** insert : table -> row -> table
 *  @requires rien
 *  @param tbl table où l'on doit insérer
 *  @param row ligne à insérer
 *  @ensures insère si possible [r] dans la table [tbl]
 *  @raises Invalid_table si [tbl] n'est pas une table bien formée,
 *    Invalid_row_size(actuel, attendu) si [row] n'a pas la bonne taille,
 *    Invalid_row_type si [row] n'admet pas le bon typage
*)
let insert tbl row =
  if check_table tbl then
    (* On vérifie si la ligne row est une ligne valable pour
       la table tbl. C'est à dire si elle a le bon nombre de
       colonnes, si elle a le bon typage et n'admet pas de valeurs
       nulles lorsque cela n'est pas autorisé.
    *)
    (* Premier test sur la taille de la ligne *)
    if (check_row_size (List.length tbl.cols) [row] true)
    then
      (* Deuxième test sur le typage *)
      if (check_a_row_type tbl.cols row) then
        {
          cols = tbl.cols;
          (* On ajoute la ligne dans la table si elle est non vide, car cela ne sert à rien de rajouter une ligne vide *)
          rows = if (row=[]) then tbl.rows
          else row::(tbl.rows)
        }
      else (* Le typage de la ligne est incorrect *)
        raise Invalid_row_type
    else (* Pas le bon nombre de valeurs dans la ligne *)
      raise (Invalid_row_size (List.length row,List.length tbl.cols))
  else
    raise Invalid_table
;;

(*

  ======================
  PROD TBL1 TBL2
  ======================
  
 *)


(** prod_row_table : 'a list -> 'a list list -> 'a list list -> 'a list list
    @requires rien
    @param row ligne à associer
    @param tb_rows table des lignes à associer
    @param aux variable auxilière permettant la récursivité terminale
    @ensures renvoie le produit carthésien entre la ligne [row] et la liste de lignes [tb_rows]
    @raises aucune exception
    @use mettre aux à [] pour retourner la bonne liste
 *)
let rec prod_row_table row tb_rows aux = match tb_rows with
  | [] -> aux
  | t::q -> prod_row_table row q ((row@t)::aux)
;;

(** prod_aux : dbvalue list list -> dbvalue list list -> dbvalue list list
    @requires rien
    @param tb1_rows première table du produit
    @param tb2_rows deuxième table du produit
    @param aux variable auxilière permettant la récursivité terminale
    @ensures renvoie la liste des lignes du produit carthésien entre [tb1_rows] et [tb2_rows]
    @raises aucune exception
    @use mettre aux à [] pour retourner la bonne liste
 *)
let rec prod_aux tb1_rows tb2_rows aux = match tb1_rows with
  |[] -> aux
  |t::q -> prod_aux q tb2_rows (prod_row_table t tb2_rows aux)
;;

(** (string * 'a) list -> (string * 'a) list -> string list -> (string * 'a) list
    @requires chaque nom de [cols1_namelist] doivent correspondre dans le même ordre aux noms des champs de [cols1]
    @param cols1 première liste de champs à concaténer
    @param cols2 deuxième liste de champs à concaténer
    @param cols1_namelist nom des champs de la première liste
    @ensures retourne la liste des champs de la table produit entre [cols1] et [cols2]
    @raises rien
 *)
let rec concat_prod cols1 cols2 cols1_namelist = match cols2 with
  | [] -> cols1
  | (name,ctype)::qcols ->
     if (List.mem name cols1_namelist) then
       (* on nomme le deuxième champ à rajouter en lui mettant "2" à la fin *)
       let new_name = name ^ "2" in
       (* on effectue un appel récursif avec les mêmes données car il faut vérifier que
          le nouveau nom ne correspond pas déjà à un nom
        *)
       concat_prod cols1 ((new_name,ctype)::qcols) cols1_namelist
     else (* aucune opération à faire sur le nom *)
       concat_prod (cols1 @ [name,ctype]) qcols (cols1_namelist@[name])

(** prod : table -> table -> table
    @requires rien
    @param tbl1 première table du produit
    @param tbl2 deuxième table du produit
    @ensures renvoie la table produit carthésien entre les tables [tbl1] et [tbl2]
    @raises Invalid_table si au moins une des tables [tbl1] et [tbl2] n'est pas une table bien formée
*)                   
let prod tbl1 tbl2 =
(*
  Une table produit carthésien admet comme définition de colonnes
  l'union des colonnes de tbl1 et de tbl2.
  Les lignes de cette table t sont de sorte que chaque ligne de
  tbl1 est suivie à chaque fois de chaque ligne de tbl2 dans t.
 *)
  if (check_table tbl1) then
    if (check_table tbl2) then
    (* On peut maintenant effectuer les opérations voulues pour le produit
       carthésien sans problèmes de dimensions.
      *)
      {
        cols = (concat_prod tbl1.cols tbl2.cols (cols_to_strings tbl1.cols []));
        (* Les lignes sont dans l'ordre inverse du produit carthésien
           habituel avec la fonction prod_aux puisque l'on ajoute les lignes
           obtenues avec la première ligne de tbl1 en premier, et se retrouve
         donc en fin de liste dans le produit.*)
        rows = List.rev (prod_aux tbl1.rows tbl2.rows [])
      }
    else
      raise Invalid_table
  else
    raise Invalid_table
;;

(*

  ======================
  PROJECTION TBL FIELDS
  ======================

 *)

(** projection_a_row_a_field : 'a list -> 'b list -> 'a -> 'a list * 'b list -> 'a list * 'b list
 *  @requires les types des valeurs de [row] correspondent bien à ceux des champs [cols]
 *  @param cols liste des champs de la ligne
 *  @param row ligne à traiter
 *  @param field champ à projeter
 *  @param aux variable auxilière pour la récursivité terminale
 *  @ensures retourne la projection du champ [field] sur la liste de champs [cols] selon la ligne [row] dans l'ordre inversé
 *  @raises Invalid_row_size (List.length row, List.length cols) si les deux longueurs ne sont pas égales
 @use mettre aux à ([],[]) pour retourner le bon résultat
*)
(*
  Attention, on considère que deux champs sont égaux s'ils ont le même nom. On suppose alors qu'il est impossible d'avoir deux champs de même nom dans une table.
 *)
let rec projection_a_row_a_field cols row field aux =
  let ncols = List.length cols in
  let nrow = List.length row in
  if (nrow=ncols) then
    match (cols,row) with
    | [],[] -> aux
    | tcol::qcol,tval::qval ->
       if (tcol=field) then
         (*
          * La colonne tcol correspond au champ field
          * On récupère les deux sous-listes auxilières
          *)
         let (selected_cols,selected_vals)=aux in
         (*
           Les listes ci dessous sont construites dans l'ordre inverse à cause de la récursivité et de l'ajout en tête
          *)
         (tcol::selected_cols,
          tval::selected_vals)
       else
         (*
           La colonne tcol ne correspond pas au champ field
          *)
         projection_a_row_a_field qcol qval field aux
    | [],_ -> failwith "cas impossible"
    | _,[] -> failwith "cas impossible"
  else raise (Invalid_row_size (nrow,ncols))
;;

(** projection_a_row : 'a list -> 'b list -> 'a list -> 'a list * 'b list
    @requires rien
    @param cols champs de la ligne à projeter
    @param row ligne à projeter
    @param fields champs de projection
    @ensures retourne la projection de la ligne row par les champs fields un à un suivant les champs cols.
    @raises Invalid_row_size (List.length row, List.length cols) si les deux longueurs ne sont pas égales,
      Invalid_row_type si la ligne [row] ne correspond pas aux champs [cols]
 *)
let rec projection_a_row cols row fields =
  (*
    Pour que les champs sélectionnés soient dans le bon ordre il faut utiliser
    un fold_right qui permettra de retourner la sélection via le dernier champ
    en dernier dans la liste.
   *)
  List.fold_right
    (fun field -> fun acc -> projection_a_row_a_field cols row field acc) 
      fields ([],[]) 
;;

(** pick_a_field : ('a * 'b) list -> 'a -> ('a * 'b) list
    @requires rien
    @param cols champs de recherche
    @param field nom du champ à rechercher
    @ensures retourne la liste des champs de [cols] ayant le nom [field]
    @raises rien
 *)
let pick_a_field cols field =
  List.fold_left (fun acc -> fun col -> match col with
                                        |(name,_) ->
                                          if (name=field) then col::acc
                                          else acc)
    [] cols
;;

  
(** pick_selected_fields : ('a * 'b) list -> 'a list -> ('a * 'b) list
    @requires rien
    @param cols champs de recherche
    @param fields noms des champs à rechercher
    @ensures retourne la liste des champs de [fields] présents dans [cols]
    @raises Invalid_table si un champ de [fields] apparaît plusieurs fois dans [cols],
      Invalid_field si un champ de [fields] n'appartient pas aux champs [cols]
 *)
let pick_selected_fields cols fields =
  List.fold_right
    (fun field -> fun acc -> let res = pick_a_field cols field in
                             match res with
                             | [] -> raise Invalid_field
                             | [(name,ctype)] -> concat_prod res acc [name]
                             | _ -> raise Invalid_table
    (* si cette table contient plus de deux valeurs
       alors il existe plus de deux champs au nom de field
       dans la liste de champs cols
       la table ayant les champs cols ne prend pas en compte
       les restrictions de nommage.*)
    )
    fields []
;;

(** projection : table -> string list -> table
    @requires rien
    @param tbl table de données
    @param fields liste des noms de champs à projeter
    @ensures retourne la projection des champs [fields] sur la table [tbl]
    @raises Invalid_table si la table [tbl] n'est pas bien formée,
      Invalid_field si un champ de [fields] n'appartient pas à la table [tbl]
 *)
let projection tbl fields =
  if check_table tbl then
    (*
      Alors la liste des champs est de la même taille que chaque ligne de la table.
*)
    let rec projection_each_row t f aux = match t.rows with
      | [] -> aux
      | trow::qrow ->
          let (selected_cols,selected_vals) = projection_a_row (List.rev (cols_to_strings t.cols [])) trow f in
          let (aux_cols,aux_rows) = aux in
          if (selected_vals=[]) then (* si la ligne en retour est vide, on ne la rajoute pas à la table projection *)
            projection_each_row
            ({cols=t.cols;
               (*
                 on effectue l'appel récursif sur le reste des lignes
                *)
              rows=qrow;})
            f (selected_cols, aux_rows)
          else
          projection_each_row
            ({cols=t.cols;
               (*
                 on effectue l'appel récursif sur le reste des lignes
                *)
              rows=qrow;})
            f (selected_cols, selected_vals::aux_rows)
    in
    let (_,rows) = projection_each_row tbl fields ([],[]) in
    {cols=pick_selected_fields tbl.cols fields;
     rows=List.rev rows}
  else raise Invalid_table
;;

(*

  ==================
  RESTRICT TBL TEST
  ==================

 *)

(** restrict : table -> (schema -> row -> bool) -> table
   [restrict tbl test] effectue la restriction des données présentes
   dans la table [tbl] en accord avec la fonction [test]. On ne garde
   dans le résultat que les lignes pour lesquelles [test] retourne
   [true].
 *)
let restrict tbl test =
  if check_table tbl then
    let rows = List.fold_right
                 (* on récupère chaque ligne passant les tests *)
                 (fun l -> fun acc -> if test tbl.cols l then l::acc else acc)
                 tbl.rows [] in
    {cols=tbl.cols; rows=rows}
  else raise Invalid_table
;;

(** test_operator_field_value_aux :
    @requires : [cols] représente les champs de la ligne [vals], [field] est un nom de champ dans cols
    @param cols champs de la table
    @param vals ligne à tester
    @param op opérateur
    @param field premier argument de l'opérateur [op] selon la liste [vals]
    @param value deuxième argument de l'opérateur [op]
    @ensures retourne le test de l'opérateur [op] sur le champ [field] de la ligne [vals] et la valeur [value]
    @raise rien
 *)
let test_operators_field_value_aux cols vals op field value =
  List.fold_left2 (
      fun acc ->
      fun (name,_) ->
      fun list_value ->
      if (field=name) then
        (* on applique l'opérateur op sur les dbvalues *)
        (fun v1 ->
          fun v2 ->
          (op) v1 v2)
          list_value value
      else acc
    ) true cols vals
;;

(** test_operator_field_field_aux :
    @requires : [cols] représente les champs de la ligne [vals], [field1] et [field2] sont des noms de champs de [cols]
    @param cols champs de la table
    @param vals ligne à tester
    @param op opérateur
    @param field1 premier argument de l'opérateur [op]
    @param field2 deuxième argument de l'opérateur [op]
    @ensures retourne le test de l'opérateur [op] sur le champ [field1] et le champ [field2] de la ligne vals
    @raise Invalid_comparison si les valeurs comparées par [op] ne sont pas de même dbtype
 *)
let test_operators_field_field_aux cols vals op field1 field2 =
  let find_value field =
    List.fold_left2 (
        fun acc ->
        fun (name, _) ->
        fun value ->
        if name = field then value else acc
      ) VNull cols vals
  in
  let v1 = find_value field1 in
  let v2 = find_value field2 in
  op v1 v2
;;

(*
 FONCTIONS DE TESTS POUR RESTRICT
 *)

(* fv pour field-value 
 *)
(** test_eq_fv : 'a -> 'b -> ('a * 'c) list -> 'b list -> bool
    @requires le champ [field] correspond au même type que la valeur [value], [cols] correspond aux champs de la ligne [vals]
    @param field nom du champ à filtrer
    @param value valeur à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "la valeur dans [vals] du champ [field] et [value] vérifient l'opérateur = "
    @raises rien
 *)
let test_eq_fv field value cols vals = test_operators_field_value_aux cols vals (=) field value;;
(** test_lt_fv : 'a -> 'b -> ('a * 'c) list -> 'b list -> bool
    @requires le champ [field] correspond au même type que la valeur [value], [cols] correspond aux champs de la ligne [vals]
    @param field nom du champ à filtrer
    @param value valeur à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "la valeur dans [vals] du champ [field] et [value] vérifient l'opérateur < "
    @raises rien
 *)
let test_lt_fv field value cols vals = test_operators_field_value_aux cols vals (<) field value;;
(** test_gt_fv : 'a -> 'b -> ('a * 'c) list -> 'b list -> bool
    @requires le champ [field] correspond au même type que la valeur [value], [cols] correspond aux champs de la ligne [vals]
    @param field nom du champ à filtrer
    @param value valeur à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "la valeur dans [vals] du champ [field] et [value] vérifient l'opérateur > "
    @raises rien
 *)
let test_gt_fv field value cols vals = test_operators_field_value_aux cols vals (>) field value;;
(** test_le_fv : 'a -> 'b -> ('a * 'c) list -> 'b list -> bool
    @requires le champ [field] correspond au même type que la valeur [value], [cols] correspond aux champs de la ligne [vals]
    @param field nom du champ à filtrer
    @param value valeur à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "la valeur dans [vals] du champ [field] et [value] vérifient l'opérateur <= "
    @raises rien
 *)
let test_le_fv field value cols vals = test_operators_field_value_aux cols vals (<=) field value;;
(** test_ge_fv : 'a -> 'b -> ('a * 'c) list -> 'b list -> bool
    @requires le champ [field] correspond au même type que la valeur [value], [cols] correspond aux champs de la ligne [vals]
    @param field nom du champ à filtrer
    @param value valeur à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "la valeur dans [vals] du champ [field] et [value] vérifient l'opérateur >= "
    @raises rien
 *)
let test_ge_fv field value cols vals = test_operators_field_value_aux cols vals (>=) field value;;

(* ff pour field-field
 *)
(** test_eq_ff : 'a -> 'a -> ('a * 'b) list -> dbvalue list -> bool
    @requires la valeur du champ [field1] correspond au même type que la valeur du champ [field2], [cols] correspond aux champs de la ligne [vals]
    @param field1 nom du premier champ à comparer
    @param field2 nom du deuxième champ à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "les valeurs dans [vals] du champ [field1] et [field2] vérifient l'opérateur = "
    @raises rien
 *)
let test_eq_ff field1 field2 cols vals = test_operators_field_field_aux cols vals (=) field1 field2;;
(** test_lt_ff : 'a -> 'a -> ('a * 'b) list -> dbvalue list -> bool
    @requires la valeur du champ [field1] correspond au même type que la valeur du champ [field2], [cols] correspond aux champs de la ligne [vals]
    @param field1 nom du premier champ à comparer
    @param field2 nom du deuxième champ à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "les valeurs dans [vals] du champ [field1] et [field2] vérifient l'opérateur < "
    @raises rien
 *)
let test_lt_ff field1 field2 cols vals = test_operators_field_field_aux cols vals (<) field1 field2;;
(** test_gt_ff : 'a -> 'a -> ('a * 'b) list -> dbvalue list -> bool
    @requires la valeur du champ [field1] correspond au même type que la valeur du champ [field2], [cols] correspond aux champs de la ligne [vals]
    @param field1 nom du premier champ à comparer
    @param field2 nom du deuxième champ à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "les valeurs dans [vals] du champ [field1] et [field2] vérifient l'opérateur > "
    @raises rien
 *)
let test_gt_ff field1 field2 cols vals = test_operators_field_field_aux cols vals (>) field1 field2;;
(** test_le_ff : 'a -> 'a -> ('a * 'b) list -> dbvalue list -> bool
    @requires la valeur du champ [field1] correspond au même type que la valeur du champ [field2], [cols] correspond aux champs de la ligne [vals]
    @param field1 nom du premier champ à comparer
    @param field2 nom du deuxième champ à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "les valeurs dans [vals] du champ [field1] et [field2] vérifient l'opérateur <= "
    @raises rien
 *)
let test_le_ff field1 field2 cols vals = test_operators_field_field_aux cols vals (<=) field1 field2;;
(** test_ge_ff : 'a -> 'a -> ('a * 'b) list -> dbvalue list -> bool
    @requires la valeur du champ [field1] correspond au même type que la valeur du champ [field2], [cols] correspond aux champs de la ligne [vals]
    @param field1 nom du premier champ à comparer
    @param field2 nom du deuxième champ à comparer
    @param cols champs de la ligne
    @param vals ligne à tester
    @ensures retourne la condition "les valeurs dans [vals] du champ [field1] et [field2] vérifient l'opérateur >= "
    @raises rien
 *)
let test_ge_ff field1 field2 cols vals = test_operators_field_field_aux cols vals (>=) field1 field2;;

(*

  =================
  COMPUTE_DEPS TBL
  =================
  
 *)

(** subsets : 'a list -> 'a list list
    @requires rien
    @param l ensemble de tous les éléments
    @ensures retourne la liste des sous ensembles de [l]
    @raises rien
 *)
let rec subsets l = match l with
  | [] -> [[]]
  | t::q ->
     let res = subsets q in
     res@(List.map (fun x -> t::x) res)
(*
  pour chaque élément t de l, on ajoute t à chaque sous-ensemble récursif
 *)
;;

(** list_all_deps : table -> (string list * string list) list
    @requires table bien formée
    @param tbl table à traiter
    @ensures renvoie la liste de toutes les dépendances fonctionnelles possibles de la table sans vérification
    @raises rien
 *) 
let rec list_all_deps tbl =
  let col_subsets = subsets (List.rev (cols_to_strings tbl.cols [])) in
  (* premier parcours pour lhs *)
  List.fold_left (fun lacc ->
      fun lhs ->
      (* lhs peut être [] *)
      match lhs with
      | [] -> lacc
      | _ ->
         (* deuxième parcours pour rhs *)
         List.fold_left (
             fun racc ->
             fun rhs ->
             (* rhs peut être [] *)
             match rhs with
             | [] -> racc
             | _ -> (* on ajoute (lhs,rhs) *)
                (lhs,rhs)::racc
           ) lacc col_subsets
    ) [] col_subsets
;;

(** equals_dbvalue : dbvalue -> dbvalue -> bool
    @requires
    @param v1 première valeur
    @param v2 deuxième valeur
    @ensures retourne la condition "[v1]=[v2]"
    @raises rien
 *)
let equals_dbvalue v1 v2 = match v1, v2 with
  | VNull, VNull -> true
  | VInt n1, VInt n2 -> n1=n2
  | VText t1, VText t2 -> t1=t2
  | _ -> false
;;

(** equals_dbvalue_list : dbvalue list -> dbvalue list -> bool
    @requires rien
    @param l1 première liste de valeurs
    @param l2 deuxième liste de valeurs
    @ensures retourne la condition "[l1]=[l2]"
    @raises rien
 *)
let equals_dbvalue_list l1 l2 =
  let rec f_aux li1 li2 aux =
    match li1,li2 with
    | [],[] -> aux
    | t1::q1, t2::q2 -> f_aux q1 q2 (aux&&(equals_dbvalue t1 t2))
    | _ -> false
  in f_aux l1 l2 true
;;


(** check_dep : string list * string list -> table -> bool
    @requires [dep] est une dépendance fonctionnelle possible de la table [tbl], [tbl] est une table bien formée
    @param dep dépendance fonctionnelle à tester
    @param tbl table de test
    @ensures retourne la condition "[dep] est une dépendance fonctionnelle de la table [tbl]"
    @raises rien
 *)

let check_dep dep tbl =
  (* on veut vérifier que pour toute ligne l1 et l2,
     si les valeurs du champ lhs sont égales alors les
     valeurs du champ rhs également
   *)
  let (lhs,rhs)=dep in
  (* premier parcours pour récupérer la première ligne *)
  List.fold_left
    (
      fun acc1 ->
      fun row1 ->
      
      (
        (* deuxième parcours pour la deuxième ligne *)
        List.fold_left (
            fun acc2 ->
            fun row2 ->
            let tbl_aux1 = {cols=tbl.cols; rows=[row1]} in
            (* on récupère lhs,rhs de la première ligne *)
            match (projection tbl_aux1 lhs).rows, (projection tbl_aux1 rhs).rows with
            | [lhs_v1], [rhs_v1]-> (
                let tbl_aux2 = {cols=tbl.cols; rows=[row2]} in
                (* on filtre lhs,rhs de la deuxième ligne *)
                match (projection tbl_aux2 lhs).rows, (projection tbl_aux2 rhs).rows with
                | [lhs_v2], [rhs_v2]->
                     (* test *)
                   if (equals_dbvalue_list lhs_v1 lhs_v2) then acc2&&(equals_dbvalue_list rhs_v1 rhs_v2)
                   else acc2
                | _ -> failwith "cas impossible")
            | _ -> failwith "cas impossible"
          ) acc1 tbl.rows
      )
    ) true tbl.rows
;;


(** compute_deps : table -> (string list * string list) list
    @requires rien
    @param tbl table à tester  
    @ensures retourne la liste de toutes les dépendances fonctionnelles trouvées de la table [tbl]
    @raises Invalid_table si la table [tbl] n'est pas bien formée
 *)
let compute_deps tbl =
  if (check_table tbl) then
    List.fold_left (
        fun acc -> fun dep ->
                   if (check_dep dep tbl) then dep::acc
                   else acc
      ) [] (list_all_deps tbl)
  else raise Invalid_table
;;

(*

  ===========================
  COMPUTE_ELEMENTARY_DEPS TBL
  ===========================
  
 *)

(** sub_elementary_dep : string list * string list -> table -> (string list * string list) list
    @requires [tbl] est une table bien formée, [dep] est une dépendance fonctionnelle possible de la table [tbl]
    @param dep dépendance fonctionnelle à tester
    @param tbl table de données à tester
    @ensures retourne la liste de toutes les sous-dépendances possibles de [dep]
    @raises rien
 *)
let sub_elementary_dep dep tbl =
  let (lhs,rhs) = dep in
  List.fold_left
    (
      fun acc ->
      fun lhs2 ->
      match lhs2 with
      | [] -> acc
      | _ ->
         if check_dep (lhs2,rhs) tbl then
           (lhs2,rhs)::acc
         else acc
    ) [] (subsets lhs)
;;

(** check_elementary_dep : table -> (string list * string list) list
    @requires [tbl] est une table bien formée, [dep] est une dépendance fonctionnelle possible de la table [tbl]
    @param dep dépendance fonctionnelle à tester
    @param tbl table de données à tester
    @ensures retourne la condition "[dep] est une dépendance élémentaire de [tbl]"
    @raises rien
 *)

let check_elementary_dep dep tbl =
  (*on vérifie que chaque sous ensemble strict n'est pas une dépendance fonctionnelle*)
  (match (sub_elementary_dep dep tbl) with
  | [] -> failwith "cas impossible"
  | [t] -> (t=dep) (* toujours true *)
  | _ -> false)
  &&
    (* on vérifie que rhs ne contient qu'un seul champ qui n'appartient pas à lhs *)
    (
      match dep with
      | lhs,[r] -> not (List.mem r lhs)
      | lhs, _ -> false
    )
;;

(** compute_elementary_deps : table -> (string list * string list) list
    @requires rien
    @param tbl table de données
    @ensures retourne la liste de toutes les dépendances fonctionnelles élémentaires de la table [tbl]
    @raises Invalid_table si la table [tbl] n'est pas bien formée
 *)
let compute_elementary_deps tbl =
  (* la validité de la table est testée dans compute_deps *)
  let deps = compute_deps tbl in
  List.fold_left
    (
      fun acc ->
      fun dep ->
      if (check_elementary_dep dep tbl) then
        dep::acc
      else acc
    ) [] deps
;;


(*

  =======================
  NORMALIZATION_LEVEL TBL
  =======================
  
 *)

let rec equals_list l1 l2 aux =
  match l1,l2 with
  | [],[] -> aux
  | t1::q1,t2::q2 -> equals_list q1 q2 (aux&&(t1=t2))
  | _ -> false
;;

(** check_key : string list -> table -> bool
    @requires [k] est une potentielle clé de la table [tbl], [tbl] est une table bien formée
    @param k clé potentielle à vérifier
    @param tbl table de valeurs à tester
    @ensures retourne la condition " [k] détermine les champs de [tbl] et aucun sous-ensemble strict de [k] ne détermine les champs de [tbl]"
    @raises rien

 *)
let check_key k tbl =
  let cols = cols_to_strings tbl.cols [] in

  (check_dep (k,cols) tbl)
  &&
    ((*
       parcours de chaque sous ensemble de la clé k
      *)
     List.fold_left (
         fun acc ->
         fun kp ->
         (*
           on vérifie pour chaque sous ensensemble STRICT de la clé k
           kp est bien une clé potentielle puisqu'elle est un sous-ensemble de la liste
           des champs de la table non réduit à la liste vide lors de l'appel à check_key
           grâce au principe d'évaluation paresseuse
          *)
         if ((not (equals_list k kp true))&&(kp!=[])) then
           acc&&(not (check_dep (kp,cols) tbl))
         else acc
       ) true (subsets k)
    )
;;

(** list_all_keys : table -> string list list
    @requires 
 *)
let list_all_keys tbl =
  let cols = cols_to_strings tbl.cols [] in
  List.fold_left (
      fun acc ->
      fun k ->
      (*
        k est bien une clé potentielle puisqu'elle est un sous-ensemble de la liste des
        champs de la table non réduit à la liste vide lors de l'appel à check_key grâce
        au principe d'évaluation paresseuse
       *)
      if ((k!=[])&&(check_key k tbl)) then k::acc
      else acc
    ) [] (subsets cols)
;;

(** check_nf2_key : string list -> table -> bool
    @requires [k] est une clé de la table [tbl], [tbl] est une table bien formée
    @param k clé à vérifier
    @param tbl table de valeurs à tester
    @ensures retourne la condition "tout champ de [tbl] n'appartenant pas à la clé [k] ne dépend pas d'un sous-ensemble strict de [k]"
    @raises rien
 *)
let check_nf2_key k tbl =
  let cols = cols_to_strings tbl.cols [] in
  (*
    parcours de chaque champ ai de la table
   *)
  List.fold_left (
      fun acc1 ->
      fun ai ->
      if  (not (List.mem ai k)) then

      (*
        parcours de chaque sous ensemble de la clé k
       *)
        List.fold_left (
            fun acc2 ->
            fun kp ->
            (*
              on vérifie pour chaque sous ensensemble STRICT de la clé k
             *)
            if (not (equals_list k kp true)) && (kp!=[]) then
              acc2&&(not (check_dep (kp,[ai]) tbl))
            else acc2
          ) acc1 (subsets k)
        
      else acc1
    ) true cols
;;

(** check_nf2 : string list list -> table -> bool
    @requires [all_keys] est la liste de toutes les clés de [tbl], [tbl] est une table bien formée
    @param all_keys liste des clés de [tbl]
    @param tbl table de valeurs à vérifier
    @ensures retourne la condition "[tbl] est en 2NF"
    @raises rien
 *)
let check_nf2 all_keys tbl =
  List.fold_left (
      fun acc ->
      fun k ->
      acc&&(check_nf2_key k tbl)
    ) true all_keys
;;

(** check_nf2 : string list list -> table -> bool
    @requires [all_keys] est la liste de toutes les clés de [tbl], [tbl] est une table bien formée
    @param all_keys liste des clés de [tbl]
    @param tbl table de valeurs à vérifier
    @ensures retourne la condition "[tbl] est en 3NF"
    @raises rien
 *)
let check_nf3 all_keys tbl =
  if check_nf2 all_keys tbl then 
    let elementary_deps = compute_elementary_deps tbl in
    (*
      parcours de chaque dépendance fonctionnelle élémentaire
     *)
    List.fold_left (
        fun acc ->
        fun e_dep ->
        (*on récupère la forme de e_dep*)
        match e_dep with
        | (x,[ai]) ->
           (*x est elle une clé*)
           if (check_key x tbl) then acc
        
           else (*ai appartient à une clé*)
             let ai_in_key =
               (
                 (*
                   parcours de chaque clé
                  *)
                 List.fold_left (
                     fun acc2 ->
                     fun k ->
                     (* ai appartient à k ou à une autre clé *)
                     acc2||(List.mem ai k)
                   ) false all_keys
               )
             in
             acc&&ai_in_key
        | _ -> failwith "cas impossible" (* car e_dep est une dépendance élémentaire *)
      ) true elementary_deps
  else false
;;

(** normalization_level : table -> int
    @requires rien
    @param tbl table à vérifier
    @ensures retourne le niveau de normalisation de [tbl] sous forme d'un entier entre 1 et 3
      @raises Invalid_table si [tbl] n'est pas une table bien formée
 *)
let normalization_level tbl =
  if (check_table tbl) then
    let all_keys = list_all_keys tbl in
    (*
      on vérifie d'abord que tbl est 3NF parce que si elle est 3NF, elle est 2NF
     *)
    if (check_nf3 all_keys tbl) then 3
    else if (check_nf2 all_keys tbl) then 2
    (*
      on suppose que tbl est déjà en 1NF grâce à nos restrictions de valeurs de la table
     *)
    else 1
  else raise Invalid_table
;;

Printf.printf "Début des tests... \n";;

(*
  ============================
  Tables tests de check_table
  ============================
 *)

(* Table bien formée : correct *)
let tbl = {
    cols = [
      ("nom",(TText,false));
      ("hp",(TInt,true));
      ("pp",(TInt,true));
    ];
      rows=
      [
        [VText "Lucas"; VInt 60; VInt 100];
        [VText "Claus"; VInt 90; VInt 25];
        [VText "Ness"; VInt 100; VInt 100];
        [VText"MR.Saturn"; VNull; VNull]
      ]
  }
;;
(* Table sans instances : correct *)
let no_instance_tbl = {
    cols = [
      ("nom",(TText,false));
      ("hp",(TInt,true));
      ("pp",(TInt,true));
    ];
      rows=[]
  }
;;
(* Table sans champs : correct *)
let no_field_tbl = {
    cols=[];
    rows=[]
  }
;;
(* Type VText au lieu de VInt : invalide *)
let invalid_tbl_1 = {
    cols = [("hp",(TInt,true))];
    rows = [[VText "Ness"]];
  }
;;
(* Type VInt au lieu de VText : invalide *)
let invalid_tbl_2 = {
    cols = [("name",(TText,false))];
    rows = [[VInt 100]];
  }
;;
(* VNull sans autorisation : invalide *)
let invalid_tbl_3 = {
    cols = [("name",(TText,false))];
    rows = [[VNull]];
  }
;;
(* Deux champs de même nom : invalide *)
let invalid_tbl_4 = {
    cols = [
      ("pp",(TInt,true));
      ("pp",(TInt,true));
    ];
      rows=[[VInt 100; VInt 100]]
  }
;;
(* Taille de la ligne trop grande : invalide *)
let invalid_tbl_5 = {
    cols = [
      ("hp",(TInt,true));
      ("pp",(TInt,true));
    ];
      rows=[[VInt 100; VInt 100; VText "Ness"]]
  }
;;
(* Taille de la ligne trop petite : invalide *)
let invalid_tbl_5 = {
    cols = [
      ("hp",(TInt,true));
      ("pp",(TInt,true));
    ];
    rows=[[VInt 100]]
  }
;;

(*
  Rappel du nom des tables :
  tbl
  no_instance_tbl
  no_field_tbl
  invalid_tbl_1
  invalid_tbl_2
  invalid_tbl_3
  invalid_tbl_4
  invalid_tbl_5
 *)

let test_check_table =
  let _ = assert (check_table tbl = true) in
  let _ = assert (check_table no_instance_tbl = true) in
  let _ = assert (check_table no_field_tbl = true) in
  let _ = assert (check_table invalid_tbl_1 = false) in
  let _ = assert (check_table invalid_tbl_2 = false) in
  let _ = assert (check_table invalid_tbl_3 = false) in
  let _ = assert (check_table invalid_tbl_4 = false) in
  let _ = assert (check_table invalid_tbl_5 = false) in
  Printf.printf "check_table : ok \n"
;;

(*
  =======================
  Lignes tests de insert
  =======================
 *)

(* ligne valide avec présence de VNull : valide *)
let tbl_valid_row = [VText "Kumatora"; VNull; VInt 200];;
(* nombre de valeurs plus petite que le nombre de champs : invalide *)
let invalid_row_1 = [VText "Kumatora"; VNull];;
(* nombre de valeurs plus grande que le nombre de champs : invalide*)
let invalid_row_2 = [VText "Kumatora"; VNull; VInt 200; VInt 200];;
(* VText au lieu de VInt : invalide *)
let invalid_row_3 = [VText "Kumatora"; VNull; VText "Kumatora"];;
(* VInt au lieu de VText : invalide *)
let invalid_row_4 = [VInt 200; VNull; VInt 200];;
(* VNull alors que le champ ne l'authorise pas : invalide*)
let invalid_row_5 = [VNull; VNull; VInt 200];;

let test_insert =
  
  (* 1. tests sur la validité de la table *)
  (* 1.1 aboutissement *)
  let _ = assert (insert tbl tbl_valid_row = {
                cols = [
                  ("nom",(TText,false));
                  ("hp",(TInt,true));
                  ("pp",(TInt,true));];
                rows=
                  [
                    tbl_valid_row;
                    [VText "Lucas"; VInt 60; VInt 100];
                    [VText "Claus"; VInt 90; VInt 25];
                    [VText "Ness"; VInt 100; VInt 100];
                    [VText"MR.Saturn"; VNull; VNull]
                  ]
            }) in
  let _ = assert (insert no_instance_tbl tbl_valid_row = {
                cols = [
                  ("nom",(TText,false));
                  ("hp",(TInt,true));
                  ("pp",(TInt,true));];
                rows=
                  [
                    tbl_valid_row;
                  ]
            }) in
  let _ = assert (insert no_field_tbl [] = {
                cols = [];
                rows = []
            }) in
  (* 1.2 test des cas limites *)
  (* tbl_valid_row ne sera pas une ligne valide pour les prochaines tables
     mais nous voulons vérifier que la validité de la table est bien vérifiée
     avant tout *)
  let _ = try ( let _ = insert invalid_tbl_1 tbl_valid_row in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = insert invalid_tbl_2 tbl_valid_row in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = insert invalid_tbl_3 tbl_valid_row in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = insert invalid_tbl_4 tbl_valid_row in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = insert invalid_tbl_5 tbl_valid_row in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  
  (* 2. Tests sur la table valide tbl et sur les lignes *)
  let _ = try ( let _ = insert tbl invalid_row_1 in failwith "cas impossible") with | Invalid_row_size (2,3) -> Printf.printf "" in
  let _ = try ( let _ = insert tbl invalid_row_2 in failwith "cas impossible") with | Invalid_row_size (4,3) -> Printf.printf "" in
  let _ = try ( let _ = insert tbl invalid_row_3 in failwith "cas impossible") with | Invalid_row_type -> Printf.printf "" in
  let _ = try ( let _ = insert tbl invalid_row_4 in failwith "cas impossible") with | Invalid_row_type -> Printf.printf "" in
  let _ = try ( let _ = insert tbl invalid_row_5 in failwith "cas impossible") with | Invalid_row_type -> Printf.printf "" in
  Printf.printf "insert : ok \n"
;;

(*
  Rappel du nom des tables :
  tbl
  no_instance_tbl
  no_field_tbl
  invalid_tbl_1
  invalid_tbl_2
  invalid_tbl_3
  invalid_tbl_4
  invalid_tbl_5
 *)

(*
  ================
  Tests pour prod
  ================
 *)

let tbl2 = {
    cols = [
      ("nom",(TText,false));
      ("arme",(TText,true));
    ];
    rows = [
        [VText "Lucas"; VText "baton"];
        [VText "MR.Saturn"; VNull];
        [VText "Claus"; VText "épée"];
        [VText "Ness"; VText "batte"];
      ]
  }

let prod_tbl1 = prod tbl tbl;;
let prod_tbl2 = prod tbl tbl2;;

let _ =
  
(* Première table invalide *)
  let _ = try ( let _ = prod invalid_tbl_1 tbl in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod invalid_tbl_2 tbl in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod invalid_tbl_3 tbl in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod invalid_tbl_4 tbl in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod invalid_tbl_5 tbl in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  
(* Deuxième table invalide *)
  let _ = try ( let _ = prod tbl invalid_tbl_1 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod tbl invalid_tbl_2 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod tbl invalid_tbl_3 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod tbl invalid_tbl_4 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = prod tbl invalid_tbl_5 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in

  let _ = assert ( prod_tbl1 = {
                cols = [
                  ("nom",(TText,false));
                  ("hp",(TInt,true));
                  ("pp",(TInt,true));
                  ("nom2",(TText,false));
                  ("hp2",(TInt,true));
                  ("pp2",(TInt,true))
                ];
                rows=
                  [
                    [VText "Lucas"; VInt 60; VInt 100; VText "Lucas"; VInt 60; VInt 100];
                    [VText "Lucas"; VInt 60; VInt 100; VText "Claus"; VInt 90; VInt 25];
                    [VText "Lucas"; VInt 60; VInt 100; VText "Ness"; VInt 100; VInt 100];
                    [VText "Lucas"; VInt 60; VInt 100; VText"MR.Saturn"; VNull; VNull];
                    
                    [VText "Claus"; VInt 90; VInt 25; VText "Lucas"; VInt 60; VInt 100];
                    [VText "Claus"; VInt 90; VInt 25; VText "Claus"; VInt 90; VInt 25];
                    [VText "Claus"; VInt 90; VInt 25; VText "Ness"; VInt 100; VInt 100];
                    [VText "Claus"; VInt 90; VInt 25; VText "MR.Saturn"; VNull; VNull];
                    
                    [VText "Ness"; VInt 100; VInt 100; VText "Lucas"; VInt 60; VInt 100];
                    [VText "Ness"; VInt 100; VInt 100; VText "Claus"; VInt 90; VInt 25];
                    [VText "Ness"; VInt 100; VInt 100; VText "Ness"; VInt 100; VInt 100];
                    [VText "Ness"; VInt 100; VInt 100; VText "MR.Saturn"; VNull; VNull];
                    
                    [VText"MR.Saturn"; VNull; VNull; VText "Lucas"; VInt 60; VInt 100];
                    [VText"MR.Saturn"; VNull; VNull; VText "Claus"; VInt 90; VInt 25];
                    [VText"MR.Saturn"; VNull; VNull; VText "Ness"; VInt 100; VInt 100];
                    [VText"MR.Saturn"; VNull; VNull; VText "MR.Saturn"; VNull; VNull]
                    
                  ]
            }
            ) in
  let _ = assert ( prod tbl no_instance_tbl = {
                cols = [
                  ("nom",(TText,false));
                  ("hp",(TInt,true));
                  ("pp",(TInt,true));
                  ("nom2",(TText,false));
                  ("hp2",(TInt,true));
                  ("pp2",(TInt,true))
                ];
                rows=[]
            }) in
  let _ = assert ( prod tbl no_field_tbl = {
                cols = [
                  ("nom",(TText,false));
                  ("hp",(TInt,true));
                  ("pp",(TInt,true))
                ];
                rows=[]
            }) in
  let _ = assert ( prod_tbl2 = {
    cols = [
      ("nom",(TText,false));
      ("hp",(TInt,true));
      ("pp",(TInt,true));
      ("nom2",(TText,false));
      ("arme",(TText,true));
    ];
      rows=
      [
        [VText "Lucas"; VInt 60; VInt 100; VText "Lucas"; VText "baton"];
        [VText "Lucas"; VInt 60; VInt 100; VText "MR.Saturn"; VNull];
        [VText "Lucas"; VInt 60; VInt 100; VText "Claus"; VText "épée"];
        [VText "Lucas"; VInt 60; VInt 100; VText "Ness"; VText "batte"];
        
        [VText "Claus"; VInt 90; VInt 25; VText "Lucas"; VText "baton"];
        [VText "Claus"; VInt 90; VInt 25; VText "MR.Saturn"; VNull];
        [VText "Claus"; VInt 90; VInt 25; VText "Claus"; VText "épée"];
        [VText "Claus"; VInt 90; VInt 25; VText "Ness"; VText "batte"];
        
        [VText "Ness"; VInt 100; VInt 100; VText "Lucas"; VText "baton"];
        [VText "Ness"; VInt 100; VInt 100; VText "MR.Saturn"; VNull];
        [VText "Ness"; VInt 100; VInt 100; VText "Claus"; VText "épée"];
        [VText "Ness"; VInt 100; VInt 100; VText "Ness"; VText "batte"];
        
        [VText"MR.Saturn"; VNull; VNull; VText "Lucas"; VText "baton"];
        [VText"MR.Saturn"; VNull; VNull; VText "MR.Saturn"; VNull];
        [VText"MR.Saturn"; VNull; VNull; VText "Claus"; VText "épée"];
        [VText"MR.Saturn"; VNull; VNull; VText "Ness"; VText "batte"];
        
      ]
  }) in
  Printf.printf "prod : ok \n"
;;


(*
  =====================
  Tests pour projection
  =====================
 *)

let _ =

  (* test avec des tables invalides *)
  let _ = try ( let _ = projection invalid_tbl_1 [] in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = projection invalid_tbl_2 [] in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = projection invalid_tbl_3 [] in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = projection invalid_tbl_4 [] in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = projection invalid_tbl_5 [] in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in

  (* tests sur différentes tables de la projection vide *)
  let _ = assert (projection tbl [] = {
                cols = [];
                rows = []
            }) in
  let _ = assert (projection no_instance_tbl [] = {
                cols = [];
                rows = []
            }) in
  let _ = assert (projection no_field_tbl [] = {
                cols = [];
                rows = []
            }) in

  (* tests sur la table tbl de plusieurs projections *)

  (* projections du même champ plusieurs fois *)
  let _ = assert ( projection tbl ["nom";"nom";"nom"] = {
                cols =
                  [("nom", (TText, false));
                   ("nom2", (TText, false));
                   ("nom22", (TText, false))];
                rows =
                  [[VText "Lucas"; VText "Lucas"; VText "Lucas"];
                   [VText "Claus"; VText "Claus"; VText "Claus"];
                   [VText "Ness"; VText "Ness"; VText "Ness"];
                   [VText "MR.Saturn"; VText "MR.Saturn"; VText "MR.Saturn"]]}) in
  let _ = assert ( projection tbl ["nom"; "hp"; "nom";"nom"] = {
                cols =
                  [("nom", (TText, false));
                   ("hp", (TInt, true));
                   ("nom2", (TText, false));
                   ("nom22", (TText, false))];
                rows =
                  [[VText "Lucas"; VInt 60; VText "Lucas"; VText "Lucas"];
                   [VText "Claus"; VInt 90; VText "Claus"; VText "Claus"];
                   [VText "Ness"; VInt 100; VText "Ness"; VText "Ness"];
                   [VText "MR.Saturn"; VNull; VText "MR.Saturn"; VText "MR.Saturn"]]}) in

  (* projections de champs inconnus *)
  let _ = try ( let _ = projection tbl ["armure"] in failwith "cas impossible") with | Invalid_field -> Printf.printf "" in
                   
  Printf.printf "projection : ok \n"
;;


(*
  ================
  test restrict
  ================
 *)

let tbl_restrict = {
    cols = [
      ("nom", (TText, false));
      ("hp", (TInt, true));
      ("pp", (TInt, true));
      ("arme", (TText, true))
    ];
    rows = [
      [VText "Lucas"; VInt 60; VInt 100; VText "épée"];
      [VText "Claus"; VInt 90; VInt 25; VText "arc"];
      [VText "Ness"; VInt 100; VInt 100; VText "batte"];
      [VText "Paula"; VInt 75; VInt 50; VText "bâton"];
      [VText "Jeff"; VInt 80; VInt 75; VText "pistolet"]
    ]
};;

let test_restrict =
  (* hp > 80 *)
  let r1 = restrict tbl_restrict (test_gt_fv "hp" (VInt 80)) in
  let _ = assert (r1.rows =
          [
            [VText "Claus"; VInt 90; VInt 25; VText "arc"];
            [VText "Ness"; VInt 100; VInt 100; VText "batte"]
          ]) in

  (* arme = "épée" *)
  let r2 = restrict tbl_restrict (test_eq_fv "arme" (VText "épée")) in
  let _ = assert (r2.rows =
          [
            [VText "Lucas"; VInt 60; VInt 100; VText "épée"]
          ]) in

  (* hp = pp *)
  let r3 = restrict tbl_restrict (test_eq_ff "hp" "pp") in
  let _ = assert (r3.rows =
          [
            [VText "Ness"; VInt 100; VInt 100; VText "batte"]
          ]) in

  (* pp <= 50 *)
  let r4 = restrict tbl_restrict (test_le_fv "pp" (VInt 50)) in
  let _ = assert (r4.rows =
          [
            [VText "Claus"; VInt 90; VInt 25; VText "arc"];
            [VText "Paula"; VInt 75; VInt 50; VText "bâton"]
          ]) in

  (* Toujours vrai *)
  let r5 = restrict tbl_restrict (fun _ _ -> true) in
  let _ = assert (r5.rows = tbl_restrict.rows) in

  (* Toujours faux *)
  let r6 = restrict tbl_restrict (fun _ _ -> false) in
  let _ = assert (r6.rows = []) in

  (* Table vide *)
  let empty_tbl = {cols = tbl_restrict.cols; rows = []} in
  let r7 = restrict empty_tbl (test_gt_fv "hp" (VInt 80)) in
  let _ = assert (r7.rows = []) in

  (* hp >= pp *)
  let r8 = restrict tbl_restrict (test_ge_ff "hp" "pp") in
  let _ = assert (r8.rows =
          [
            [VText "Claus"; VInt 90; VInt 25; VText "arc"];
            [VText "Ness"; VInt 100; VInt 100; VText "batte"];
            [VText "Paula"; VInt 75; VInt 50; VText "bâton"];
            [VText "Jeff"; VInt 80; VInt 75; VText "pistolet"]
          ]) in

  Printf.printf "restrict : ok \n"



(*
  =================
  test compute_deps
  =================
 *)

(*
  les dépendances fonctionnelles évidentes sont :
  nom,hp -> nom
  nom,hp -> hp
  nom,hp -> nom,hp

  nom -> nom

  hp -> hp

  les autres dépendances sont :
  nom -> hp
  donc
  nom -> nom, hp
 *)
let deps_tbl1 = {
    cols = [("nom",(TText, false));
            (("hp",(TInt, true)))];
    rows = [
        [VText "Ness"; VInt 100];
        [VText "Lucas"; VInt 50];
        [VText "Autre"; VInt 50]
      ]
  }
;;

(*
  les dépendances fonctionnelles évidentes sont :
  hp,pp -> hp
  hp,pp -> pp
  hp,pp -> hp,pp
  hp -> hp
  pp -> pp
 *)
let deps_tbl2 = {
    cols = [("hp",(TInt, true));
            (("pp",(TInt, true)))];
    rows = [
        [VInt 100; VInt 50];
        [VInt 100; VInt 25];
        [VNull; VInt 25]
      ]
  }
;;

let _ =
  
  (* tests sur des tables invalides *)
  let _ = try ( let _ = compute_deps invalid_tbl_1 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_deps invalid_tbl_2 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_deps invalid_tbl_3 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_deps invalid_tbl_4 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_deps invalid_tbl_5 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in

  (* tests sur des tables valides *)
  let res1 = compute_deps deps_tbl1 in
  let _ = assert (
              (List.mem (["nom";"hp"],["nom"]) res1)
              && (List.mem (["nom";"hp"],["hp"]) res1)
              && (List.mem (["nom";"hp"],["nom";"hp"]) res1)
              
              && (List.mem (["nom"],["nom"]) res1)
              &&(List.mem (["hp"],["hp"]) res1)
              
              &&(List.mem (["nom"],["hp"]) res1)
              &&(List.mem (["nom"],["nom";"hp"]) res1)

              &&(List.length res1 = 7)
            ) in
  let res2 = compute_deps deps_tbl2 in
  let _ = assert (
              (List.mem (["hp";"pp"],["hp"]) res2)
              && (List.mem (["hp";"pp"],["pp"]) res2)
              && (List.mem (["hp";"pp"],["hp";"pp"]) res2)
              
              && (List.mem (["hp"],["hp"]) res2)
              &&(List.mem (["pp"],["pp"]) res2)

              &&(List.length res2 = 5)
            ) in
  (*
    dépendances fonctionnelles de tbl2:
    évidentes :
    nom, arme -> nom,arme
    nom,arme -> nom
    nom,arme -> arme
    nom->nom
    arme-> arme

    moins évidentes :
    nom -> arme
    arme -> arme
    cela implique :
    nom -> nom,arme
    arme -> nom,arme
   *)
  let res3 = compute_deps tbl2 in
  let _ = assert (
              (List.mem (["nom"; "arme"], ["nom"; "arme"]) res3)
              &&(List.mem (["nom"; "arme"], ["nom"]) res3)
              &&(List.mem (["nom"; "arme"], ["arme"]) res3)
              &&(List.mem (["nom"], ["nom"]) res3)
              &&(List.mem (["arme"], ["arme"]) res3)
              
              &&(List.mem (["nom"], ["arme"]) res3)
              &&(List.mem (["arme"], ["nom"]) res3)
              
              &&(List.mem (["nom"], ["nom"; "arme"]) res3)
              &&(List.mem (["arme"], ["nom"; "arme"]) res3)
              &&(List.length res3 = 9)
            ) in
  Printf.printf "compute_deps : ok \n"
;;

(*
  ============================
  test compute_elementary_deps
  ============================
 *)

let _ =
  
  (* tests sur des tables invalides *)
  let _ = try ( let _ = compute_elementary_deps invalid_tbl_1 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_elementary_deps invalid_tbl_2 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_elementary_deps invalid_tbl_3 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_elementary_deps invalid_tbl_4 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in
  let _ = try ( let _ = compute_elementary_deps invalid_tbl_5 in failwith "cas impossible") with | Invalid_table -> Printf.printf "" in

  (* tests sur des tables valides *)

  (*
    dépendances fonctionnelles élémentaires de deps_tbl2:
    évidentes :
    nom -> hp
   *)
  let res1 = compute_elementary_deps deps_tbl1 in
  let _ = assert (
              (List.mem (["nom"],["hp"]) res1)
              &&(List.length res1 = 1)
            ) in
  (*
    aucune dépendance fonctionnelle élémentaire
   *)
  let res2 = compute_elementary_deps deps_tbl2 in
  let _ = assert (res2=[]) in
  (*
    dépendances fonctionnelles élémentaires de tbl2:
    nom -> arme
    arme -> arme
   *)
  let res3 = compute_elementary_deps tbl2 in
  let _ = assert (
               (List.mem (["nom"], ["arme"]) res3)
              &&(List.mem (["arme"], ["nom"]) res3)
              &&(List.length res3 = 2)
            ) in
  Printf.printf "compute_elementary_deps : ok \n"
;;

(*
  ============================
  Tests pour normalization_level
  ============================
 *)

(* Table en 1NF seulement *)
let tbl_1nf = {
    cols = [
      ("nom", (TText, false));
      ("profession", (TText, false));
      ("salaire", (TInt, true))
    ];
    rows = [
        [VText "A"; VText "prof"; VInt 12];
        [VText "B"; VText "prof"; VInt 12];
        [VText "A"; VText "agent"; VInt 13];
        [VText "A"; VText "nettoyage"; VInt 13];
    ]
}
;;

(* Table en 2NF mais pas en 3NF *)
let tbl_2nf = {
    cols = [
      ("id_ordonnance",(TInt,false));
      ("n_secu",(TInt,false));
      ("nom",(TText,false));
    ];
    rows = [
      [VInt 1; VInt 50; VText "A";];
      [VInt 2; VInt 30; VText "B";];
      [VInt 3; VInt 50; VText "A";];
    ]
}
;;

(* Table en 3NF *)
let tbl_3nf = {
    cols = [
      ("nom", (TText, false));
      ("niveau", (TInt, true));
      ("arme", (TText, true));
    ];
    rows = [
      [VText "Lucas"; VInt 10; VText "épée"];
      [VText "Claus"; VInt 15; VText "épée"];
      [VText "Ness"; VInt 20; VText "batte"];
    ]
  }
;;

let test_normalization_level =

  (* Test des tables incorrectes *)
  let _ = try (let _ = normalization_level invalid_tbl_1 in failwith "cas impossible")
          with Invalid_table -> Printf.printf "" in
  let _ = try (let _ = normalization_level invalid_tbl_2 in failwith "cas impossible")
          with Invalid_table -> Printf.printf "" in
  let _ = try (let _ = normalization_level invalid_tbl_3 in failwith "cas impossible")
          with Invalid_table -> Printf.printf "" in
  let _ = try (let _ = normalization_level invalid_tbl_4 in failwith "cas impossible")
          with Invalid_table -> Printf.printf "" in
  let _ = try (let _ = normalization_level invalid_tbl_5 in failwith "cas impossible")
           with Invalid_table -> Printf.printf "" in
  
  (* Table en 1NF seulement *)
  let _ = assert ((normalization_level tbl_1nf) = 1) in

  (* Table en 2NF mais pas en 3NF *)
  let _ = assert ((normalization_level tbl_2nf) = 2) in

  (*Table en 3NF *)
  let _ = assert ((normalization_level tbl_3nf) = 3) in

  (* Table vide *)
  let _ = assert ((normalization_level no_instance_tbl) = 3) in

  Printf.printf "normalization_level : ok \n"
;;
