

(** Ce fichier contient les fonctions à compléter. *)

(** Fonctions utilitaires. *)

(** filter_map : ('a -> 'b option) -> 'a list -> 'b list
   disponible depuis la version 4.08.0 de OCaml dans le module List :
   pour chaque élément de `list', appliquer `filter' :
   - si le résultat est `Some e', ajouter `e' au résultat ;
   - si le résultat est `None', ne rien y ajouter.
   Attention, cette implémentation inverse l'ordre de la liste *)
let filter_map filter list =
  let rec aux list ret =
    match list with
    | []   -> ret
    | h::t -> match (filter h) with
      | None   -> aux t ret
      | Some e -> aux t (e::ret)
  in aux list []

(* Ceci est un exemple de test unitaire pour la fonction filter_map. *)
let%test "filter_map" =
  (filter_map (fun x -> if x mod 2 = 0 then Some (x * 2) else None) [1; 2; 3; 4]) = [8; 4]

(* Voici d'autres exemples de tests unitaires dont vous pouvez vous inspirer pour tester vos fonctions. *)
let rec big_list n acc =
  if n <= 0 then acc else big_list (n-1) (n::acc)

(* On teste d'abord sur une petite liste.*)
let %test "length vide" = (List.length [] = 0)
let %test "length 1" = (List.length [1] = 1)
let %test "length 3" = (List.length [1;2;3] = 3)
(* Puis sur une grande liste pour vérifier qu'il n'y a pas de débordement de pile. *)
let %test "length big" = let big = 10000000 in (List.length (big_list big []) = big)






(** Voici une proposition de types pour une implémentation simple.
    Vous pouvez éventuellement changer les types dans un deuxième temps. *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type resultat = Sat of interpretation | Unsat

(** Avec ce choix de type cette fonction est l'identité. *)
let cnf_of_int_list_list (l: int list list) : cnf = l

(** print_modele : resultat -> unit afficher le résultat *)
let print_modele: resultat -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let modele_trie = List.sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") modele_trie;
     print_string "0\n"

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let rec simplifie l = function
  | [] -> []
  | c :: rest ->
      if List.exists ((=) l) c then
        simplifie l rest  (* Clause satisfaite, on la supprime *)
      else
        let rec filter_l acc = function
          | [] -> List.rev acc
          | x :: xs ->
              if x = -l then filter_l acc xs
              else filter_l (x :: acc) xs
        in
        (filter_l [] c) :: simplifie l rest
;;

(** solveur_split_rec : cnf -> interpretation -> resultat
   exemple d'utilisation de `simplifie' cette fonction ne doit pas être modifiée, sauf si vous changez 
   le type de la fonction simplifie *)
let rec solveur_split_rec clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = [] then Sat interpretation else
  (* la clause vide n'est jamais satisfiable *)
  if List.mem [] clauses then Unsat else
  (* branchement *) 
  let l = List.hd (List.hd clauses) in
  let branche = solveur_split_rec (simplifie l clauses) (l::interpretation) in
  match branche with
  | Unsat -> solveur_split_rec (simplifie (-l) clauses) ((-l)::interpretation)
  | _    -> branche

let solveur_split clauses = solveur_split_rec clauses []

(** Solveur dpll récursif *)

(** pur : cnf -> literal option
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon renvoie None *)
let pur clauses =
  (* Aplatir et trier par valeur absolue *)
  let lits = List.sort (fun a b -> compare (abs a) (abs b)) (List.flatten clauses) in
  let rec aux = function
    | [] -> None
    | x :: xs ->
        let rec fold_group (current_abs, seen_neg, last_lit) = function
          | [] -> if not seen_neg then Some last_lit else None
          | y :: ys ->
              if abs y = current_abs then
                fold_group (current_abs, seen_neg || y = -last_lit, last_lit) ys
              else
                if not seen_neg then Some last_lit
                else aux (y :: ys)
        in
        fold_group (abs x, false, x) xs
  in
  aux lits
;;


(** unitaire : cnf -> literal option
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon renvoie None *)
let rec unitaire = function
  | [] -> None
  | clause :: rest ->
      (match clause with
       | [] -> unitaire rest          (* Clause vide : pas unitaire *)
       | [lit] -> Some lit            (* Clause unitaire trouvée *)
       | _ -> unitaire rest)          (* Clause non unitaire *)
;;

(** solveur_dpll_rec : cnf -> interpretation -> resultat 
    Étapes :
        1. Si la CNF est vide -> toutes les clauses sont satisfaites -> SAT.
        2. Si une clause vide est présente -> contradiction -> UNSAT.
        3. Sinon, on simlifie :
          a) On regarde si une clause unitaire existe
          b) On simplifie les littéraux purs
        4. Si aucune simplification possible, on choisit un littéral et on tente :
          - d’abord `lit` vrai
          - sinon `lit` faux*)
let rec solveur_dpll_rec clauses interpretation =
  match clauses with
  | [] -> Sat interpretation (* Etape 1 : toutes les clauses ont été satisfaites -> solution trouvée. *)
  | c :: _ when c = [] -> Unsat (* Etape 2 : une clause vide existe -> contradiction -> formule insatisfiable. *)
  | _ -> 
    (
      match unitaire clauses with
      | Some l -> 
          (* Etape 3a : une clause unitaire existe -> on doit lui attribue une valeur. *)
          let clauses' = simplifie l clauses in
          solveur_dpll_rec clauses' (l :: interpretation)  

      | None ->
        (
          match pur clauses with
          | Some l -> 
            (* Cas 3b : un littéral pur existe -> on le met à vrai. *)
            let clauses' = simplifie l clauses in 
            solveur_dpll_rec clauses' (l :: interpretation)
          
          | None -> 
            (
              (* Cas 4 : aucun littéral pur ou unitaire -> on split (on met d'abord le litteral à vraie puis à faux). *)
              let l = List.hd (List.hd clauses) in
              (* On essaie d’abord la branche où l = vrai. *)
              let clauses_true = simplifie l clauses in 
              match solveur_dpll_rec clauses_true (l :: interpretation) with
              (* Si satisfiable, on la retourne. *)
              | Sat i -> Sat i
              | Unsat ->
                (* Sinon, on explore la branche où l = faux. *)
                let clauses_false = simplifie (-l) clauses in 
                solveur_dpll_rec clauses_false ((-l) :: interpretation)
            )
        )
    )
;;

let solveur_dpll clauses = solveur_dpll_rec clauses []

(* --- Tests simplifie --- *)
let%test "simplifie supprime clauses satisfaites" =
  simplifie 1 [[1;2]; [-1;3]; [2;3]] = [[3]; [2;3]]

let%test "simplifie supprime -l de clauses restantes" =
  simplifie (-2) [[1;2]; [-2;3]; [2;3]] = [[1]; [3]]

let%test "simplifie rien à supprimer" =
  simplifie 4 [[1;2]; [3;5]] = [[1;2]; [3;5]]

(* --- Tests unitaire --- *)
let%test "unitaire vide" =
  unitaire [] = None

let%test "unitaire simple" =
  unitaire [[1]; [2;3]] = Some 1

let%test "unitaire pas de clause unitaire" =
  unitaire [[1;2]; [-1;3]] = None

(* --- Tests pur --- *)
let%test "pur simple" =
  pur [[1;2]; [-1;3]] = Some 2

let%test "pur avec plusieurs littéraux purs" =
  let r = pur [[4;5]; [6]] in
  r = Some 4 || r = Some 5 || r = Some 6

let%test "pur aucun littéral pur" =
  pur [[1; -1]; [2; -2]] = None

(* --- Tests solveur_dpll --- *)
let%test "solveur simple SAT" =
  match solveur_dpll [[1]; [-1;2]] with
  | Sat i -> i = [1;2] || i = [2;1]
  | Unsat -> false

let%test "solveur simple UNSAT" =
  solveur_dpll [[1]; [-1]] = Unsat


let%test "solveur SAT avec clause unitaire" =
  match solveur_dpll [[1]; [2;3]] with
  | Sat i -> i = [2;1]  
  | Unsat -> false
;;



let%test "solveur SAT avec pur" =
  match solveur_dpll [[1;2]; [3]] with
  | Sat i -> i = [1;3]  
  | Unsat -> false

let%test "solveur plus complexe SAT" =
  match solveur_dpll [[1;2]; [-1;2]; [-2;3]] with
  | Sat i -> i = [2;3]  
  | Unsat -> false
