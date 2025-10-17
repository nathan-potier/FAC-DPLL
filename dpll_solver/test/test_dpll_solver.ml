open Dpll_solver.Dpll

let exemple_3_12 = [[1;2;-3];[2;3];[-1;-2;3];[-1;-3];[1;-2]]
let exemple_7_2 = [[1;-1;-3];[-2;3];[-2]]
let exemple_7_4 = [[1;2;3];[-1;2;3];[3];[1;-2;-3];[-1;-2;-3];[-3]]
let exemple_7_8 = [[1;-2;3];[1;-3];[2;3];[1;-2]]
let systeme = [[-1;2];[1;-2];[1;-3];[1;2;3];[-1;-2]]

let coloriage = [
  [1;2;3];[4;5;6];[7;8;9];[10;11;12];[13;14;15];[16;17;18];
  [19;20;21];[-1;-2];[-1;-3];[-2;-3];[-4;-5];[-4;-6];[-5;-6];
  [-7;-8];[-7;-9];[-8;-9];[-10;-11];[-10;-12];[-11;-12];[-13;-14];
  [-13;-15];[-14;-15];[-16;-17];[-16;-18];[-17;-18];[-19;-20];
  [-19;-21];[-20;-21];[-1;-4];[-2;-5];[-3;-6];[-1;-7];[-2;-8];
  [-3;-9];[-4;-7];[-5;-8];[-6;-9];[-4;-10];[-5;-11];[-6;-12];
  [-7;-10];[-8;-11];[-9;-12];[-7;-13];[-8;-14];[-9;-15];[-7;-16];
  [-8;-17];[-9;-18];[-10;-13];[-11;-14];[-12;-15];[-13;-16];
  [-14;-17];[-15;-18]]



let%test "simplifie supprime clauses satisfaites" =
  simplifie 1 [[1;2]; [-1;3]; [2;3]] = [[3]; [2;3]]

let%test "simplifie supprime -l de clauses restantes" =
  simplifie 2 [[1;2]; [-2;3]; [2;3]] = [[1]; [3]; [3]]

let%test "simplifie rien à supprimer" =
  simplifie 4 [[1;2]; [3;5]] = [[1;2]; [3;5]]

let%test "unitaire vide" =
  unitaire [] = None

let%test "unitaire simple" =
  unitaire [[1]; [2;3]] = Some 1

let%test "unitaire pas de clause unitaire" =
  unitaire [[1;2]; [-1;3]] = None

let%test "pur simple" =
  pur [[1;2]; [-1;3]] = Some 2  (* 2 est pur, -1 et 1 s'annulent *)

let%test "pur avec plusieurs littéraux purs" =
  pur [[4;5]; [6]] = Some 4      (* n'importe lequel des purs, retourne le premier rencontré *)

let%test "pur aucun littéral pur" =
  pur [[1; -1]; [2; -2]] = None

let%test "solveur simple SAT" =
  solveur_dpll [[1]; [-1;2]] = Sat [1;2]  (* ou Sat [-1;2] selon ordre de parcours *)

let%test "solveur simple UNSAT" =
  solveur_dpll [[1]; [-1]] = Unsat

let%test "solveur SAT avec clause unitaire" =
  solveur_dpll [[1]; [2;3]] = Sat [1;2;3]

let%test "solveur SAT avec pur" =
  solveur_dpll [[1;2]; [3]] = Sat [3;1;2]

let%test "solveur plus complexe SAT" =
  solveur_dpll [[1;2]; [-1;2]; [-2;3]] = Sat [1;2;3]  (* exemple simple avec propagation unitaire *)


let () = print_string "Test 1:\n"
let () = print_modele (solveur_split (cnf_of_int_list_list systeme))
let () = print_string "Test 2:\n"
let () = print_modele (solveur_dpll (cnf_of_int_list_list systeme))
let () = print_string "Test 3:\n"
let () = print_modele (solveur_split (cnf_of_int_list_list coloriage))
let () = print_string "Test 4:\n"
let () = print_modele (solveur_dpll (cnf_of_int_list_list coloriage))
