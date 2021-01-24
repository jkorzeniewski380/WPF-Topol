(* ======= Autor: Jakub Korzeniewski ======= *)
(* ======== Sortowanie topologiczne ======== *)
(* ===== Code Review: Weronika Tkaczyk ===== *)

(* == Testy: https://gitlab.com/MIMUW-wpf/testy-topol/-/tree/master/tests == *)

open PMap

(* ========= Funkcje pomocnicze =========== *)

(** funkcja zwracająca graf przedstawiony jako mapa sąsiedztwa;
    dla każdego wierzchołka grafu przechowujemy listę jego sąsiadów;
    W liście input każda para (k, v) oznacza, że z każdego
    wierzchołka z listy v mamy krawędź skierowaną do wierzchołka k; 
    ('a * 'a list) list -> ('a, 'a list) PMap.t *)
let graph input = 
  let n_list = List.fold_left (fun m (k, v) -> 
      List.fold_left (fun a x -> 
          if not (mem x a) then add x [k] a
          else add x (k :: (find x a)) a) m v) empty input in
  List.fold_left (fun m (k, v) ->
      if not (mem k m) then add k [] m else m) n_list input;;

(** funkcja zwracająca listę sąsiadów wierzchołka [x] w grafie [g]
    ('a, 'b list) PMap.t -> 'a -> 'b list *)
let neighbours g x =
  try find x g with Not_found -> [];;

(* ==== Koniec Funkcji pomocniczych ======= *)

(* =============== Wyjątki ================ *)


(** wyjątek rzucany przez topol gdy zależności są cykliczne *)
exception Cykliczne;;


(* =========== Koniec Wyjątków ============ *)

(** funkcja, która dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il 
    ('a * 'a list) list -> 'a list*)
let topol (input : ('a * 'a list) list) : 'a list =
  let g = graph input in

  (** funkcja sprawdzająca, czy zależności są cykliczne;
      Jeśli są, rzuca wyjątek Cykliczne, w innym przypdku
      zwraca wynik sortowania topologicznego
      ('a * 'a list) list -> 'a list -> 'a list *)
  let isCyclic input output = 
    let (pos, _) = List.fold_left (fun (a, i) x -> 
        (add x i a, i+1)) (empty, 0) output in
    let isInOrder a b =
      try 
        if (find a pos) < (find b pos) then true
        else false
      with Not_found -> false in 
    List.iter (fun (k, v) ->
        List.iter (fun x -> 
            if isInOrder k x = false then raise Cykliczne) v) input;
    output in

  let rec topologicalUtil g v visited stack =
    visited := add v true !visited;
    List.iter (fun x -> if (find x !visited) = false then
                  topologicalUtil g x visited stack) (neighbours g v);
    Stack.push v !stack in

  (** funkcja zwracająca odwróconą listę będącą wynikiem
      sortowania topologicznego grafu g
      ('a, 'a list) PMap.t -> 'a  list *)
  let topological g = 
    let visited = ref empty in
    iter (fun k _ -> visited := (add k false !visited)) g;

    let stack = ref (Stack.create ()) in

    iter (fun k _ -> if (find k !visited) = false then topologicalUtil g k visited stack) !visited;

    let res = ref [] in
    while not (Stack.is_empty !stack) do
      res := (Stack.pop !stack) :: !res
    done;
    !res
  in

  (* potencjalny wynik - przed sprawdzeniem czy zależności są cykliczne *)
  let output = topological g in

  isCyclic input output;;


