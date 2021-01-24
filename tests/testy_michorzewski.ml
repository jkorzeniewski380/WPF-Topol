(* Autor:
 * Licence: GNU GPL v3 
 * Original repo: https://github.com/Nhemisirmkow/ *)

(* Testy te są własnością Marcina Michorzewskiego i sprawdzają poprawność na małych testach *)
let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

open Topol;;

let isOk input output =
  let rec where a = function
    | [] -> []
    | h::t -> if h = a then h::t else (where a t) in 
  let rec length used = function
    | [] -> List.length used
    | (h, l1)::t -> let newOne used a = 
                      if (where a used) = [] then a::used else used
      in length (newOne (List.fold_left newOne used l1) h) t in
  let size = length [] input in
  let rec find acc wh = function
    | [] -> acc
    | h::t -> acc && ((where h wh) <> []) && (find acc wh t) in
  let rec pom acc = function
    | [] -> acc
    | (a, l)::t -> acc && find acc (where a output) l && pom acc t
  in (size = List.length output) && (pom true input);;

let a = [(3, [4]); (5, [6]); (6, [7]); (10, [11]); (11, [12]); (12, [13]); (7, [8]); (9, [13]); (8, [9]); (1, [2]); (13, [6])];;
let a = try topol a with Cykliczne -> [];;

test 1 (a = []);;

let b = [(11, [12]); (12, [13]); (7, [8]); (8, [9]); (1, [2]); (13, [6]); (3, [4]); (5, [6]); (6, [7]); (10, [11])];; (* Niecykliczne *)
let c = [];;

test 2 (isOk b (topol b));;
test 3 (isOk c (topol c));;

let a = [("Polska", ["Niemcy"]); ("Niemcy", ["Polska"])];; (* Cykliczne *)
let a = try topol a with Cykliczne -> [];;

test 4 (a = []);;

let a = [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3])];; (* Niecykliczne *)

test 5 (isOk a (topol a));;

let a = [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (2, [5])];; (* Cykliczne *)
let b = [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (1, [5])];; (* Cykliczne *)
let c = [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (2, [6])];; (* Cykliczne *)
let d = [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (1, [6])];; (* Cykliczne *)
let e = [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14])];; (* Niecykliczne *)

let a = try topol a with Cykliczne -> [];;
let b = try topol b with Cykliczne -> [];;
let c = try topol c with Cykliczne -> [];;
let d = try topol d with Cykliczne -> [];;

test 6 (a = []);;
test 7 (b = []);;
test 8 (c = []);;
test 9 (d = []);;
test 10 (isOk e (topol e));;

let cyclic = [
  (1, [2]);
  (2, [3]);
  (3, [1])];;
let cyclic = try topol cyclic with Cykliczne -> [];;

test 11 (cyclic = []);;

let l1 = [
  (1, [2]);
  (2, []);
  (3, [2])];;

let l2 = [
  ('a', ['e']);
  ('b', ['a'; 'c']);
  ('c', ['a']);
  ('e', [])];;
test 12 (isOk l1 (topol l1));;

test 13 (isOk l2 (topol l2));;


let a = [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8])];; (* Niecykliczne *)
let b = [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8]); (8, [14])];; (* Niecykliczne *)
let c = [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8]); (8, [12])];; (* Cykliczne *)

let c = try topol c with Cykliczne -> [];;

test 14 (isOk a (topol a));;
test 15 (isOk b (topol b));;
test 16 (c = []);;


