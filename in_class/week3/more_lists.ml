open Printf

let rec sum l =
  match l with
  | [] -> 0
  | h::t -> h + (sum t)

let rec inc_fst (l : (int * int) list ) =
  match l with
  | [] -> []
  | (x,y)::t -> (x+1,y)::(inc_fst t)

let print_int_list l =
  List.iter (fun x -> printf "%d " x) l;
  printf "\n"



let () =
  (*inc_fst [(1,2);(3,4);(5,6)]*);
  printf "sum of [1,2,3,4,5] is %d\n" (sum [1;2;3;4;5]);
