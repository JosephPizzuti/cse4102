open Printf

let rec fold (f : 'a -> 'b -> 'b) (l : 'a list) (u : 'b) : 'b =
  match l with
  | [] -> u
  | h::t -> f h (fold f t u)

let sum l = fold (+) l 0
let product l = fold ( * ) l 1
let concat l = fold (@) l []
let sum_doubles l = fold (fun elem the_rest -> elem * 2 + the_rest) l 0

let reverse l = fold (fun elem the_rest -> the_rest @ [elem]) l []

let rec print_int_list l =
  match l with
  | [] -> ()
  | h::t ->
      printf "%d " h;
      print_int_list t

let rec only_even l =
  match l with
  | [] -> []
  | h::t -> if h mod 2 = 0 then h::(only_even t) else only_even t

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> if f h then h::(filter f t) else (filter f t)

let only_nonempty =
  filter (fun l -> match l with [] -> false | _ -> true)  


let () =
  printf "fold (+) [1,2,3] 0 = %d\n" (fold (+) [1;2;3] 0);
  printf "sum of doubles in [1,2,3] = %d\n" (sum_doubles [1;2;3]);
  
  printf "reverse([1,2,3]) = [ ";
  print_int_list (reverse [1;2;3]);
  printf "]\n";

  printf "only_even([1,2,3,4,5,6,7]) = [ ";
  print_int_list (only_even [1;2;3;4;5;6;7]);
  printf "]\n";
