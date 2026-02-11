open Printf

let five = 5
let plusone = (fun x -> x + 1)

let f = fun y -> plusone

let apply_twice (f : 'a -> 'a) x =
  f (f x)

let compose (f : 'a -> 'a) (g : 'b -> 'c) x =
  g (f x)

let plus_five_then_triple = compose (( + ) 5) (( * ) 3)

let rec sq_all l =
  match l with
  | [] -> []
  | h::t -> (h * h) :: (sq_all t)

let rec capitalize_all l =
  match l with
  | [] -> []
  | h::t -> (String.capitalize_ascii h) :: (capitalize_all t)

let rec print_str_list l =
  match l with
  | [] -> printf "\n";
  | h::t ->
      printf "%s " h;
      print_str_list t

let rec product l =
  match l with
  | [] -> 1
  | h::t -> h * (product t)

let one_to_five = [1;2;3;4;5]

let rec concat (ll : 'a list list) : 'a list =
  match ll with
  | [] -> []
  | h::t -> h @ (concat t)


let () =
  printf "5 + 1 + 1 is %d\n" (apply_twice plusone 5);
  printf "40 / 2 / 2 is %d\n" (apply_twice (fun x -> x / 2) 40);
  printf "(5 + 1) * 2 is %d\n" (compose ((+) 1) (( * ) 2) 5);
  printf "(10 + 5) * 3 is %d\n" (plus_five_then_triple 10);
  printf "hello world -> ";
  print_str_list (capitalize_all ["hello";"world"]);
  printf "1 * 2 * 3 * 4 * 5 = %d\n" (product one_to_five);
  
  printf "[[you];[are];[here]] -> ";
  printf "[you are here] -> ";
  print_str_list (concat [["you"];["are"];["here"]]);
