open Printf

let list1 = 0 :: []
let list2 = [1; 2; 3; 4; 5]
let list3 = 0 :: 1 :: 2 :: 3 :: (4 :: (5 :: []))

let print_int_list l =
  List.iter (printf "%d") l;
  printf "\n"

let hd l =
  match l with
  | [] -> -1
  | h :: _ -> h

let print_list_head l =
  printf "Head of ";
  List.iter (printf "%d") l;
  printf " is %d\n" (hd l)

let hd2 l =
  match l with
  | [] -> None
  | h :: _ -> Some h

let print_list_head2 l =
  printf "Head of [";
  List.iter (printf " %d ") l;
  printf "] is ";
  match hd2 l with
  | None -> printf "None\n"
  | Some value -> printf "%d\n" value

let rec sum (l : int list) : int =
  match l with
  | [] -> 0
  | h::t -> (sum t) + h

let rec is_sorted (l : int list) : bool =
  match l with
  | [] -> true
  | [x] -> true
  | h1::h2::t -> (h1 <= h2) && is_sorted (h2::t)

let rec append1 (l1 : int list) (l2 : int list) : int list =
  match l1 with
  | [] -> l2
  | h::t -> h :: (append1 t l2)

let () =
  print_int_list list1;
  print_int_list list2;
  
  print_int_list list3;
  print_int_list (List.append list1 list2);
  print_list_head list2;
  
  print_list_head2 list3;
  print_list_head2 [];

  printf "Sum of [1,2,3,4,5] is %d\n" (sum list2);
  
  printf "[1,2,3,4,5] is sorted: %B\n" (is_sorted list2);
  printf "[1,2,3,5,4] is sorted: %B\n" (is_sorted [1;2;3;5;4]);
  printf "[] is sorted: %B\n" (is_sorted []);
  printf "[2] is sorted: %B\n" (is_sorted [2]);

  printf "[1,2,3] ++ [4,5,6] = ";
  print_int_list (append1 [1;2;3] [4;5;6]);
