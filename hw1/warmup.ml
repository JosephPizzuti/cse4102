(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 1 **)
(** Warmup **)

let rec sum (n: int) : int =
  match n with
  | 0 -> 0
  | n -> n + sum(n-1)

let rec sumsq (n: int) : int =
  match n with
  | 0 -> 0
  | n -> (n * n) + sumsq(n-1)

let rec sumOdd (n: int) : int =
  match n with
  | _ when n <= 0 -> 0
  | 1 -> 1
  | n -> ((n*2)-1) + sumOdd (n-1)

let rec fib (n: int) : int =
  if n <= 1 then 1
  else fib (n - 1) + fib (n - 2)

let fibFast n =
  let rec aux iter curr next =
    if iter = 0 then curr
    else aux (iter-1) next (curr+next)
  in if n <= 0
  then 0
  else aux n 1 1

let sinappx (n: int) (x : float) : float =
  let rec aux i curr acc =
    if i > n then acc
    else
      let next_i = float_of_int (2*i) in
      let multiplier = -. (x *. x) /. (next_i *. (next_i +. 1.)) in
      let next = curr *. multiplier in
      aux (i + 1) next (acc +. next)
  in if n < 0 then 0.
  else aux 1 x x

let rec repeat (c: char) (n: int) : char list =
  let rec aux n acc =
    if n <= 0
    then acc
    else aux (n-1) (c::acc)
  in aux n []

let rec run_length_encode (l : char list) : (char * int) list =
  let rec aux curr_letter count list acc =
    match list with
    | [] -> (curr_letter,count) :: acc
    | h::t ->
        if h = curr_letter then aux curr_letter (count+1) t acc
        else aux h 1 t ((curr_letter,count) :: acc) 
in match l with
| [] -> []
| h::t ->
    List.rev (aux h 1 t [])

let rec run_length_decode (l: (char * int) list) : char list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | (char,count)::t ->
        if count = 0 then aux t acc
        else aux ((char,count-1)::t) (char::acc)
in match l with
| [] -> []
| l ->
    List.rev (aux l [])

let () =
  assert (sum 0 = 0);
  assert (sum 10 = 55);
  
  assert (sumsq 0 = 0);
  assert (sumsq 5 = 55);

  assert (sumOdd 1 = 1);
  assert (sumOdd 5 = 25);
  assert (sumOdd 10 = 100);

  assert (fibFast 0 = 0);
  assert (fibFast 1 = 1);
  assert (fibFast 2 = 1);
  assert (fibFast 3 = 2);
  assert (fibFast 10 = 55);

  (*
  Printf.printf "sinappx(n=1, x=0) should be 0, and yields %f\n" (sinappx 1 0.);
  Printf.printf "sinappx(n=0, x=3) should be 3, and yields %f\n" (sinappx 0 3.);
  Printf.printf "sinappx(n=10, x=pi/6) should be 0.5, and yields %f\n" (sinappx 10 (Float.pi /. 6.));
  Printf.printf "sinappx(n=10, x=-pi/6) should be -0.5, and yields %f\n" (sinappx 10 (Float.pi /. 6.) *. -1.);
  Printf.printf "sinappx(n=10, x=pi) should be 0, and yields %f\n" (sinappx 10 (Float.pi));
  *)

  assert (repeat 'a' 0 = []);
  assert (repeat 'b' 5 = ['b';'b';'b';'b';'b']);

  assert (run_length_encode [] = []);
  assert (run_length_encode ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'] = [('a', 5); ('b', 3)]);
  assert (run_length_encode ['a'; 'a'; 'b'; 'b'; 'b'; 'a'] = [('a', 2); ('b', 3); ('a', 1)]);

  assert (run_length_decode [] = []);
  assert (run_length_decode [('a', 5); ('b', 3)] = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b']);


