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
  | 1 -> 1
  | n -> if (n % 2 = 0) then sumOdd(n-1) else n + sumOdd(n-1)

let rec fib (n: int) : int =
  if n <= 1 then 1
  else fib (n - 1) + fib (n - 2)

let fibFast n =
  match n with
  | _ when n < 1 -> 0
  | 1 -> 1
  | _ ->
      let rec aux n a b =
        if n = 0 then a
        else aux (n-1) b (a+b)
      in aux n 0 1

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
  raise Util.Unimplemented

let rec run_length_encode (l : char list) : (char * int) list =
  raise Util.Unimplemented

let rec run_length_decode (l: (char * int) list) : char list =
    raise Util.Unimplemented
