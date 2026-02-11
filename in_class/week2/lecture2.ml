open Printf

let result1 =
  let x = 1 in 
  (x+x) * (let x = 2 in x + x)

let result2 =
  let x = (let y = 3 in y * 2) + 5 in 
  (let y = 1 in x + y)

let func1 x = x + 1
let func2 x = x * x
let ten = (fun (x:int) -> x * 10) 10

let use1 = func2 5

(* Factorial; this is an interesting way to comment*)
let rec fact (n:int) : int =
  if n <= 0 then 1
  else n * fact (n-1)

(* Fib *)
let rec fib (n:int) : int =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-2) + fib (n-1)

(* This pattern matching allows multiple arguments! *)
let rec fib2 n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib2 (n-2) + fib2 (n-1)

(* This pattern matching does NOT allow multiple arguments! only 1! *)
let rec fib3 = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib3 (n-2) + fib3 (n-1)

let () =
  printf "Result 1: %d\n" result1;
  printf "Result 2: %d\n" result2;
  printf "3 + 1 = %d\n" (func1 3);
  printf "5 * 5 = %d\n" (use1);
  printf "10 * 10 = %d\n" ten;
  printf "fact(%d) = %d\n" 5 (fact 5);
  printf "fib(%d) = %d\n" 10 (fib 10);
  printf "fib2(10) = %d\n" (fib2 10);
  printf "fib3(10) = %d\n" (fib3 10);

  (* Assert does nothing if it works; otherwise kills program and throws *)
  assert (fact 5 = 120);
  assert (10 = 10);
  printf "Done!\n"

