open Printf

let tuple1 = (1, 1, true)
let tuple2 = (1, 2)
let (a, b, c, d) = ('a', 'b', 'c', 'd')

(*val fst3 : 'a * 'b * 'c -> 'a = <fun>
  ' followed by anything means the var can take any type*)
let fst3 (x,_,_ : 'fst * 'snd * 'trd) = x

let id (x : 'a -> 'a) = x
let swap (x, y) = (y, x)

let () =
  match tuple1 with
  | (x, y, z) -> printf "%d %d %B\n" x y z;
  
  printf "%d %d\n" (fst tuple2) (snd tuple2);
  printf "%c%c%c%c\n" a b c d;

  printf "First of (4,5,6) is %d\n" (fst3 (4,5,6)); 

  printf "If we swap (%d, %d), we get (%d, %d)\n" (fst tuple2) (snd tuple2) (fst (swap tuple2)) (snd (swap tuple2))

