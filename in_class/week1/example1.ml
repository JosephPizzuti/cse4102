print_int (1+2);
print_endline "";
print_string ("Hello there!" ^ " you there!\n");
print_endline (if 5 = 0 then "what" else "OK");

let y = 0 in
print_endline (if y = 0 then "OK: y=0" else "not good");

print_float (3.21 +. 4.32);
print_endline "";
