let rec gcd x y =
  if x < 0 then gcd (-x) y
  else if y < 0 then gcd x (-y)
  else if x = y then x
  else if x > y then gcd (x-y) y
  else gcd x (y-x)

let fib n =
  let rec fibr n a b =
    if n <= 0 then a
    else fibr (n-1) b (a+b)
  in
  fibr n 0 1

let prime n =
  let rec is_prime n d =
    if d*d > n then true
    else if n mod d == 0 then false
    else is_prime n (d+1)
  in
  let rec inner n m =
    if m == 0 then (n-1)
    else if is_prime n 2 then inner (n+1) (m-1)
    else inner (n+1) m
  in
  inner 2 n

let substring s1 s2 =
  let rec inner s1 s2 n =
    if (String.length s2 + n) > (String.length s1) then -1
    else if String.sub s1 n (String.length s2) = s2 then n
    else inner s1 s2 (n+1)
  in
  inner s1 s2 0

let quicksort l =
  let rec qsort l =
    match l with
    | [] -> []
    | x::xs -> let left, right = List.partition (fun y -> y < x) xs in
               qsort left @ [x] @ qsort right
  in
  qsort l

let () =
  let a, b = 56, 98 in
  Printf.printf "gcd(%d, %d) = %d\n" a b (gcd a b);  (* 14 *)

  let a, b = -18, 24 in
  Printf.printf "gcd(%d, %d) = %d\n" a b (gcd a b);  (* 6 *)

  let n = 10 in
  Printf.printf "fib(%d) = %d\n" n (fib n);  (* 55 *)

  let nth_prime = 10 in
  Printf.printf "%dth prime = %d\n" nth_prime (prime nth_prime);  (* 29 *)

  print_endline (string_of_int (substring "hello world" "world")); (* 6 *)
  print_endline (string_of_int (substring "hello world" "earth")); (* -1 *)

  let list_to_sort = [5; 1; 9; 3; 7; 6] in
  Printf.printf "quicksort of [%s] = [%s]\n"
    (String.concat "; " (List.map string_of_int list_to_sort))
    (String.concat "; " (List.map string_of_int (quicksort list_to_sort)));  (* [1; 3; 5; 6; 7; 9] *)
