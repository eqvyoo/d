 (* 1. Basic Recursion *)

  (* sum n: calculate 1 + 2 + ... + n *)
  fun sum 1 = 1
    | sum n = n + sum (n-1)

  (* fac n: calculate 1 * 2 * ... * n *)
  fun fac n = if n = 0 then 1 else n * fac(n-1)
   
   fun fib 1 = 1
    | fib 0 = 1
    | fib n = fib(n-1) + fib(n-2)

  (* gcd (x, y): find the great common divisor of x and y *)
  fun gcd (x, y) = if y=0 then x else gcd(y,x mod y)

  (* 2. List *)

  (* max l: return the maximum value in l *)
  fun max [] = 0
    | max [x] = x
    | max (x::xs) = if x > max xs then x else max xs

  (* exist l x: check if x exists in l *)
  fun exist ([], x) = false
    | exist (l::ls, x) = (l=x) orelse exist (ls,x);

  (* reverse l: return the reversed l *)
fun reverse [] = []
   | reverse (x::xs) = reverse(xs) @ [x];
