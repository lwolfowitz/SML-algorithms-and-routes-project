(*  COMP 212 Lab 2:  Recursive functions, list patterns.
*   N. Danner
*)

structure Lab2 =
struct

  (*  minInt is the smallest integer value.
  *)
  val minInt = valOf Int.minInt

  (*  length [x_0,...,x_{n-1}] = n.
  *)
  fun length (xs : int list) : int = if xs = [] then 0 else 1 + length (tl(xs));

  (*  sum [x_0,...,x_{n-1}] = x_0 + x_1 + ... + x_{n-1}.
  *)
  fun sum (xs : int list) : int = if xs = [] then 0 else hd(xs) + sum(tl(xs));

  (*  dup [x_0,...,x_{n-1}] = [x_0, x_0, x_1, x_1,...,x_{n-1}, x_{n-1}].
  *)
  fun dup(xs : int list) : int list = if length (xs) = 0
   then [] else (hd(xs)::[hd(xs)]) @ dup(tl(xs))

  (*  max [x_0,...,x_{n-1}] = x_k, where x_k >= x_i for all 0 <= i < n.
  *)
  fun max(xs : int list) : int = if xs= []
   then minInt else Int.max(hd(xs), max(tl(xs)));

  (*  lengthPattern [x_0,...,x_{n-1}] = n.
  *)
  fun lengthPattern (xs : int list) : int = 0

  (*  sumPattern [x_0,...,x_{n-1}] = x_0 + x_1 + ... + x_{n-1}.
  *)
  fun sumPattern (xs : int list) : int = 0

  (*  dupPattern [x_0,...,x_{n-1}] = [x_0, x_0, x_1, x_1,...,x_{n-1}, x_{n-1}].
  *)
  fun dupPattern (xs : int list) : int list = []

  (*  maxPattern [x_0,...,x_{n-1}] = x_k, where x_k >= x_i for all 0 <= i < n.
  *)
  fun maxPattern (xs : int list) : int = 0

end
