(*  COMP 212 final exam:  a vector signature.
*   
*   N. Danner
*
*   This is a signature for vectors; it is a subset of ML's VECTOR signature
*   because I haven't implemented all the functions in that signature.
*   We use a different name here to ensure that we don't conflict with 
*   the standard library signature.
*)

signature COMP212VECTOR =
sig

  (*  The type of a vector of values of type 'a.
  *)
  type 'a vector

  (*  length(#[x_0,...,x_{n-1}]) = n.
  *)
  val length : 'a vector -> int

  (*  updaate(#[x_0,...,x_{n-1}], i, x)
  *     = #[x_0,...,x_{i-1},x,x_{i+1},...,x_{n-1}], 0 <= i < n
  *     = #[x_0,...,x_{n-1},x], i = n.
  *)
  val update : 'a vector * int * 'a -> 'a vector

  (*  sub(#[x_0,...,x_{n-1}], i) = x_i.
  *)
  val sub : 'a vector * int  -> 'a

  (*  tabulate(n, f) = #[f(0),...,f(n-1)].
  *)
  val tabulate : int * (int -> 'a) -> 'a vector

  (*  fromList([x_0,...,x_{n-1}]) = #[x_0,...,x_{n-1}].
  *)
  val fromList : 'a list -> 'a vector

  (*  map f #[x_0,...,x_{n-1}] = #[f(x_0),...,f(x_{n-1})]
  *)
  val map : ('a -> 'b) -> ('a vector) -> ('b vector)

  (*  foldri f b #[x_0,...,x_{n-1}] = 
  *     f(0, x_0, f(1, x_1, f(...,f(n-1, x_{n-1}, b)...)))
  *)
  val foldri : (int*'a*'b -> 'b) -> 'b -> 'a vector -> 'b

end
