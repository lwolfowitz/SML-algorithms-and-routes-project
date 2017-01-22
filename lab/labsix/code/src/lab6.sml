(*  COMP 212 Lab 6:  Higher-order functions.
*   N. Danner
*)

structure Lab6 =
struct

  (*  The following three functions are just the three functions
  *   defined in EOMLP, Section 5.4 (except we call our function map
  *   instead of simpleMap).
  *)

  (*  map(f, [x_0,...,x_{n-1}]) = [f(x_0),...,f(x_{n-1})].
  *)
  fun map(f : 'a -> 'b, xs : 'a list) : 'b list =
    case xs of
         [] => []
       | x' :: xs' => f(x') :: map(f, xs')

  (*  reduce(f, [x_0,...,x_{n-1}]) = 
  *     f(x_0, f(x_1, f(...f(x_{n-2}, x_{n-1})...))).
  *)
  fun reduce(f : 'a*'a -> 'a, xs : 'a list) : 'a =
    case xs of
         [] => raise Empty
       | [x] => x
       | x' :: xs' => f(x', reduce(f, xs'))

  (*  filter(p, [x_0,...,x_{n-1}]) = [x_{i_0},...,x_{i_{m-1}}], where
  *     - i_0 < i_1 < ... < i_{m-1};
  *     - p(x_{i_j}) for 0 <= j < m;
  *     - not p(x_i) if i <> i_j for some 0 <= j < m.
  *   I.e., filter(p, xs) is the list consisting of those elements x of xs
  *   such that p(x), in the order they appear in xs.
  *)
  fun filter(p : 'a -> bool, xs : 'a list) : 'a list =
    case xs of
         [] => []
       | x' :: xs' => if p x' then x' :: filter(p, xs') else filter(p, xs')

  (*  You should try to write each of the following functions first
  *   using named functions for the function arguments.  For example,
  *   your first solution to negsToZero would look something like
  *
  *     fun negsToZero(xs : real list) : real list =
  *     let
  *       fun f(...) = ...
  *     in
  *       map (f, xs)
  *     end
  *
  *   Once you've done this succesfully, rewrite your solution to
  *   use an anonmyous function, so that in the end your solution
  *   looks something like
  *
  *     fun negsToZero(xs : real list) : real list =
  *       map (fn ... => ..., xs)
  *)

  (*  negsToZero([x_0,...,x_{n-1}]) = [y_0,...,y_{n-1}], where
  *     - y_i = x_i if x_i >= 0;
  *     - y_i = 0.0 if x_i < 0.
  *
  *   You must implement this function using map.
  *)
  fun negsToZero(xs : real list) : real list =
      map(fn x => if x < 0.0 then 0.0 else x, xs)

  (*  min([x_0,...,x_{n-1}]) = x_i, where x_i <= x_j for all 0 <= j < n.
  *   Pre-condition:  n >= 1.
  *
  *   You must implement this function using reduce.
  *)
  fun min(xs : real list) : real =
      reduce(fn (x, y) => if y < x then y else x, xs)

  (*  f([x_0,...,x_{n-1}]) = [x_{i_0},...,x_{i_{m-1}], where
  *     - i_0 < ... < i_{m-1};
  *     - 1.0 <= x_{i_j} < 2.0 for all 0 <= j < m;
  *     - x_i < 1.0 or x_i >= 2.0 if i <> i_j for any j.
  *
  *   You must implement this function using filter.
  *)
  fun f(xs : real list) : real list =
    filter(fn (x) => 1.0 <= x andalso x < 2.0, xs)

  (*  squares(n) = #[0, 1, 4, 9,...,(n-1)^2].
  *
  *   You must implement this function using Vector.tabulate.
  *
  *   Note that #[x_0,...,x_{n-1}] is SML/NJ notation for the vector v
  *   such that Vector.length v = n and Vector.sub(v, i) = x_i.
  *)
  fun squares(n : int) : int vector =
    Vector.tabulate(n, fn x => (x*x))

end
