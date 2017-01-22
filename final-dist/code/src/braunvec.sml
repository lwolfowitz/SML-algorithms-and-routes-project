(*  A structure for Braun vectors.
*   
*   In the documentation for the functions, we use SML/NJ notation
*   for describing vectors:  #[x_0,...,x_{n-1}] is notation for the
*   vector v such that length(v) = n and sub(v, i) = x_i.  Note, though,
*   that we cannot write these literals in code, because as literals,
*   they always refer to values of type Vector.vector.
*
*   This is a dummy implementation.  You must have some implementation
*   of COMP212VECTOR in order to compile the test code, *even if you do
*   not use BraunVector in your solution*.  You may also replace this
*   dummy implementation with the solutions to HW 7.
*)
structure BraunVector : COMP212VECTOR =
struct

  (*  The type of an 'a-labeled binary tree.
  *)
  datatype 'a tree = E | N of 'a*('a tree)*('a tree)

  (*  The type of a Braun vector.  A value (t, n) represents the
  *   vector #[sub.subTree(t, 1),...,sub.subTree(t, n)] if t is
  *   non-empty, and the vector #[] if it is empty.
  *
  *   Note, though, that from the client's perspective, a vector
  *   is always 0-indexed, not 1-indexed!  In particular,
  *   if v = (t, n) : 'a vector, then
  *     sub(v, i) = sub.subTree(i+1).
  *)
  type 'a vector = ('a tree)*int

  (*  length (#[x_0,...,x_{n-1}]) = n.
  *   
  *   This function must be constant-time.
  *)
  fun length ((_, n) : 'a vector) : int = 0

  (*  sub (#[x_0,...,x_{n-1}], i) = x_i.
  *
  *   Pre-condition:  0 <= i < n.
  *   Raises Subscript if i < 0 or n <= i.
  *)
  fun sub ((t, n) : 'a vector, i : int) : 'a =
    raise Fail ""

  (*  sub (update (v, i, x), j) = sub(v, j), j <> i
  *                               x,         j = i.
  *   
  *   Pre-condition:  0 <= i <= length(v).
  *   Raises Subscript if i < 0 or length(v) < i.
  *)
  fun update ((t, n) : 'a vector, i : int, x : 'a) : 'a vector =
    (E, 0)

  (*  tabulate(n, f) = #[f(0),...,f(n-1)].
  *)
  fun tabulate (n : int, f : int -> 'a) : 'a vector =
    (E, 0)

  (*  fromList [x_0,...,x_{n-1}] = #[x_0,...,x_{n-1}].
  *)
  fun fromList (xs : 'a list) : 'a vector =
    (E, 0)

  (*  toList #[x_0,...,x_{n-1}] = [x_0,...,x_{n-1}].
  *)
  fun toList ((t, n) : 'a vector) : 'a list =
    []

  (*  map f #[x_0,...,x_{n-1}] = #[f(x_0),...,f(x_{n-1})].
  *   
  *   Here is a lovely functional version:
  *     fun map (f : 'a -> 'b) = fromList o (List.map f) o toList
  *   Unfortunately, it isn't very efficient, because conversion
  *   to/from lists is O(n*log(n)) time.  Instead, we implement
  *   the map directly, which is O(n) time.
  *)
  fun map (f : 'a -> 'b) ((t, n) : 'a vector) : 'b vector =
    (E, 0)

  (*  foldri f b #[x_0,...,x_{n-1}] =
  *     f(0, x_0, f(...f(n-2, x_{n-2}, f(n-1, x_{n-1}, b))...)).
  *)
  fun foldri (f : int*'a*'b -> 'b) (b : 'b) (v : 'a vector) : 'b =
  let
    fun g (n : int) : 'b =
      if n = length v then b
      else f(n, sub(v, n), g(n+1))
  in
    g(0)
  end

end
