(*  COMP 212 Lab 8:  foldr.
*   
*   N. Danner
*)

structure Lab8 =
struct

  (*  You must implement concat, implode, sumPairs, and filter using foldr.
  *   Each function declaration must be of the form
  *     val f = foldr ...
  *   except filter, which must have the form
  *     fun filter p = foldr ...
  *)

  (*  concat [s_0,...,s_{n-1}] = s_0 ^ ... ^ s_{n-1}.
  *)
  val concat : string list -> string =
    foldr (op ^) ""

  (*  implode [c_0,...,c_{n-1}] = 
  *    (Char.toString c_0) ^ ... ^ (Char.toString *  c_{n-1}).
  *)
  val implode : char list -> string = 
    foldr (fn (x, r) => (Char.toString x) ^ r) ""

  (*  sumPairs [(x_0,y_0),...,(x_{n-1}, y_{n-1})] =
  *     [x_0+y_0,...,x_{n-1}+y_{n-1}].
  *)
  val sumPairs : (int*int) list -> int list =
    foldr (fn ((x, y), r) => (x + y)::r) []

  (*  filter p xs = List.filter p xs.
  *)
  fun filter (p : 'a -> bool) : 'a list -> 'a list =
    foldr (fn (x, r) => if p(x) then x::r else r) []

  (*  sumUpPairs [(x_0,y_0),...,(x_{n-1}, y_{n-1})] =
  *     x_0+y_0+...+x_{n-1}+y_{n-1}.
  *
  *   This function must be implemented with a val declaration using foldr.
  *   Hint:  use function composition and sumPairs.
  *)
  val sumUpPairs : (int*int) list -> int =
    foldr (fn ((x,y), r) => (x + y) + r) 0

end
