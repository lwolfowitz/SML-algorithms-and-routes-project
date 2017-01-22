(*  COMP 212 Lab 5:  Exceptions
*)

structure Lab5 =
struct

  (*  Define two exceptions:  
  *   * TooShort, which takes no arguments;
  *   * BadIndex, which takes a single argument of type int.
  *)
  exception TooShort
  exception BadIndex of int

  (*  third ([x_0,...,x_{n-1}]) = x_2.
  *   Raises TooShort if n < 3.
  *)
  fun third(xs : int list) : int =
    case xs of
      x::x'::x''::ys => x''
      |_ => raise TooShort

  (*  take ([x_0,...,x_{n-1}], m) = [x_0,...,x_{m-1}].
  *   Pre-condition:  0 <= m.
  *   Raises TooShort if m > n.
  *)
  fun take(xs : int list, n : int) : int list =
    case (xs, n) of
      (_, 0) => []
      | (y::ys, _) => y:: take (ys, n-1)
      | (_, _) => raise TooShort

  (*  take2 ([x_0,...,x_{n-1}], m) = [x_0,...,x_{m-1}].
  *   Raises BadIndex(m) if m < 0 or m > n.
  *)
  fun take2(xs : int list, n : int) : int list =
    case (xs, n) of
      (_, 0) => []
      | (y::ys, _) => y:: take (ys, n-1)
      | (_, _) => raise BadIndex(n)

  (*  nthOpt([x_0,...,x_{n-1}], m) = SOME(x_m), 0 <= m < n
  *                                = NONE, m < 0 or n < m.
  *   This function must be implemented by invoking List.nth and
  *   handling the exception it might raise appropriately.
  *)
  fun nthOpt(xs : int list, n : int) : int option =
    SOME(List.nth(xs,n)) handle Subscript => NONE

end
