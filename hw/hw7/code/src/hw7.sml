(*  COMP 212 Homework 7:  Braun vectors.
*   
*   N. Danner
*)

(*  A structure for Braun vectors.
*   
*   In the documentation for the functions, we use SML/NJ notation
*   for describing vectors:  #[x_0,...,x_{n-1}] is notation for the
*   vector v such that length(v) = n and sub(v, i) = x_i.  Note, though,
*   that we cannot write these literals in code, because as literals,
*   they always refer to values of type Vector.vector.
*)
structure Hw7BraunVector =
struct

exception empty
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
  fun length ((_, n) : 'a vector) : int =
    n

  (*  sub (update (v, i, x), j) = sub(v, j), j <> i
  *                               x,         j = i.
  *   
  *   Pre-condition:  0 <= i <= length(v).
  *   Raises Subscript if i < 0 or length(v) < i.
  *)
  fun update ((t, n) : 'a vector, i : int, x : 'a) : 'a vector =
    let
    (* update2 (t, i', x) = t', 
    *   where t' contains x and t does not
    *)
      fun update2 (t : 'a tree, i' : int, x: 'a) : 'a tree =
          case (t, i') of 
            (E, 1) => N(x, E, E)
          | (N(v, t0, t1), 1) => N(x, t0, t1) 
          | (E, _) => raise empty
          | (N(v, t0, t1), i') => 
              if (i' mod 2) = 0 
              then N(v, update2(t0, i' div 2, x), t1)
              else N(v, t0, update2(t1, i' div 2, x))
    in
      if i < 0 orelse i > n then raise Subscript else 
      if i = n then (update2(t, i+1, x), n+1) else (update2(t, i+1, x), n)
    end

  (*  sub (#[x_0,...,x_{n-1}], i) = x_i.
  *
  *   Pre-condition:  0 <= i < n.
  *   Raises Subscript if i < 0 or n <= i.
  *)
  fun sub ((t, n) : 'a vector, i : int) : 'a =
    let 
  (*  sub2 (#[x_0,...,x_{n-1}], i) = x_i.
  *
  *   Pre-condition:  0 <= i < n.
  *   Raises Subscript if i < 0 or n <= i.
  *)
      fun sub2 (t' : 'a tree, i' : int) : 'a =
        case (t') of
          ((E)) => raise empty
          |(N(v, t0, t1)) => if i' = 1 then v else 
            if (i' mod 2) = 0 then sub2(t0, (i' div 2)) else sub2(t1, (i' div 2))
    in
       if i < 0 orelse i > n then raise Subscript else sub2(t, i+1)
    end 

  (*  tabulate(n, f) = #[f(0),...,f(n-1)].
  *)
  fun tabulate (n : int, f : int -> 'a) : 'a vector =
    let
      (*  tabulate2(n, f) = #[f(0),...,f(n-1)].
  *)
      fun tabulate2 (i : int, v : 'a vector) : 'a vector =
        if i = n then v else tabulate2(i + 1, (update(v, i, f(i))))
    in
      tabulate2(0, (E,0))
    end

  (*  fromList [x_0,...,x_{n-1}] = #[x_0,...,x_{n-1}].
  *)
 fun fromList (xs : 'a list) : 'a vector =
    let
      (*  fromList2 [x_0,...,x_{n-1}] = #[x_0,...,x_{n-1}].
  *)
      fun fromList2((t, n) : 'a vector, xs' : 'a list, i : int) : 'a tree =
        case xs' of
        [] => t
        | x''::xs'' => fromList2(update((t, n), i, x''), xs'', i+1)
    in
      (fromList2((E, 0), xs, 0), List.length(xs))
    end

  (*  toList #[x_0,...,x_{n-1}] = [x_0,...,x_{n-1}].
  *)
  fun toList ((t, n) : 'a vector) : 'a list =
    let
      (*  toList2 #[x_0,...,x_{n-1}] = [x_0,...,x_{n-1}].
  *)
      fun toList2 (i : int) : 'a list =
        if i = n then [] else sub((t, n), i)::toList2(i+1)
    in
      toList2(0)
    end

  (*  map f #[x_0,...,x_{n-1}] = #[f(x_0),...,f(x_{n-1})].
  *)

  fun map (f : 'a -> 'b) ((t, n) : 'a vector) : 'b vector =
    let
      (*  map2 f #[x_0,...,x_{n-1}] = #[f(x_0),...,f(x_{n-1})].
  *)
      fun map2 ((t') : 'a tree) : 'b tree =
        case t' of
          E => E
          |N(v, t0, t1) => N(f(v), map2(t0), map2(t1))
    in
      (map2(t), n)
    end

end
