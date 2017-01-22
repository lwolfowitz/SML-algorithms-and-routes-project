(*  COMP 212 Lab 7:  Higher-order functions and datatype definitions.
*   
*   N. Danner
*)

structure Lab7 =
struct

  (*  You must implement the following functions using map and List.filter
  *   from the Standard Basis Library.  All function arguments must
  *   be functions defined in the Standard Basis Library or anonymous 
  *   functions (i.e., fn expressions).  For full credit, each function
  *   must be declared with a val declaration (i.e., not a fun declaration).
  *)

  (*  toReals [x_0,...,x_{n-1}] = [real(x_0),...,real(x_{n-1})].
  *)
  val toReals : int list -> real list = 
    map (Real.fromInt)

  (*  toAbsReals [x_0,...,x_{n-1}] = [|real(x_0)|,...,|real|(x_{n-1})].
  *)
  val toAbsReals : int list -> real list = 
    map (Real.fromInt o Int.abs)

  (*  oneCharStrings [s_0,...,s_{n-1}] = [s_{i_0},...,s_{i_{m-1}}], where
  *     - i_0 < ... < i_{m-1}; and
  *     - s_i = s_{i_j} if and only if s_i is one character long.
  *)
  val oneCharStrings : string list -> string list = 
    List.filter (fn x => String.size(x) = 1)

  (*  An "IR tree" is defined inductively as follows:
  *     - The empty tree is an IR tree.
  *     - i-nodes:  an i-node tree is constructed from an integer value
  *       and two IR trees.
  *     - r-nodes:  an r-node tree is constructed from a real value and
  *       two IR trees.
  *   In other words, an IR tree is a binary tree in which the nodes are
  *   labeled by either integer or real values.
  *)

  (*  Define a datatype irtree using the following constructors,
  *   paralleling the inductive definition:
  *     - Empty;
  *     - INode, with domain int*irtree*irtree;
  *     - RNode, with domain real*irtree*irtree.
  *)
  datatype irtree = Empty | INode of int*irtree*irtree | RNode of real*irtree*irtree

  (*  Define the following function by recursion on the irtree value.
  *   
  *   sum(t) = x_0 + ... + x_{n-1}, where keys(t) = {x_0,...,x_{n-1}}.
  *   By convention, the empty sum is 0.0.
  *)
  fun sum(t : irtree) : real =
    case t of
      Empty => 0.0
      |INode(n, t1, t2) => Real.fromInt(n) + sum(t1) + sum(t2) 
      |RNode(n, t1, t2) => n + sum(t1) + sum(t2)

end
