(*  COMP 212 Homework 6:  Street map representation.
*   
*   N. Danner
*
*   Throughout the documentation of this structure, if x : 'a vector,
*   then we write x[i] for Vector.sub(x, i).
*)

structure Hw6Graph =
struct

  (*  The type of a vertex.
  *)
  type vertex = int

  (*  The type of an undirected graph.  If g : graph, then g represents the 
  *   graph (V, E), where
  *     - V = {0,...,Vector.length g}
  *     - E = {(u, v) : v is a member of g[u]}.
  *   Invariant:  if v is a member of g[u], then u is a member of g[v].
  *)
  type graph = vertex list vector


end
