(*  COMP 212 final exam:  weighted graph signature.
*
*   This signature specifies the abstract type of weighted directed
*   graphs.  Some conventions:
*
*   - When we say that g is a graph, we mean g is a weighted directed graph.
*   - When we say that g is a graph on n vertices, we mean that g is
*     a graph with vertices {0,...,n-1}.
*
*   N. Danner
*)

signature WEIGHTED_GRAPH =
sig

  (*  The following structure declaration and type definitions would
  *   typically not be part of a signature for (weighted) graphs, because
  *   they are not of interest or value to the typical client.
  *
  *   However, I'm not your typical client.  I need to have the underlying
  *   types exposed, so that my testing code can manually build weighted
  *   graphs, something we would not typically trust a client to do.
  *
  *   A typical signature would replace this structure declaration and
  *   type definitions with the single type declaration
  *
  *     type graph
  *
  *)
  structure V : COMP212VECTOR

  type vertex = int
  type adjlist = (vertex*real) list
  type graph = adjlist V.vector

  exception BadVertex

  (*  new n = a weighted graph on n vertices.
  *)
  val new : int -> graph

  (*  size g = the number of vertices in g.
  *)
  val size : graph -> int

  (*  fromList(n, [(u_0,u_0',w_0),...]) = g, where g is the weighted
  *   directed graph on n vertices such that there is an edge
  *   from u to u' with weight w if and only if (u, u', w) = (u_i, u_i', w_i)
  *   for some i.
  *
  *   Raises BadVertex if there is some u_i or u_i' such that u_i < 0 or
  *   u_i >= n or u_i' < 0 or u_i' >= n.
  *)
  val fromList : int*((vertex*vertex*real) list) -> graph

  (*  addEdge(g, u, (v, w)) = g', where g' is a graph on size(g) vertices
  *   and there is an edge from u' to v' with weight w' if
  *   - there is an edge from u' to v' with weight w' in g; or
  *   - (u', v', w') = (u, v, w).
  *   If there is an edge of the form (u, v, w') in g for some w',
  *   then g' = g.
  *
  *   Raises BadVertex if u or v is not a vertex in g.
  *)
  val addEdge : graph*vertex*(vertex*real) -> graph

  (*  hasEdge(g, u, v) = true if there is an edge from u to v in g,
  *                      false, otherwise.
  *
  *   Raises BadVertex if u or v is not a vertex in g.
  *)
  val hasEdge : graph*vertex*vertex -> bool

  (*  weight(g, u, v) = w, where the edge from u to v in g has weight w.
  *   Raises BadVertex if u or v is not a vertex in g.
  *   Raises BadVertex if there is no edge from u to v in g.
  *)
  val weight : graph*vertex*vertex -> real

  (*  getNeighbors(g, u) = [(v_0,w_0),...], where (v, w) = (v_i, w_i) if
  *   and only if there is an edge from u to v_i with weight w_i.
  *
  *   Raises BadVertex if u is not a vertex in g.
  *
  *   In other words, getNeighbors(g, u) returns the list of pairs
  *   (v, w) such that there is an edge from u to v of weight w in g.
  *)
  val getNeighbors : graph*vertex -> (vertex*real) list

  (*  dist (g, u, v) = SOME d, if there is a path from u to v in g,
  *                            and d is the minimal weight of any such path.
  *                  = NONE, if there is no path from u to v in g.
  *   Raises BadVertex if u or v is not a vertex in g.
  *)
  val dist : graph*vertex*vertex -> real option

  (*  path (g, u, v) = [], if there is no path from u to v in g
  *                  = [u_0,...,u_{n-1}], where u_0 = u, u_{n-1} = v,
  *   and there is an edge from u_i to u_{i+1} for all 0 <= i < n-1.
  *   Furthermore,
  *     sum_i(weight(g, u_i, u_{i+1}))
  *   is minimal among all paths from u to v.
  *
  *   Raises BadVertex if u or v is not a vertex in g.
  *
  *   In other words, path(g, u, v) is a minimal-weight path from u to v
  *   in g, provided such a path exists.
  *)
  val path : graph*vertex*vertex -> vertex list

  (*  toString g = a string representation of g.
  *)
  val toString : graph -> string

end
