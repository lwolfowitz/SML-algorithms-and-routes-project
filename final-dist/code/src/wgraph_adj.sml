(*  COMP 212 final exam:  weighted graph structure.
*   
*   This structure provides an implementation of WEIGHTED_GRAPH based
*   on adjacency lists.
*
*   N. Danner
*)

structure WeightedGraphAdjList : WEIGHTED_GRAPH =
struct

  (*  A vector type for adjacency lists.  This should really use
  *   a more efficiently-updateable vector.
  *)
  structure V = Vector

  (*  An updateable priority queue implementation.  This should really
  *   use a more efficient implementation.
  *)
  structure PQ = ListPQ

  (*  The type of a vertex.
  *)
  type vertex = int

  (*  The type of an adjacency list.
  *)
  type adjlist = (vertex*real) list

  (*  The type of a graph.
  *)
  type graph = adjlist V.vector

  (*  Exception that is raised by functions when applied to vertices that
  *   are not in a graph.
  *)
  type pq = vertex ListPQ.pri_queue


  exception BadVertex

  (*  graph n = ({0,...n-1}, empty); i.e., the graph on n vertices
  *   with no edges.
  *)
  fun new(n : int) : graph =
    V.tabulate (n, fn i => [])

  (*  addToAdjList(us as [(u_0, w_0),...,(u_{n-1}, w_{n-1})], (v, w)) =
  *     [u_0,...,u_{n-1},(v, w)], v <> u_i, any i.
  *     us, v = u_i, some i.
  *)
  fun addToAdjList(is : adjlist, e as (i, _): vertex*real) : adjlist =
    case is of
         [] => [e]
       | (e' as (i', w)) :: is' => 
           if i' = i then is else e' :: (addToAdjList(is', e))

  (*  fromList(n, [(u_0, u_0', w_0),...,(u_{m-1}, u_{m-1}', w_{m-1})]) = g, 
  *   where g is the weighted directed graph with
  *   - size g = n;
  *   - g[u] = [v_0,...,v_{n-1}], where v = v_i if and only if
  *     (u, v) = (u_j, u_j').
  *)
  fun fromList(n : int, es : (int*int*real) list) : graph =
  let

    val adjList : adjlist V.vector = new(n)

    (*  addToAdjList([(u_0,u_0',w_0),...,(u_{n-1},u_{n-1}',w_{n-1})], adj) = 
    *     adj', 
    *   where for all i,
    *     adj'[u_i] = [(v_0,v_0',w_0'),...] @ adj[u_i]
    *   where (v, v', w') = (v_i, v_i', w_i') if an only if
    *   (v, v', w') = (u_i, u_i', w_i) for some i.
    *)
    fun addToAdjLists (es : (int*int*real) list, adjList : adjlist V.vector) =
      case es of
           [] => adjList
         | (i, j, w) :: es => 
             addToAdjLists(es, 
               V.update(adjList, i, addToAdjList(V.sub(adjList, i), (j, w))))
      handle Subscript => raise BadVertex
  in
    addToAdjLists(es, adjList)
  end

  (*  **********
  *   ADD YOUR CODE HERE
  *   **********
  *)
  (*  size g = the number of vertices in g.
  *)
  fun size(g : graph) : int =
    V.length(g)

    (*  hasEdge(g, u, v) = true if there is an edge from u to v in g,
  *                      false, otherwise.
  *
  *   Raises BadVertex if u or v is not a vertex in g.
  *)
  fun hasEdge (g : graph, u : vertex, v : vertex) : bool =
    let
      fun hasEdge2 (pairList :  adjlist) : bool =
        case pairList of
          [] => false
          |y::ys => if #1(y) = v then true else hasEdge2(ys)
      handle Subscript => raise BadVertex
    in
      hasEdge2(V.sub(g, u))
    end
    (*  addEdge(g, u, (v, w)) = g', where g' is a graph on size(g) vertices
  *   and there is an edge from u' to v' with weight w' if
  *   - there is an edge from u' to v' with weight w' in g; or
  *   - (u', v', w') = (u, v, w).
  *   If there is an edge of the form (u, v, w') in g for some w',
  *   then g' = g.
  *
  *   Raises BadVertex if u or v is not a vertex in g.
  *)
  fun addEdge (g : graph, u : vertex, (v, w) : (vertex*real)) : graph =
    if hasEdge (g, u, v) then g else
      V.update(g, u, addToAdjList(V.sub(g, u), (v, w)))
    handle Subscript => raise BadVertex

  (*  weight(g, u, v) = w, where the edge from u to v in g has weight w.
  *   Raises BadVertex if u or v is not a vertex in g.
  *   Raises BadVertex if there is no edge from u to v in g.
  *)
  fun weight(g: graph, u: vertex, v:vertex): real = 
  let 
    val us = V.sub(g,u) handle Subscript => raise BadVertex
    fun weight2(us: adjlist, v: vertex): real = 
        case us of 
             [] => raise BadVertex 
           | (u,w)::ys => if u = v then w else weight2(ys,v)
  in 
    weight2(us,v) 
  end

  (*  getNeighbors(g, u) = [(v_0,w_0),...], where (v, w) = (v_i, w_i) if
  *   and only if there is an edge from u to v_i with weight w_i.
  *
  *   Raises BadVertex if u is not a vertex in g.
  *
  *   In other words, getNeighbors(g, u) returns the list of pairs
  *   (v, w) such that there is an edge from u to v of weight w in g.
  *)
  fun getNeighbors (g : graph, u : vertex) : (vertex*real) list =
    V.sub(g, u)
   (* handle Subscript => BadVertex
*)
  (*  dist (g, u, v) = SOME d, if there is a path from u to v in g,
  *                            and d is the minimal weight of any such path.
  *                  = NONE, if there is no path from u to v in g.
  *   Raises BadVertex if u or v is not a vertex in g.
  *)

  fun dist (g : graph, u : vertex, v : vertex) : real option =
let
      val VisList = V.tabulate (size(g), fn i => false )
      val TDL = V.tabulate (size(g), fn i => NONE)

(* front pq, bool V.vector = vertex,
*   where the returned vertex is the next element in the pq to be examined
*   that is, vertex has not already been visited.
*)
      fun front (PriQue : pq, VisList : bool V.vector) : vertex =
          if PQ.isEmpty(PriQue) then v else 
                     if V.sub(VisList, #1(PQ.delMin(PriQue)))
                     then front(#2(ListPQ.delMin(PriQue)), VisList) else 
                          #1(PQ.delMin(PriQue))

     (* fun toBeAddeds us, TDL = (vertex * real) list
        where the returned list are composed of elements that now belong in the priority queue
        *)
      fun toBeAddeds(us : (vertex * real) list, TDL : real option V.vector) : (vertex * real ) list =
         case us of
          [] => [] 
          | y::ys => (#1(y), valOf(V.sub(TDL, #1(y))))::toBeAddeds(ys,TDL)

      (* fun createPQ pq, vertex, real option V.vector = pq'
        where pq' is the updated pq that now contains the 
        neighbors of u
      *)    
      fun createPQ (PriQue : pq, u : vertex, TDL : real option V.vector) : pq=
        PQ.insertMany(PriQue,(toBeAddeds(getNeighbors(g,u),TDL)))


      (* fun newTDLVal currentVertex, CVTDL, TDL, (newVertex, NVWeight) = 
                                          real option V.vector
         where the returned vector has updated the TDL for some 
          new vertex's weight
      *)
      fun newTDLVal (currentVertex : vertex, CVTDL : real, 
            TDL : real option V.vector,(newVertex,NVWeight)) : 
              real option V.vector = 
        case V.sub(TDL, newVertex) of
           NONE => V.update(TDL, newVertex, SOME(CVTDL + NVWeight))
          |SOME (x) => if x > (CVTDL + NVWeight) then 
                        V.update(TDL, newVertex, SOME(CVTDL + NVWeight)) else TDL

      (* fun newTDLVals currentVertex, CVTDL, TDL, elements = 
                                          real option V.vector
         where the returned vector has updated the TDL for some 
           vertex's neighbors weights
      *)
      fun newTDLVals (currentVertex : vertex, CVTDL : real, 
     TDL : real option V.vector, elements) : real option V.vector list =
        case elements of
          [] => []
          |y::ys => newTDLVal (currentVertex, CVTDL, TDL, y)::
                    newTDLVals (currentVertex, CVTDL, TDL, ys)
(*      Dist2 (g, v, pq, vislist, TDL) = SOME d, if there is a path from u to v in g,
  *                            and d is the minimal weight of any such path.
  *                  = NONE, if there is no path from u to v in g.
  *   Raises BadVertex if u or v is not a vertex in g.
*)
      fun Dist2 (g : graph, v : vertex, PriQue : pq, VisList : bool V.vector,
                   TDL : real option V.vector) : real option = 
        let 
          val u = front(PriQue, VisList)
          val TDL' = V.fromList(newTDLVals(u, valOf(V.sub(TDL, u)), TDL, V.sub(g, u)))
        in
          if u = v then V.sub(TDL, u) else
            Dist2(g, v, createPQ(#2(PQ.delMin(PriQue)), u, TDL), 
                    V.update (VisList, u, true), TDL)
        end
    in
      Dist2(g, v, [(u, 0.0)], VisList, TDL)
      handle Subscript => raise BadVertex
    end

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
  fun path (g : graph, u : vertex, v : vertex) : vertex list =
  []
  (* if i had more time, 
    I would have implemented path by taking dist,
    and attaching a list to each vertex in the TDL
    starting at [], each list would grow to be the shortest found path
    to that vertex. then, i would return the path.
    This implementation is very similar to dist, so i 
    decided to copy-paste dist to reflect my progress,
    unfortunately, I have run out of time for this exam.

  *)
  (*
  let
      val VisList = V.tabulate (size(g), fn i => false )
      val TDL = V.tabulate (size(g), fn i => NONE)

(* front pq, bool V.vector = vertex,
*   where the returned vertex is the next element in the pq to be examined
*   that is, vertex has not already been visited.
*)
      fun front (PriQue : pq, VisList : bool V.vector) : vertex =
          if PQ.isEmpty(PriQue) then v else 
                     if V.sub(VisList, #1(PQ.delMin(PriQue)))
                     then front(#2(ListPQ.delMin(PriQue)), VisList) else 
                          #1(PQ.delMin(PriQue))

     (* fun toBeAddeds us, TDL = (vertex * real) list
        where the returned list are composed of elements that now belong in the priority queue
        *)
      fun toBeAddeds(us : (vertex * real) list, TDL : real option V.vector) : (vertex * real ) list =
         case us of
          [] => [] 
          | y::ys => (#1(y), valOf(V.sub(TDL, #1(y))))::toBeAddeds(ys,TDL)

      (* fun createPQ pq, vertex, real option V.vector = pq'
        where pq' is the updated pq that now contains the 
        neighbors of u
      *)    
      fun createPQ (PriQue : pq, u : vertex, TDL : real option V.vector) : pq=
        PQ.insertMany(PriQue,(toBeAddeds(getNeighbors(g,u),TDL)))


      (* fun newTDLVal currentVertex, CVTDL, TDL, (newVertex, NVWeight) = 
                                          real option V.vector
         where the returned vector has updated the TDL for some 
          new vertex's weight
      *)
      fun newTDLVal (currentVertex : vertex, CVTDL : real, 
            TDL : real option V.vector,(newVertex,NVWeight)) : 
              real option V.vector = 
        case V.sub(TDL, newVertex) of
           NONE => V.update(TDL, newVertex, SOME(CVTDL + NVWeight))
          |SOME (x) => if x > (CVTDL + NVWeight) then 
                        V.update(TDL, newVertex, SOME(CVTDL + NVWeight)) else TDL

      (* fun newTDLVals currentVertex, CVTDL, TDL, elements = 
                                          real option V.vector
         where the returned vector has updated the TDL for some 
           vertex's neighbors weights
      *)
      fun newTDLVals (currentVertex : vertex, CVTDL : real, 
     TDL : real option V.vector, elements) : real option V.vector list =
        case elements of
          [] => []
          |y::ys => newTDLVal (currentVertex, CVTDL, TDL, y)::
                    newTDLVals (currentVertex, CVTDL, TDL, ys)
(*      Path2 (g, v, pq, vislist, TDL) = = [], if there is no path from u to v in g
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
      fun Path2 (g : graph, v : vertex, PriQue : pq, VisList : bool V.vector,
                   TDL : real option V.vector) : 'a list = 
        let 
          val u = front(PriQue, VisList)
          val TDL' = V.fromList(newTDLVals(u, valOf(V.sub(TDL, u)), TDL, V.sub(g, u)))
        in
          if u = v then V.sub(TDL, u) else
            Dist2(g, v, createPQ(#2(PQ.delMin(PriQue)), u, TDL), 
                    V.update (VisList, u, true), TDL)
        end
    in
      Path2(g, v, [(u, 0.0)], VisList, TDL)
      handle Subscript => raise BadVertex

    end
*)

  (*  toString(g) = a string representation of g.
  *   This representation has the form
  *     0: [(v_{00}, w_{00}),...]
  *     1: [(v_{10}, w_{10}),...]
  *     ...
  *     n-1: [(v_{n-1,0}, w_{n-1,0}),...]
  *   where (v, w) = (v_{ij}, w_{ij}) if an only if (v, w) is a member
  *   of g[i].
  *
  *   In other words, the string representation consists of string 
  *   representations of each adjacency list.
  *
  *   Note that the newlines above are replaced with semicolons, and
  *   the entire representation is enclosed in braces.
  *)
  fun toString(g : graph) : string =
  let
    fun vwToString (v : vertex, w : real) : string =
      String.concat ["(", Int.toString v, ", ", Real.toString w, ")"]

    val edges : string list = V.foldri
      (fn (i, js, a) => 
        (String.concat [Int.toString i, ": ",
        ListFormat.listToString vwToString js]) :: a)
      []
      g
  in
    "{" ^ (String.concatWith "; " edges) ^ "}"
  end

end
