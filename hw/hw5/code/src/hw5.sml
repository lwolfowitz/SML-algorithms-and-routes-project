(*  COMP 212 Homework 5:  Graphs and depth-first search.
*   
*   N. Danner
* By Lee Wolfowitz
*
*   Throughout the documentation of this structure, if x : 'a vector,
*   then we write x[i] for Vector.sub(x, i).
*)

structure Hw5Graph =
struct

  structure T = TextIO
  structure S = T.StreamIO

  (*  The type of a vertex.
  *)
  type vertex = int

  (*  The type of a graph.  If g : graph, then g represents the graph (V, E),
  *   where
  *     - V = {0,...,Vector.length g}
  *     - E = {(u, v) : v is a member of g[u]}.
  *)
  type graph = vertex list vector

   (*  toList #[u_0,...,u_{n-1}] = [u_0,...,u_{n-1}].
  *)
  fun vectorToList (v : 'a vector) : 'a list = Vector.foldr op:: [] v

  (*  graphToString g = a string representation of the graph g.
  *)
  val graphToString : graph -> string =
    (ListFormat.fmt {init="#[", sep=",", final="]",
      fmt=ListFormat.listToString Int.toString}) o vectorToList

  (*  fromEdgeList(n, es) =
  *     #[ [v_{00},...,v_{0,m_0}],...,[v_{n-1,0},...,v_{n-1,m_{n-1}] ], where
  *     - For each v_{i,j}, (i, v_{ij}) is an element of es;
  *     - If (u, v) is an element of es, then there is j such that
  *       v = v_{u, j};
  *     - If j <> j', then v_{i,j} <> v_{i, j'}.
  *   In other words, (n, es) is an edge-list representation of some graph g,
  *   and fromEdgeList(n, es) is the adjacency-list representation of g.
  *   Note that edges may be repeated in es.
  *)
  fun fromEdgeList(n : int, es : (vertex*vertex) list) : graph =
     let
    (*  addVertex([v_0,...,v_{n-1}], w) = [v_0,...,v_{n-1},w], w <> v_i, any i
    *                                   = [v_0,...,v_{n-1}], w = v_i, some i.
    *)
    fun addVertex(vs : vertex list, w : vertex) : vertex list =
      case vs of
           [] => [w]
         | v' :: vs' => if w = v' then vs else v' :: addVertex (vs', w)

    (*  addEdges([[vs_0,...,vs_{n-1}]], u, v) =
    *     [[vs_0,...,addVertex(vs_u, v),...,addVertex(vs_v, u),...,vs_{n-1}]].
    *)
    fun addEdge(g : graph, (u, v) : vertex*vertex) : graph =
      Vector.update(g, u, addVertex(Vector.sub(g, u), v))

    fun addEdges(g : graph, es : (vertex*vertex) list) : graph =
      case es of
           [] => g
         | (u, v) :: es' => 
             addEdges(Vector.update(g, u, addVertex(Vector.sub(g, u), v)), es')
  in
    addEdges(Vector.tabulate(n, fn i => []), es)
  end

  (*  fromEdgeListFile(filename) = fromEdgeList(n, es), where
  *   filename is a text file of the form
  *     n
  *     u_0 v_0
  *     u_1 v_1
  *     ...
  *     u_{m-1} v_{m-1}
  *   That is, the first line of the file consists of a single non-negative
  *   integer n.  All remaining lines consist of two non-negative integers
  *   separated by whitespace, both of which are < n.  es is
  *   [(u_0, v_0),...,(u_{m-1}, v_{m-1})].
  *
  *   Note that the number of edges is usually not the same as the
  *   number of vertices!
  *)
  fun fromEdgeListFile(filename : string) : graph =
  let
    val ins : S.instream = T.getInstream(T.openIn filename)

    (*  fromString s = n, where n is the int represented by s.
    *     Raises Option of s does not represent any integer.
    *)
    val fromString : string -> int = valOf o Int.fromString

    (*  If ins = <c_0,...,c_{n-1},#"\n",c_{n+1}...> where 
    *   implode[c_0,...,c_{n-1}] represents an integer, then
    *     - n = fromString(implode[c_0,...,c_{n-1}])
    *     - ins = <c_{n+1},...>
    *)
    val (n, ins) : int*S.instream = 
      let
        val SOME(s, ins) = S.inputLine(ins)
      in
        (fromString s, ins)
      end
    fun edges(ins : S.instream) : (vertex*vertex) list =
      case S.inputLine(ins) of
           NONE => []
         | SOME(s, ins') =>
           let
             val [s0, s1] = String.tokens Char.isSpace s
           in
             (fromString s0, fromString s1) :: edges(ins')
           end
  in
    fromEdgeList(n, edges(ins))
  end

  (*  isAcyclic(g) = true if g represents an acyclic graph,
  *                  false otherwise.
  *)
  fun isAcyclic(g : graph) : bool =
    let
    (*  findSink (g, u) = SOME(v) if g[v] = [] and u <= v.
    *                   = NONE, o/w.
    *)
    fun findSink(g : graph, u : vertex) : vertex option =
      if u = Vector.length g then NONE
      else
        case Vector.sub(g, u) of
             [] => SOME u
           | _ => findSink(g, u + 1)

    (*  removeVertex(g, u) = g', where
    *     - g'[v] = g[v] - {u}, v < u
    *     - g'[v] = g[v+1] - {u}, v >= u.
    *   In other words, g' is the graph that results from deleting
    *   u from g.
    *)
    fun removeVertex(g : graph, u : vertex) : graph =
    let

      (*  Note that removeFromList and shift are not used directly.
      *   They are part of my initial design of this function.  Once
      *   I had written them and implemented everything using them,
      *   I realized I could improve the efficiency a bit by combining
      *   them into a single function (removeAndShift).  I've left them
      *   here so you can see part of my design process.
      *)

      (*  removeFromList [v_0,...,v_{n-1}] 
      *     = [v_0,...,v_{n-1}], u <> v_i, any i,
      *     = [v_0,...,v_{i-1},v_{i+1},...,v_{n-1}], u = v_i.
      *)
      fun removeFromList (vs : vertex list) : vertex list =
        case vs of
             [] => []
           | v' :: vs' => if u = v' then vs' else v' :: removeFromList vs'

      (*  shift [v_0,...,v_{n-1}] = [v_0',...,v_{n-1}'], where
      *     - v_i' = v_i if v_i < u
      *            = v_i + 1 if v_i >= u.
      *)
      fun shift (vs : vertex list) : vertex list =
        case vs of
             [] => []
           | v' :: vs' => if v' >= u then (v'-1) :: shift vs'
                          else v' :: shift vs'

      (*  removeAndShift vs = shift(removeFromList vs).
      *)
      fun removeAndShift (vs : vertex list) : vertex list =
        case vs of
             [] => []
           | v' :: vs' =>
               if v' < u then v' :: removeAndShift vs'
               else if v' = u then removeAndShift vs'
               else (v'-1) :: removeAndShift vs'
    in
      Vector.tabulate(Vector.length g - 1,
        fn i => if i < u then removeAndShift(Vector.sub(g, i))
                else removeAndShift(Vector.sub(g, i+1)))
    end

    (*  removeSinks g = g', where
    *   - g_0 = g
    *   - g_n = removeVertex(g_{n-1}, v), if findSink(g_{n-1}) = SOME v,
    *         = g_{n-1}, o/w.
    *   - g' = g_n, where n is the smallest value such that g_n = g_{n-1}.
    *)
    fun removeSinks(g : graph) : graph =
      case findSink (g, 0) of
           SOME u => removeSinks(removeVertex(g, u))
         | NONE => g
  in
    Vector.length(removeSinks g) = 0
  end
      

  (*  topSort g = [u_0,...,u_{n-1}], where
  *     - {u_0,...,u_{n-1}} is the set of vertices of g; and
  *     - if there is a path from u_i to u_j in g, then i < j.
  *   In other words, topSort g is a listing vs of the vertices in g
  *   such that if there is a path from u to v in g, then u occurs in vs
  *   before v.
  *
  *   Pre-condition:  isAcyclic g.
  *
  *   We phrase this informally by writing topSort g = us, where us is
  *   a topological sort of the vertices of g.
  *)
  fun topSort(g : graph) : vertex list =
    let 
      val length = Vector.length(g)

(* findEmptyDFS (g, u) = v where
*   - Vector.sub(g, v) = []
*   - hasPath(g, u, v) = true
*)
      fun findEmptyDFS (g : graph, u : vertex) : vertex =
        case Vector.sub(g, u) of 
          [] => u
          |e::es => findEmptyDFS(g, e)
(* isIn (vl, u) = vl' where
*   - if vl contains u then vl' = vl - u
*   - o/w vl' = vl
*)
      fun isIn (vl : vertex list, u : vertex ) : vertex list = 
        case vl of 
          [] => []
          |e::es => if u = e then es else e::isIn(es, u)

(* removeFromGraph (g, u) = g' where
*   - for all g[v], Vector.sub(g, v) may contain u
*   - for all g'[v], Vector.sub(g', v) does not contain u
*)
      fun removeFromGraph (g : graph, u : vertex) : graph =
        Vector.tabulate(length, fn i => isIn(Vector.sub(g, i), u))
(* falseElementsNum (bl, i) = v where
*   - v is the lowest list index where Vector.sub(bl, v) is false
*   - if all are true then return the Vector.length(bl)
*)
      fun falseElementNum (bl : bool vector, i : int) : vertex =
        if i = length then length else
          case Vector.sub(bl, i) of 
          true => falseElementNum(bl, i+1)
          |false => i

  (*  topSort2 g, rl, bl = [u_0,...,u_{n-1}], where
  *     - {u_0,...,u_{n-1}} is the set of vertices of g; and
  *     - if there is a path from u_i to u_j in g, then i < j.
  *   In other words, topSort2 g, rl, bl is a listing vs of the vertices in g
  *   such that if there is a path from u to v in g, then u occurs in vs
  *   before v.
  *
  *   Pre-condition:  isAcyclic g.
  *
  *   We phrase this informally by writing topSort2 g, rl, bl = us, where us is
  *   a topological sort of the vertices of g.
  *)

      fun TopSort2 (g : graph, rl : vertex list, bl : bool vector) : vertex list =
        let
          val lowestFalse = falseElementNum(bl, 0)
        in
          if lowestFalse = length then rl else 
          TopSort2(removeFromGraph(g, findEmptyDFS(g, lowestFalse)),
           findEmptyDFS(g, lowestFalse)::rl, 
           Vector.update(bl, findEmptyDFS(g, lowestFalse), true))
        end

    in
      TopSort2(g, [], Vector.tabulate(length, fn i => false))
    end

end
