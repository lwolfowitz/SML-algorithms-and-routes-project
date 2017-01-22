structure Osm = 
struct

  structure G = Hw6Graph
  structure T = TextIO
  structure S = T.StreamIO

  exception BadNodeLine of int
  exception BadWayLine of int

  structure Node =
  struct
    type node = IntInf.int
    type ord_key = node
    val compare = IntInf.compare
    val fromString = IntInf.fromString
    val toString = IntInf.toString
  end

  structure Vertex =
  struct
    type vertex = G.vertex
    type ord_key = vertex
    val compare = Int.compare
  end

  (*  Maps with node keys.
  *)
  structure Mn = SplayMapFn(Node)

  (*  Maps with vertex keys.
  *)
  structure Mv = SplayMapFn(Vertex)

  (*  The type of an OSM node id.
  *)
  type node = Node.node

  (*  The type of an OSM way.
  *)
  type way = string

  (*  The type of a mapping from nodes to vertices.
  *)
  type nvmap = G.vertex Mn.map

  (*  The type of a mapping from vertices to nodes.
  *)
  type vnmap = node Mv.map

  (*  The type of a street map.  The value (g, nv, vn) represents the
  *   following street map:
  *     - g is an undirected graph with vertices 0,...,n-1;
  *     - nv[id] is the vertex corresponding to node id;
  *     - vn[u] is the node_id corresponding to vertex u.
  *     - There is an edge between u and v if nodes vn[u] and vn[v]
  *       are adjacent nodes on some way in the map.
  *)
  type osm = G.graph*nvmap*vnmap

  (*  fromCsv(osmbasname) = (g, nv, vn), where (g, nv, vn) represents
  *   the street map that is represented by the nodes and ways in
  *   basename ^ ".osm.nodes" and basename ^ ".osm.ways".
  *)
  fun fromCsv(osmbasename : string) : osm = 
    let 
    (* val onlyNums string -> string list, where
    the string list is the substrings broken by the "\t" char *)
      val onlyNums = String.tokens (fn x => x = #"\t") 
(* fun MakeMaps S.instream, vnmap, nvmap, int -> vnmap', nvmap', int', where
vnmap' and nvmap' are the complete maps of the vectors and nodes of the nodes file,
and int' is the total number of nodes *)
      fun MakeMaps(nodes: S.instream, vn : vnmap, nv : nvmap, 
                              n : int) : vnmap*nvmap*int=
        case S.inputLine(nodes) of
             NONE => (vn, nv, n)
            |SOME(line, ins') => 
                  let
                    val vn' = Mv.insert(vn,n,valOf(Node.fromString(List.nth(onlyNums(line), 1))))
                    val nv' = Mn.insert(nv,valOf(Node.fromString(List.nth(onlyNums(line),1))),n)
                   in
                  MakeMaps(ins',vn',nv',n+1)     
                   end
(*addListElementsToGraph string list, G.graph, nvmap -> G.graph', where
G.graph' is the undirected graph representing the desired street map, with the data from 
a single way added *)
      fun addListElementsToGraph (waysList : string list, G : G.graph, nv : nvmap ) : G.graph  =
        case waysList of 
          [] => G
          |x::[] => G
          |x::y::ys => 
            let
              val x' = valOf(Mn.find(nv, valOf(Node.fromString(x))))
              val y' = valOf(Mn.find(nv, valOf(Node.fromString(y))))
              val G' = Vector.update(G, x', y'::Vector.sub(G, x'))
            in
              addListElementsToGraph(y::ys,
                Vector.update(G', y', x'::Vector.sub(G', y')), nv)
            end
(*makeGraph S.instream, nvmap, G.graph -> G.graph' returns the completed G.graph for the 
desired street map*)
     fun makeGraph (ways : S.instream , thenvmap : nvmap , G : G.graph) : G.graph =
        case S.inputLine(ways) of
             NONE => G
            |SOME(line, ins') => 
                makeGraph(ins',thenvmap,
                  addListElementsToGraph(
                    List.drop(onlyNums(line), 2), G, thenvmap))
(*orgResults string -> osm, where
osm is the desired result of fromCsv, in other words,
orgResults compiles the valuable results from other functions and returns them*)
      fun orgResults (osmbasename2 : string) : osm =
        let 
          val (vnmapResult, nvmapResult, graphLength) = 
            MakeMaps((T.getInstream(T.openIn (osmbasename2 ^ ".osm.nodes"))), Mv.empty, Mn.empty, 0)
          val graphResult = makeGraph (T.getInstream(T.openIn (osmbasename2 ^ ".osm.ways")), nvmapResult, 
                  Vector.tabulate(graphLength, fn x => []))
        in
          (graphResult, nvmapResult, vnmapResult)
        end
  in
    orgResults (osmbasename)
  end
end