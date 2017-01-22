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
    (#[ ], Mn.empty, Mv.empty)

end

