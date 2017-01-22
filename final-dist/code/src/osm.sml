structure Osm = 
struct

  structure G = WeightedGraphAdjList
  structure S = TextIO.StreamIO

  exception NoSuchAttr
  exception NoIntersection of string*string
  exception BadNodeLine of int
  exception BadWayLine of int

  structure Node =
  struct
    type node_id = IntInf.int
    type node = node_id*real*real
    type ord_key = node_id
    val compare = IntInf.compare
    val fromString = IntInf.fromString
    val toString : node -> string = IntInf.toString o #1
  end

  structure Way =
  struct
    type way = string
    type ord_key = way
    val compare = String.compare
  end

  structure Vertex =
  struct
    type ord_key = int
    val compare = Int.compare
  end

  (*  Maps with node keys.
  *)
  structure Mn = SplayMapFn(Node)

  (*  Maps with vertex keys.
  *)
  structure Mv = SplayMapFn(Vertex)

  (*  Maps with way keys.
  *)
  structure Mw = SplayMapFn(Way)

  (*  The type of an OSM node id.
  *)
  type node = Node.node_id

  (*  The type of an OSM way.
  *)
  type way = Way.way

  type ncmap = (real*real) Mn.map

  type osm = {
    (*  Graph representation of the data.  g = (V, E), where:
    *     V = {0,...,n-1}, n = number of nodes in the osm data.
    *     (i, j) is an edge if there is a way in the osm data
    *       with nodes ...,n0,n1,... and (nv[n0], nv[n1]) = (i, j)
    *       or (nv[n1], nv[n0]) = (i, j).
    *)
    g : G.graph,
    (*  nv and vn map nodes to vertices and vertices to nodes,
    *     respectively, so that nv o vn = id and vn o nv = id.
    *)
    nv : G.vertex Mn.map,
    vn : node Mv.map,

    nc : ncmap,

    (*  wn[w] is the list of nodes associated with the way w.
    *)
    wn : node list Mw.map,

    (*  nw[n] is the list of ways that include node n (may be empty).
    *)
    nw : way list Mn.map
  }

  fun numNodes ({g : G.graph,...} : osm) : int =
    G.size g

  fun numWays ({wn : node list Mw.map,...} : osm) : int =
    Mw.numItems wn

  fun parseFile (osmfile : string) : XML.tree list =
  let
    val ins = TextIO.getInstream (TextIO.openIn(osmfile))
  in
    XML.parseInstream ins
  end

  fun osm2csv(basename : string) : unit =
  let

    fun getAttr(attrs : XML.attr list, attr : string) : string =
      case attrs of
           [] => raise NoSuchAttr
         | (k, v) :: attrs =>
             if k = attr then v else getAttr (attrs, attr)

    (*  getWayName ns = s, where 
    *     ns = [...,Node(tag, [("k", "name"), ("v", s)], _),...].
    *     If there is no such Node in ns, then s = "Unkonwn".
    *)
    fun getWayName(ns : XML.tree list) : string =
      case ns of
           [] => "Unkonwn"
         | XML.Node("tag", [("k", "name"), ("v", wayName)], _) :: _ => wayName
         | _ :: ns => getWayName ns

    (*  getWayNodes ns = [ndref_0,...,ndref_{n-1}], where
    *     ns = [Node(nd, [("ref", ndref_0)]),
    *           Node(nd, [("ref", ndref_1)]),...
    *           Node(nd, [("ref", ndref_{n-1})])a,...]
    *)
    fun getWayNodes(ns : XML.tree list) : string list =
      case ns of
           [] => []
         | XML.Node("nd", [("ref", refId)], _) :: ns => refId :: (getWayNodes ns)
         | _ :: ns => getWayNodes ns

    val [XML.Node("osm", _, nodesWays)] = 
      parseFile(String.concat [basename, ".osm"])

    val nodeStrm = TextIO.openOut(concat [basename, ".osm.nodes"])
    val wayStrm = TextIO.openOut(concat [basename, ".osm.ways"])

    fun processElement (XML.Node(elt, attrs, children) : XML.tree) =
      case elt of
           "node" =>
             TextIO.output(nodeStrm,
               String.concat [
                 String.concatWith "\t" ["node", getAttr(attrs, "id"),
                   getAttr(attrs, "lat"), getAttr(attrs, "lon")], "\n"])
         | "way" =>
             TextIO.output(wayStrm,
               String.concat [
                 String.concatWith "\t"
                   ["way", getWayName(children), 
                   String.concatWith "\t" (getWayNodes(children))], "\n"])
         | _ => ()

    val _ = map processElement nodesWays

    val _ = TextIO.closeOut(nodeStrm)
    val _ = TextIO.closeOut(wayStrm)
  in
    ()
  end

  fun fromCsv(osmbasename : string) : osm = 
  let

    (*  makeVertices ins = (g, nv, vn), where
    *     ins = <<"node_0,id_0,lat_0,lon_0\n",...,
    *             "node_{n-1},id_{n-1},lat_{n-1},lon_{n-1}\n">> 
    *       (where we think of ins as a stream of lines);
    *     g = Graph.new n;
    *     nv is an injective map from {node_0,...,node_{n-1}} to {0,...,n-1};
    *     vn is an injective map from {0,...,n-1} to {node_0,...,node_{n-1}};
    *     nv o vn = vn o nv = id.
    *)
    fun makeVertices (nodesIns : S.instream) :
        G.graph * G.vertex Mn.map * node Mv.map * ncmap =
    let
      fun makeMaps (ins : S.instream, vnum : int, 
                    nv : G.vertex Mn.map, vn : node Mv.map,
                    nc : ncmap,
                    lineNo : int) : 
          G.vertex Mn.map * node Mv.map * ncmap =
        case S.inputLine ins of
             NONE => (nv, vn, nc)
           | SOME (s, ins') =>
             let
               (*  Remove the trailing newline character from s.
               *)
               val s = String.extract(s, 0, SOME(String.size s - 1))

               val ["node", nodeIdS, latS, lonS] =
                 String.tokens (fn c => c = #"\t") s

               val nodeId = valOf (IntInf.fromString nodeIdS)
               val lat = valOf (Real.fromString latS)
               val lon = valOf (Real.fromString lonS)

               val nv' = Mn.insert(nv, nodeId, vnum)
               val vn' = Mv.insert(vn, vnum, nodeId)
               val nc' = Mn.insert(nc, nodeId, (lat, lon))
             in
               makeMaps(ins', vnum+1, nv', vn', nc', lineNo+1)
             end
         handle Option => raise BadNodeLine lineNo

      val (nv, vn, nc) = makeMaps(nodesIns, 0, Mn.empty, Mv.empty, Mn.empty, 1)
      val n = Mn.numItems nv
    in
      (G.new n, nv, vn, nc)
    end

    fun haversine((lat0, lon0), (lat1, lon1)) =
    let
      fun toRad(d) = d*Math.pi/180.0

      val phi0 = toRad(lat0)
      val phi1 = toRad(lat1)
      val lam0 = toRad(lon0)
      val lam1 = toRad(lon1)

      val b = Math.sin((phi0 - phi1)/2.0)
      val b' = Math.sin((lam0 - lam1)/2.0)
      
      val a = b*b + Math.cos(phi0)*Math.cos(phi1)*b'*b'
      val c = 2.0*Math.atan2(Math.sqrt(a), Math.sqrt(1.0-a))
      val R = 6371.0
      val d = R*c
    in
      d
    end

    (*  makeEdges (ins, g, nv) = g', where g' is obtained from g by
    *     adding an edge of the form nv[n] -> nv[n'] for each
    *     line in ins of the form
    *       way "..." n_0 n_1 ... n n' ...
    *)
    fun makeEdges (waysIns : S.instream, 
                   g : G.graph, 
                   nv : G.vertex Mn.map,
                   nc : ncmap) :
          G.graph * node list Mw.map * way list Mn.map =
    let
      fun edgesFromList (nodeIds : node list, g : G.graph) : G.graph =
        case nodeIds of
             [] => g
           | [_] => g
           | n0 :: n1 :: ns =>
             let
               val (lat0, lon0) = valOf(Mn.find(nc, n0))
               val (lat1, lon1) = valOf(Mn.find(nc, n1))
               val d = haversine((lat0, lon0), (lat1, lon1))
               val g0 =
                 G.addEdge(g, valOf (Mn.find(nv, n0)), 
                   (valOf (Mn.find(nv, n1)), d))
               val g1 =
                 G.addEdge(g0, valOf (Mn.find(nv, n1)), 
                   (valOf (Mn.find(nv, n0)), d))
             in
               edgesFromList (n1 :: ns, g1)
             end

      fun edgesFromIns (ins : S.instream, g : G.graph, wn : node list Mw.map,
                        nw : way list Mn.map, 
                        lineNo : int) : 
          G.graph * node list Mw.map * way list Mn.map =
        case S.inputLine ins of
             NONE => (g, wn, nw)
           | SOME (s, ins') =>
             let
               (*  Remove the trailing newline character from s.
               *)
               val s = String.extract(s, 0, SOME(String.size s - 1))

               val "way" :: wayName :: nodeIdsS =
                 String.tokens (fn c => c = #"\t") s

               (*  If we've already seen this way, then give it a new
               *   name of the form wayName ^ i, where i is the number
               *   of times we've seen it before.
               *)
               (*
               fun patchWayName(w : way, c : int Mw.map) : way * int Mw.map =
                 case Mw.find(wn, wayName) of
                      NONE => (w, c)
                    | SOME _ =>
                      let
                        val n =
                          case Mw.find(c, wayName) of
                               NONE => 1
                             | SOME m => m+1
                      in
                        (String.concatWith " " [wayName, Int.toString n],
                          Mw.insert(c, wayName, n))
                      end
               val (wayName, wcounts') = patchWayName(wayName, wcounts)
               *)

               val nodeIds = map (valOf o IntInf.fromString) nodeIdsS

               val g' = edgesFromList (nodeIds, g)
               (*
               val wn' = Mw.insert(wn, wayName, nodeIds)
               *)
               val wayNodes =
                 valOf(Mw.find(wn, wayName)) handle Option => []
               val wn' = Mw.insert(wn, wayName, nodeIds @ wayNodes)

               fun addNode (n : node, m : way list Mn.map) : way list Mn.map =
                 case Mn.find(m, n) of
                      NONE => Mn.insert(m, n, [wayName])
                    | SOME ws => Mn.insert(m, n, wayName :: ws)

                    (*
               val nw' = foldr 
                 (fn (n, m) => Mn.insert(m, n, wayName :: valOf(Mn.find(m, n)))) nw
                 nodeIds
                 *)
               val nw' = foldr addNode nw nodeIds
             in
               edgesFromIns (ins', g', wn', nw', lineNo+1)
             end
             handle Option => raise BadWayLine lineNo
    in
      edgesFromIns (waysIns, g, Mw.empty, Mn.empty, 1)
    end

    val nodesFile = String.concat [osmbasename, ".osm.nodes"]
    val waysFile = String.concat [osmbasename, ".osm.ways"]

    val nodesIns = TextIO.getInstream (TextIO.openIn nodesFile)
    val waysIns = TextIO.getInstream (TextIO.openIn waysFile)

    (*
    val () = print "makeVertices..."
    *)
    val (g, nodeVtxMap, vtxNodeMap, nodeCoordMap) = makeVertices nodesIns
    (*
    val () = print "makeEdges..."
    *)
    val (g, wayNodesMap, nodeWaysMap) = 
      makeEdges (waysIns, g, nodeVtxMap, nodeCoordMap)
    (*
    val () = print "done.\n"
    *)

    val _ = TextIO.StreamIO.closeIn nodesIns
    val _ = TextIO.StreamIO.closeIn waysIns

  in

    {g=g, nv=nodeVtxMap, vn=vtxNodeMap, nc=nodeCoordMap,
      wn=wayNodesMap, nw=nodeWaysMap}

  end

  fun findRoute({g, nv, vn, nc, wn, nw} : osm,
                startWay0 : way, startWay1 : way,
                endWay0 : way, endWay1 : way) : way list list =
  let
    fun mem (xs : ''a list) (x : ''a) = List.exists (fn y => y = x) xs

    fun intersection(way0 : way, way1 : way) : node =
    let
      val way0Nodes = valOf (Mw.find(wn, way0))
      val way1Nodes = valOf (Mw.find(wn, way1))
    in
      case List.find (mem way0Nodes) way1Nodes of
           NONE => raise NoIntersection(way0, way1)
         | SOME n => n
    end

    val vStart = valOf (Mn.find(nv, intersection(startWay0, startWay1)))
    val vEnd = valOf (Mn.find(nv, intersection(endWay0, endWay1)))

    val vertices : G.vertex list = 
      G.path(g, vStart, vEnd)
    val nodes : node list = map (valOf o (fn v => Mv.find(vn, v))) vertices

    fun nodeListToIntersections(ns : node list) : way list list =
      case ns of
           [] => []
         | n :: ns => (valOf (Mn.find (nw, n))) :: (nodeListToIntersections ns)
    val intersections : way list list = nodeListToIntersections(nodes)
  in
    intersections
  end


end

structure OsmDriver =
struct

  fun convert (args : string list) : int =
  let
    val [basename] = args
  in
    (Osm.osm2csv basename ; 0)
  end

  fun info (args : string list) : int =
  let
    val [basename] = args
    val t = Timer.startCPUTimer()
    val osm = Osm.fromCsv basename
    val {usr, sys} = Timer.checkCPUTimer(t)
    
    val nNodes = Osm.numNodes osm
    val nWays = Osm.numWays osm

    val () = print ("Number of nodes: " ^ Int.toString nNodes ^ ".\n")
    val () = print ("Number of ways: " ^ Int.toString nWays ^ ".\n")
    val () = print ("File load time:  " ^ Time.toString usr ^ " sec.\n")
  in
    0
  end


  fun findroute (args : string list) : int =
  let
    val [basename, start0, start1, end0, end1] = args
    val () = print ("Loading " ^ basename ^ "...")
    val osm = Osm.fromCsv basename
    val () = print ("Finding route...")
    val route = 
      List.filter (fn ws => length ws > 1)
        (Osm.findRoute (osm, start0, start1, end0, end1))
    val () = print ("Done.\n")
    val routeS =
      ListFormat.fmt {
        init="", sep="\n", final="\n",
        fmt=ListFormat.listToString String.toString} route
    val () = print routeS
  in
    0
  end
  handle 
      Osm.BadNodeLine(n) =>
        (print (String.concat ["Bad node line: ", Int.toString n]) ; 1)
    | Osm.BadWayLine(n) =>
        (print (String.concat ["Bad way line: ", Int.toString n]) ; 1)

  structure StringKey : ORD_KEY =
  struct
    type ord_key = string
    val compare = String.compare
  end
  structure M = SplayMapFn(StringKey)

  val handlers : (string list -> int) M.map =
  let
    fun addHandlers (hs : (string * (string list -> int)) list) : 
        (string list -> int) M.map =
      case hs of
           [] => M.empty
         | (cmd, hndlr) :: hs => M.insert(addHandlers(hs), cmd, hndlr)
  in
    addHandlers [
      ("convert", convert),
      ("info", info),
      ("findroute", findroute)
    ]
  end

  fun main(arg0 : string, argv : string list) : int =
  let
    val cmd :: cmdArgs = argv
    val handler = M.find(handlers, cmd)
  in
    case handler of
         NONE => (print ("No command " ^ cmd) ; 1)
       | SOME h => h cmdArgs
  end

end
