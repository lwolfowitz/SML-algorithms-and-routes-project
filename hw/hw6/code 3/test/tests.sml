(*  Tests for COMP 212 Homework 6: Street maps.
*
*   N. Danner
*)

structure Tests =
struct

  structure T = TextIO
  structure S = T.StreamIO

  structure U = UnitTest
  structure TR = TestRunner

  structure G = Hw6Graph

  (*  sortv vs = a sorted permutation of vs.
  *)
  val sortv : G.vertex list -> G.vertex list = ListMergeSort.sort op>=

  (*  toList #[u_0,...,u_{n-1}] = [u_0,...,u_{n-1}].
  *)
  fun vectorToList (v : 'a vector) : 'a list = Vector.foldr op:: [] v

  val graphToString : G.graph -> string =
    (ListFormat.fmt {init="#[", sep=",", final="]",
      fmt=ListFormat.listToString Int.toString}) o vectorToList

  type node_id = IntInf.int
  type node = node_id*real*real
  type way = string*(node_id list)


  (*  testStreetMapGraph(name, ns, ws) = t, where t is a test that succeeds
  *   if Osm.fromCsv produces a correct osm value from ns and ws.
  *
  *   Roughly, this function does the following:
  *     - Creates a filename f.
  *     - Creates the file f.osm.nodes with one line per value in ns.
  *     - Creates the file f.osm.ways with one line per value in ws.
  *     - Sets (g, nv, vn) = Osm.fromCsv(f).
  *     - Verifies that g has the right size.  If not, reports that g has
  *       the wrong size, and the test fails.
  *     - Verifies that the keys (i.e., domain) of nv is exactly the
  *       node identifiers in ns.  If not, reports the extra or missing
  *       node identifiers, and the test fails.
  *     - Verifies that the edges in g are exactly the expected edges
  *       corresponding to the way definitions in ws.  If not, reports
  *       the extra or missing edges, and the test fails.
  *     - If all of these subtests succeed, then the test succeeds.
  *)
  fun testStreetMapGraph(name : string,
      nodes : node list, ways : way list) : U.test =
  let
    (*  outputNode(outs, n) = ().
    *   As a side-effect, a string representation of n is written to
    *   outs.  The representation is the tab-separated value format
    *   used in .node files.
    *)
    fun outputNode(outs : S.outstream, (id, lon, lat) : node) : unit =
      S.output(outs,
        (String.concatWith "\t" 
          ["node", IntInf.toString id, Real.toString lon, Real.toString lat]) ^
        "\n")

    fun outputWay(outs : S.outstream, (name, ns) : way) : unit =
      S.output(outs,
        (String.concatWith "\t" 
          ["way", name,
            ListFormat.fmt 
              {init="", sep="\t", final="", fmt=IntInf.toString} ns]) ^
        "\n")

    (*  outputMany(outs, [x_0,...,x_{k-1}], output) = ().
    *   As a side-effect, string representations of x_0,...,x_{k-1}
    *   are written to outs in order; the string representation is
    *   output(outs, x_i).
    *)
    fun outputMany(outs : S.outstream, xs : 'a list, 
        output : (S.outstream * 'a) -> unit) : unit =
      foldl (fn (x, _) => output(outs, x)) () xs

    (*  outputManyToFile(filename, [x_0,...,x_{n-1}], output) = ()
    *   As a side effect, the file filename is written with the i-th
    *   line output(x_i).
    *)
    fun outputManyToFile(filename : string, xs : 'a list, 
        output : (S.outstream * 'a) -> unit) : unit =
    let
      val outs = T.getOutstream(T.openOut(filename))
      val () = outputMany(outs, xs, output)
      val () = S.closeOut(outs)
    in
      ()
    end

    (*  outputNodesToFile(basename, ns) = ()
    *   As a side effect, the nodes file basename.osm.nodes is written
    *   with the nodes of ns as data.
    *)
    fun outputNodesToFile(basename : string, ns : node list) : unit =
    let
      val filename = basename ^ ".osm.nodes"
    in
      outputManyToFile(filename, ns, outputNode)
    end

    (*  outputWaysToFile(basename, ws) = ()
    *   As a side effect, the ways file basename.osm.ways is written
    *   with the ways of ws as data.
    *)
    fun outputWaysToFile(basename : string, ws : way list) : unit =
    let
      val filename = basename ^ ".osm.ways"
    in
      outputManyToFile(filename, ws, outputWay)
    end

    fun mem (xs : ''a list) (x : ''a) = List.exists (fn y => x = y) xs

    (*  edges = list of node_id*node_id pairs corresponding to edges
    *   that we should find in the street map graph.
    *)
    val edges : (node_id*node_id) list =
    let
      fun edgesFromNodeList(ns : node_id list) : (node_id*node_id) list =
        case ns of
             [] => []
           | [n] => []
           | n0 :: n1 :: ns =>
               (n0, n1) :: (n1, n0) :: (edgesFromNodeList (n1 :: ns))

      val wayNodeLists : node_id list list = map #2 ways
    in
      foldr (fn (ns, es) => (edgesFromNodeList ns) @ es) [] wayNodeLists
    end

    fun checkRightSize((g, _, _) : Osm.osm) : bool*string =
    let
      val n = Vector.length g
      val m = length nodes
    in
      case n = m of
           true => (true, "")
         | false => 
             (false, 
               String.concat [
                 "g has wrong number of vertices: ",
                 Int.toString n, "; expected ", Int.toString m])
    end

    fun checkNVDomain(nv : G.vertex Osm.Mn.map) =
    let
      (*  nvNodes = the list of keys (node ids) in nv.
      *)
      val nvNodes : node_id list = map (#1) (Osm.Mn.listItemsi nv)

      (*  node_ids = the list of node ids of nodes in nodes.
      *)
      val node_ids : node_id list = map (#1) nodes

      (*  extraNodes = the list of node ids that are keys in nv that are
      *   not in nodes.
      *)
      val extraNodes : node_id list = List.filter (not o (mem node_ids)) nvNodes

      (*  missingNodes = the list of node ids that are in nodes but are
      *   not keys in nv.
      *)
      val missingNodes : node_id list = 
        List.filter (not o (mem nvNodes)) node_ids
    in
      case (extraNodes, missingNodes) of
           ([], []) => (true, "")
         | ([], _) =>
             (false,
             String.concat ["nv missing nodes ",
               ListFormat.listToString (IntInf.toString) missingNodes])
         | (_, _) =>
             (false,
             String.concat ["nv has extra nodes ",
               ListFormat.listToString (IntInf.toString) extraNodes])
    end

    fun checkEdges((g, nv, vn) : Osm.osm) : bool*string =
    let
      fun ge((u0, v0) : G.vertex*G.vertex, (u1, v1) : G.vertex*G.vertex) 
          : bool =
        u0 > u1 orelse (u0 = u1 andalso v0 >= v1)

      val expEdges : (G.vertex*G.vertex) list =
        ListMergeSort.sort ge
        (
        map 
          (fn (n0, n1) => (valOf(Osm.Mn.find(nv, n0)), valOf(Osm.Mn.find(nv, n1))))
          edges
        )

      val actEdges : (G.vertex*G.vertex) list =
      let
        fun edgesFromAdjList(u : G.vertex) : (G.vertex*G.vertex) list =
          foldr (fn (v, r) => (u, v) :: r) [] (Vector.sub(g, u))
      in
        ListMergeSort.sort ge
          (List.concat 
            (List.tabulate(Vector.length g, fn u => edgesFromAdjList u)))
      end

      val missingEdges : (G.vertex*G.vertex) list = 
        List.filter (not o (mem actEdges)) expEdges

      val extraEdges : (G.vertex*G.vertex) list = 
        List.filter (not o (mem expEdges)) actEdges

      val extraEdgesAsNodes : (node_id*node_id) list =
        map 
          (fn (u, v) => (valOf(Osm.Mv.find(vn, u)), valOf(Osm.Mv.find(vn, v))))
          extraEdges

      val missingEdgesAsNodes : (node_id*node_id) list =
        map 
          (fn (u, v) => (valOf(Osm.Mv.find(vn, u)), valOf(Osm.Mv.find(vn, v))))
          missingEdges

      fun edgeToString (u : G.vertex, v : G.vertex) : string =
        String.concat ["(", Int.toString u, ", ", Int.toString v, ")"]

      fun npToString (n0 : node_id, n1 : node_id) : string =
        String.concat ["(", IntInf.toString n0, ", ", IntInf.toString n1, ")"]
    in
      case (extraEdges, missingEdges) of
           ([], []) => (true, "")
         | ([], _) =>
             (false,
             String.concat ["g missing edges ",
               ListFormat.listToString edgeToString missingEdges,
               " (as nodes: ",
               ListFormat.listToString npToString missingEdgesAsNodes,
               ")"])
         | (_, _) =>
             (false,
             String.concat ["g has extra edges ",
               ListFormat.listToString edgeToString extraEdges,
               " (as nodes: ",
               ListFormat.listToString npToString extraEdgesAsNodes,
               ")"])
    end

    infix andthen
    fun op andthen(t0 : unit -> bool*string, t1 : unit -> bool*string)
        : bool*string =
      case t0() of
           (true, _) => t1()
         | (false, msg) => (false, msg)

    fun test() : bool*string =
    let
      val basename : string = OS.FileSys.tmpName()
      val () = outputNodesToFile(basename, nodes)
      val () = outputWaysToFile(basename, ways)
      val (g, nv, vn) : Osm.osm = Osm.fromCsv(basename)
    in
      (fn () => checkRightSize(g, nv, vn))
      andthen
      (fn () =>
        (fn () => checkNVDomain(nv)) 
        andthen 
        (fn () => checkEdges(g, nv, vn))
      )
    end
  in
    U.assertTrueMsg(name, test)
  end

  val nodes1 : node list = [
    (178491208,41.5437296,~72.6727951),
    (2521355230,41.5484069,~72.6651904),
    (2677763939,41.5319538,~72.6547506),
    (2954479220,41.5620124,~72.6519010),
    (2954479221,41.5620806,~72.6517156),
    (2955258470,41.5598994,~72.6475472),
    (178479314,41.5316093,~72.6546047),
    (178484648,41.5463540,~72.6576113),
    (178485712,41.5587909,~72.6599085),
    (178491020,41.5565287,~72.6521649)
  ]

  val ways1 : way list = [
    ("A street", [2677763939, 2955258470, 178485712, 178491208]),
    ("B street", [2521355230, 2677763939, 2954479221]),
    ("C street", [178491020]),
    ("D street", [2677763939, 178479314, 178485712, 2954479220, 2954479221])
  ]

  (*  See the documentation for testStreetMapGraph above.
  *)
  val fromCsvTests = ("fromCsv tests", [
    testStreetMapGraph("test0", nodes1, []),
    testStreetMapGraph("test1", nodes1, ways1)
  ])

  val allTests = [
    fromCsvTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
