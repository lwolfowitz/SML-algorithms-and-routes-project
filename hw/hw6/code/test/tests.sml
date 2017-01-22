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


  (*  testStreetMapGraph(name) = t, where t is a test that succeeds
  *   if Osm.fromCsv(name) produces a correct osm value.
  *
  *   Roughly, this function does the following:
  *     - Sets (g, nv, vn) = Osm.fromCsv("testfiles/" ^ name).
  *     - Reads the file "testfiles/" ^ name ^ ".edg".  That file has the format
  *         node_0
  *         node_1
  *         ...
  *         node_{k-1}
  *         --
  *         edge_0
  *         edge_1
  *         ...
  *         edge_{m-1}
  *       This file is essentially an edge-list representation of the
  *       value that Osm.fromCsv(name) should return.  Specifically:
  *         - g should have k vertices.
  *         - The domain of nv should be {node_0,...,n_{k-1}} and
  *           the range of nv should be {0,...,k-1}.
  *         - The domain of vn should be {0,...,k-1} and the range
  *           of vn should be {node_0,...,node_{k-1}}.
  *         - nv and vn should match up, in that if Vector.sub(nv, n) = v,
  *           then Vector.sub(vn, v) = n.  So nv and vn let us translate
  *           between node identifiers and graph vertices.
  *         - The edges of g should be precisely the set of pairs
  *           (u, v), where u = Vector.sub(nv, node_i) and
  *           v = Vector.sub(nv, node_j) for some i and j.  Note
  *           that these are undirected edges!
  *     - If (g, nv, vn) satisfies al of the conditions just listed,
  *       then the test succeeds; otherwise it fails.
  *)
  fun testStreetMapGraph(name : string) : U.test =
  let

    fun mem (xs : ''a list) (x : ''a) = List.exists (fn y => x = y) xs

    fun checkRightSize(g, nv, vn, nodes, edges) : bool*string =
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

    fun checkNVDomain(nv : G.vertex Osm.Mn.map, nodes, edges) =
    let
      (*  nvNodes = the list of keys (node ids) in nv.
      *)
      val nvNodes : node_id list = map (#1) (Osm.Mn.listItemsi nv)

      (*  extraNodes = the list of node ids that are keys in nv that are
      *   not in nodes.
      *)
      val extraNodes : node_id list = List.filter (not o (mem nodes)) nvNodes

      (*  missingNodes = the list of node ids that are in nodes but are
      *   not keys in nv.
      *)
      val missingNodes : node_id list = List.filter (not o (mem nvNodes)) nodes
    in
      case (extraNodes, missingNodes) of
           ([], []) => (true, "")
         | ([], _) =>
             (false,
             String.concat ["nv domain missing nodes ",
               ListFormat.listToString (IntInf.toString) missingNodes])
         | (_, _) =>
             (false,
             String.concat ["nv domain has extra nodes ",
               ListFormat.listToString (IntInf.toString) extraNodes])
    end

    fun checkNVRange(g, nv, _, nodes, edges) : bool*string =
    let
      val nvValues : int list = 
        ListMergeSort.sort op>= (map (#2) (Osm.Mn.listItemsi nv))
    in
      case nvValues = List.tabulate(Vector.length g, fn i => i) of
           true => (true, "")
         | false => 
             (false,
             String.concat ["nv range wrong: ",
               ListFormat.listToString Int.toString nvValues])
    end

    fun checkVNDomain(g, nv, vn, nodes, edges) : bool*string =
    let
      val vnKeys : int list = 
        ListMergeSort.sort op>= (map (#1) (Osm.Mv.listItemsi vn))
    in
      case vnKeys = List.tabulate(Vector.length g, fn i => i) of
           true => (true, "")
         | false => 
             (false,
             String.concat ["vn domain wrong: ",
               ListFormat.listToString Int.toString vnKeys])
    end

    fun checkVNRange(vn : node_id Osm.Mv.map, nodes, edges) =
    let
      (*  vnNodes = the list of keys (node ids) in vn.
      *)
      val vnNodes : node_id list = map (#2) (Osm.Mv.listItemsi vn)

      (*  extraNodes = the list of node ids that are keys in vn that are
      *   not in nodes.
      *)
      val extraNodes : node_id list = List.filter (not o (mem nodes)) vnNodes

      (*  missingNodes = the list of node ids that are in nodes but are
      *   not keys in vn.
      *)
      val missingNodes : node_id list = List.filter (not o (mem vnNodes)) nodes
    in
      case (extraNodes, missingNodes) of
           ([], []) => (true, "")
         | ([], _) =>
             (false,
             String.concat ["vn range missing nodes ",
               ListFormat.listToString (IntInf.toString) missingNodes])
         | (_, _) =>
             (false,
             String.concat ["vn range has extra nodes ",
               ListFormat.listToString (IntInf.toString) extraNodes])
    end

    fun checkNVCompVN(nv : G.vertex Osm.Mn.map, vn : node_id Osm.Mv.map, nodes,
      edges) =
    let
      val m = Osm.Mn.mapPartial (fn v => Osm.Mv.find(vn, v)) nv
      val badN = List.find (fn n => n <> valOf(Osm.Mn.find(m, n))) nodes
    in
      case badN of
           NONE => (true, "")
         | SOME n =>
           let
             val v : G.vertex = valOf(Osm.Mn.find(nv, n))
           in
             (false,
             String.concat ["nv[", IntInf.toString n, "] =",
               Int.toString v, " but vn[", Int.toString v, "] = ",
               IntInf.toString(valOf(Osm.Mv.find(vn, v)))])
           end
    end

    fun checkEdges(g, nv, vn, nodes, edges) : bool*string =
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
        : unit -> bool*string =
      case t0() of
           (true, _) => t1
         | (false, msg) => raise Fail msg

    fun test() : bool*string =
    let
      val basename = "testfiles/" ^ name

      (*  ns = the list of node_ids in basename.edg
      *   es = the list of edges (node_id*node_id pairs) in basename.edg.
      *
      *   ns and es are the expected node_ids and edges for this test.
      *)
      val (nodes, edges) : (node_id list)*((node_id*node_id) list) =
      let
        fun readNodes(ins : S.instream) : (node_id list)*(S.instream) =
          case S.inputLine(ins) of
               NONE => raise Fail ""
             | SOME(line, ins') =>
                 if line = "--\n" then ([], ins')
                 else
                 let
                   val (ns, ins'') = readNodes ins'
                 in
                   (valOf(IntInf.fromString line) :: ns, ins'')
                   handle Option =>
                     raise Fail ("Failed to parse " ^ line)
                 end

        fun readEdges(ins : S.instream) : ((node_id*node_id) list)*S.instream =
          case S.inputLine ins of
               NONE => ([], ins)
             | SOME("\n", ins') => readEdges(ins')
             | SOME(line, ins') =>
               let
                 val (es, ins'') = readEdges ins'
                 val [n0, n1] = 
                   map (valOf o IntInf.fromString) 
                     (String.tokens (Char.isSpace) line)
                   handle Option =>
                     raise Fail ("Failed to parse " ^ line)
               in
                 ((n0, n1) :: (n1, n0) :: es, ins'')
               end

        val ins = T.getInstream(T.openIn(basename ^ ".edg"))
        val (ns, ins') = readNodes(ins)
        val (es, ins'') = readEdges(ins')
        val () = S.closeIn ins

      in
        (ns, es)
      end

      val (g, nv, vn) : Osm.osm = Osm.fromCsv(basename)
    in
      (
      (fn () => checkRightSize(g, nv, vn, nodes, edges))
      andthen
      (fn () => checkNVDomain(nv, nodes, edges))
      andthen
      (fn () => checkNVRange(g, nv, vn, nodes, edges))
      andthen
      (fn () => checkVNDomain(g, nv, vn, nodes, edges))
      andthen
      (fn () => checkVNRange(vn, nodes, edges))
      andthen
      (fn () => checkNVCompVN(nv, vn, nodes, edges))
      andthen
      (fn () => checkEdges(g, nv, vn, nodes, edges))
      ) ()
      handle Fail msg => (false, msg)

    end
  in
    U.assertTrueMsg(name, test)
  end

  (*  See the documentation for testStreetMapGraph above.
  *)
  val fromCsvTests = ("fromCsv tests", [
    testStreetMapGraph("test2"),
    testStreetMapGraph("test3")
  ])

  val allTests = [
    fromCsvTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
