(*  Tests for COMP 212 Homework 4: Graphs.
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure G = Hw4Graph

  (*  sortv vs = a sorted permutation of vs.
  *)
  val sortv : G.vertex list -> G.vertex list = ListMergeSort.sort op>=

  (*  toList #[u_0,...,u_{n-1}] = [u_0,...,u_{n-1}].
  *)
  fun vectorToList (v : 'a vector) : 'a list = Vector.foldr op:: [] v

  val graphToString : G.graph -> string =
    (ListFormat.fmt {init="#[", sep=",", final="]",
      fmt=ListFormat.listToString Int.toString}) o vectorToList


  (*  assertEqGraph (name, test, exp) = t, where t is a graph test
  *   that succeeds if eq(test(), exp), where eq is defined below.
  *)
  fun assertEqGraph (name : string,
                     test : unit -> G.graph,
                     exp : G.graph) : U.test =
  let
    exception UnequalLength

    (*  zip (#[u_0,...,u_{m-1}], #[v_0,...,v_{n-1}]) =
    *     #[(u_0,v_0),...,(u_{m-1},v_{m-1})].
    *   Raises UnequalLength if m <> n.
    *)
    fun zip (v0 : 'a vector, v1 : 'a vector) : ('a*'a) vector =
      if Vector.length v0 = Vector.length v1
      then Vector.tabulate(Vector.length v0,
                           fn i => (Vector.sub(v0, i), Vector.sub(v1, i)))
      else raise UnequalLength

    fun pairSort(us : G.vertex list, vs : G.vertex list) :
        G.vertex list * G.vertex list =
      (ListMergeSort.sort op>= us, ListMergeSort.sort op>= vs)

    (*  eq (#[u_0,...,u_{m-1}], #[v_0,...,v_{n-1}]) = true if m = n and
    *                                                   u_i = v_i, all i,
    *                                                 false o/w.
    *)
    fun eq (v0 : G.vertex list vector, v1 : G.vertex list vector) : bool = 
      ((Vector.all (op= o pairSort)) o zip) (v0, v1) handle UnequalLength => false

  in
    U.makeTestEq(name, test, exp, eq, graphToString)
  end

  (*  fromEdgeListTest(n, es, exp) = t, where t is a G.graph test that succeeds
  *   if G.fromEdgeList(n, es) = exp.
  *)
  fun fromEdgeListTest(n : int, es : (G.vertex*G.vertex) list, exp : G.graph) : 
      U.test =
  let
    fun intPairToString(x : int, y : int) : string =
      String.concat ["(", Int.toString x, ", ", Int.toString y, ")"]

    val name : string =
      String.concat ["{", Int.toString n, ": ",
        ListFormat.listToString intPairToString es, "}"]

    fun test() : G.graph = G.fromEdgeList(n, es)
  in
    assertEqGraph (name, test, exp)
  end

  (*  fromEdgeListFileTest(name, exp) = t, where t is a G.graph test that
  *   succeeds if G.fromEdgeListFile(name) = exp.
  *)
  fun fromEdgeListFileTest(name : string, exp : G.graph) : U.test =
  let
    fun test() : G.graph = G.fromEdgeListFile name
  in
    assertEqGraph (name, test, exp)
  end

  (*  acyclicTest(g, exp) = t, where t is bool test that succeeds if
  *   G.isAcyclic(g) = exp.
  *)
  fun acyclicTest(g : G.graph, exp : bool) : U.test =
  let
    val name = graphToString g
    fun test() : bool = G.isAcyclic g
  in
    U.assertEqBool(name, test, exp)
  end

  (*  G.fromEdgeList tests.
  *)
  val fromEdgeListTests = ("fromEdgeList tests", [
    fromEdgeListTest(2, [(0, 0), (0, 1), (1, 0)], #[ [0, 1], [0] ])
  ])

  (*  G.fromEdgeListFile tests.
  *)
  val fromEdgeListFileTests = ("fromEdgeListFile tests", [
    fromEdgeListFileTest("g22.txt", #[ [1], [0] ])
  ])

  (*  G.isAcyclic tests.
  *)
  val acyclicTests = ("isAcyclic tests", [
    acyclicTest(#[ [1], [2], [3], [] ], true),
    acyclicTest(#[ [1], [2], [3], [4], [0] ], false),
    acyclicTest(#[[1,5],[2,4],[4,3],[ ],[3,5],[2]], true)
  ])

  val allTests = [
    fromEdgeListTests,
    fromEdgeListFileTests,
    acyclicTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
