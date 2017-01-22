(*  Tests for COMP 212 Homework 5: Graphs.
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure G = Hw5Graph

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

  (*  fromEdgeListTest(n, es, exp) = t, where t is a G.graph test that
  *   succeeds if G.fromEdgeList(n, es) = exp.
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

  (*  fromEdgeListFileTest(name, exp) = t, where t is a G.graph test
  *   that succeeds if G.fromEdgeListFile(name) = exp.
  *)
  fun fromEdgeListFileTest(name : string, exp : G.graph) : U.test =
  let
    fun test() : G.graph = G.fromEdgeListFile name
  in
    assertEqGraph (name, test, exp)
  end

  (*  acyclicTest(g, exp) = t, where t is a bool test that succeeds if
  *   G.isAcyclic(g) = exp.
  *)
  fun acyclicTest(g : G.graph, exp : bool) : U.test =
  let
    val name = graphToString g
    fun test() : bool = G.isAcyclic g
  in
    U.assertEqBool(name, test, exp)
  end

  (*  topSortTest(g) = t, where t is a test that succeeds if
  *   G.topSort(g) returns a valid topological sort of g.
  *
  *   This test function is remarkably complicated, so it is really
  *   the specification that should matter to you:  the test is
  *   going to call G.topSort(g) and see if the result is a topological
  *   sort of g.  Don't worry about the implementation.
  *)
  fun topSortTest (g : G.graph) : U.test =
  let

    (*  n = the number of vertices in g.
    *)
    val n : int = Vector.length g

    (*  es = the list of edges in g.
    *)
    val es : (G.vertex*G.vertex) list =
      Vector.foldr op@ [] (
        Vector.mapi (fn (i, vs) => map (fn v => (i, v)) vs) g
      )

    (*  iiListToString es = a string representation of es.
    *)
    val iiListToString : (int*int) list -> string =
          ListFormat.listToString 
            (fn (i, j) => String.concat ["(", Int.toString i, ", ",
              Int.toString j, ")"])

    (*  testName = a string to print out when running the test.
    *)
    val testName : string = graphToString g

    (*  allPaths g = [(u_0, v_0),...,(u_{m-1}, v_{m-1})], where
    *     - (u, v) = (u_i, v_i) for some i if and only if there is a
    *       path from u to v in g; and
    *     - if i <> j, then (u_i, v_i) <> (u_j, v_j).
    *   In other words, allPaths g is a list of pairs of the form
    *   (u, v), where there is a path from u to v in g, and the
    *   list contains no duplicates.
    *
    *   This is a horrendously inefficient implementation of this
    *   function.
    *)
    fun allPaths () : (G.vertex*G.vertex) list =
    let

      (*  vs = the list of vertices of g.
      *)
      val vs : G.vertex list = List.tabulate (n, fn i => i)

      (*  allPathN n = [(u_0, v_0),...], where (u, v) = (u_0, v_0) if
      *   and only if there is a path of length n from u to v in g.
      *)
      fun allPathsN (n : int) : (G.vertex*G.vertex) list =
        case n of
             1 => es
           | _ =>
             let
               (*  paths = [(u_0,v_0),...], where (u, v) = (u_i, v_i) if
               *   and only if there is a path of length n-1 from u to v.
               *)
               val paths = allPathsN(n-1)

               (*  mem [x_0,...,x_{m-1}] x = true if x = x_i for some i,
               *                           = false, o/w.
               *)
               fun mem (xs : ''a list) (x : ''a) : bool = 
                 List.exists (fn y => y = x) xs

               (*  extendPathsWithEdge (u, v) = [(u, y_0), (u, y_1),...], where
               *     - paths = [(w_0, x_0), (w_1, x_1),...]
               *     - y = y_i if and only if
               *       * v = w_j and y = x_j for some j.
               *   In other words, extendPathsWidthEdge (u, v) is the list
               *   of pairs (u, x) such that there is some pair of the
               *   form (y, x) in paths.
               *)
               fun extendPathsWithEdge (u, v) =
                 map (fn (w, x) => (u, x))
                     (List.filter (fn (w, x) => v = w) paths)

               (*  paths' = [(u_0, v_0),...], where (u, v) = (u_i, v_i)
               *   if and only if there is a paht of length n from u to v.
               *)
               val paths' =
                 List.concat (map extendPathsWithEdge es)

               (*  removeDups [x_0,...,x_{k-1}] = [y_0,...,y_{m-1}], where
               *   [y_0,...,y_{m-1}] is a permutation of the set
               *   {x_0,...,x_{k-1}}.  In other words, removeDups xs is
               *   a list consisting of the elements of xs, but with
               *   duplicate elements removed.
               *)
               fun removeDups (xs : ''a list) : ''a list =
                 case xs of
                      [] => []
                    | x' :: xs' =>
                      let
                        val xs'' = removeDups xs'
                      in
                        if mem xs'' x' then xs'' else x' :: xs'
                      end
             in
               removeDups paths'
             end
    in
      List.concat (List.tabulate (n, fn i => allPathsN (i+1)))
    end

    (*  index ([i_0,...,[i_{m-1}], i) = k, where i = i_k.
    *   Pre-condition:  i = i_k for exactly one value k.
    *)
    fun index (is : int list, i : int) =
      case is of
           i' :: is => if i = i' then 0 else 1 + (index (is, i))

    (*  badPair(vs, i, j) = true, if j occurs before i in vs
    *                     = false, otherwise.
    *   Pre-condition:  i and j occur in vs.
    *)
    fun badPair (vs : int list, i : int, j : int) : bool =
      index (vs, j) < index (vs, i)

    (*  badPairs vs = [(v_i, v_j),...], where
    *     each (v_i, v_j) is a pair in vs with i < j such that
    *     there is no path from v_i to v_j in (n, es).
    *)
    fun badPairs (vs : int list) : (int*int) list =
      List.filter (fn (i, j) => badPair(vs, i, j)) (allPaths ())

    fun test() =
      let
        val g = G.fromEdgeList (n, es)
        val vs = G.topSort g
        val msg = ""

        (*  vsRight = true if vs is a permutation of {0,...,n-1}.
        *)
        val vsRight =
          (ListMergeSort.sort (op>=) vs) = (List.tabulate (n, fn i => i))
        val vsMsg = if vsRight then ""
                    else String.concat ["Wrong vertices: ",
                      ListFormat.listToString (Int.toString) vs]

        (*  bps = the list of bad pairs in vs.
        *)
        val bps = badPairs vs
        val noBPs = null bps
        val bpsMsg = if noBPs then ""
                     else String.concat ["Bad pairs in ",
                       ListFormat.listToString (Int.toString) vs, ": ",
                       iiListToString bps]

        val msg = String.concatWith "; " [msg, vsMsg, bpsMsg]

      in
        (vsRight andalso noBPs, msg)
      end
  in
    U.assertTrueMsg(testName, test)
  end

  (*  See the documentation for fromEdgeListTest, fromEdgeListFileTest,
  *   etc. to see how these tests correspond to calling functions
  *   in G.
  *)

  val fromEdgeListTests = ("fromEdgeList tests", [
    (* Some small graphs.
    *)
    fromEdgeListTest(2, [(0, 0), (0, 1), (1, 0)], #[ [0, 1], [0] ])
  ])

  val acyclicTests = ("isAcyclic tests", [
    acyclicTest(#[ [1], [2], [3], [] ], true),
    acyclicTest(#[ [1], [2], [3], [4], [0] ], false)
  ])

  val topSortTests = ("topSort tests", [
    topSortTest (#[ [1], [2], [3], [] ]),
    topSortTest (#[ [1], [4], [3], [], [] ]),
    topSortTest (#[ [1], [2], [3], [] ]),
    topSortTest (#[ [1], [] ]),
    topSortTest (#[ [1, 5], [2, 4], [6], [2], [3], [3], [] ])
  ])

  val allTests = [
    fromEdgeListTests,
    acyclicTests,
    topSortTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
