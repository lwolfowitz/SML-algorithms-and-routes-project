(*  Tests for COMP 212 final exam.
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  (*  **********
  *   Graph tests.
  *   **********
  *)

  structure G = WeightedGraphAdjList : WEIGHTED_GRAPH

  (*  The following function lets us glue together a sequence of tests.
  *)
  infix andthen
  fun op andthen(t0 : unit -> bool*string, t1 : unit -> bool*string)
      : unit -> bool*string =
    case t0() of
         (true, _) => t1
       | (false, msg) => raise Fail msg

  (*  isPath(g, us) = (true, ""), if us is a path in g
  *                 = (false, s), if us is not a path in g, where s is a
  *                               message describing why not.
  *)
  fun isPath(g : G.graph, us : G.vertex list) : bool*string =
  let
    fun firstBadPair (us : G.vertex list) : int option =
      case us of
           [] => NONE
         | [u] => NONE
         | u_0 :: u_1 :: us =>
             if not (G.hasEdge(g, u_0, u_1)) then SOME 0
             else
               case firstBadPair(u_1 :: us) of
                    NONE => NONE
                  | SOME i => SOME (1 + i)
  in
    case firstBadPair us of
         NONE => (true, "")
       | SOME i =>
           (false, 
                  "No edge from " ^ (Int.toString (List.nth(us,  i))) ^ " to " ^
                  (Int.toString (List.nth(us, i+1))) ^ ".")
  end

  (*  startsAt(us, u) = (true, "") if us = u :: us'
  *                   = (false, s), otherwise, where s is a message describing
  *                                 why not.
  *)
  fun startsAt(us : G.vertex list, u : G.vertex) : bool*string =
    if List.nth(us, 0) = u then (true, "")
    else (false,
      "First vertex in path not " ^ (Int.toString u) ^ ".")

  (*  endsAt(us, v) = (true, ""), if List.last us = v,
  *                 = (false, s), otherwise, where s is a message describing
  *                               why not.
  *)
  fun endsAt(us : G.vertex list, v : G.vertex) : bool*string =
    if List.last(us) = v then (true, "")
    else (false,
      "Last vertex in path not " ^ (Int.toString v) ^ ".")

  (*  rightWeight(g, us, exp) = (true, ""), if the weight of us is exp,
  *                           = (false, s), otherwise, where s is a message
  *                                         describing why not.
  *   Pre-condition:  us is a path in g.
  *)
  fun rightWeight(g : G.graph, us : G.vertex list, exp : real) : bool*string =
  let
    fun pathWeight(us : G.vertex list) : real =
      case us of
           [] => 0.0
         | [u] => 0.0
         | u_0 :: u_1 :: us =>
             G.weight(g, u_0, u_1) + (pathWeight(u_1 :: us))

    val res = pathWeight us
  in
    if Real.==(res, exp) then (true, "")
    else (false,
      "Wrong weight:  got " ^ (Real.toString res) ^ ", expected " ^
      (Real.toString exp) ^ ".")
  end

  (*  checkPath(g, us, u, v, expWt) = (true, ""), if us is a path from u to v
  *                                               in g with weight expWt
  *                                 = (false, s), otherwise, where s is a
  *                                               message describing why not.
  *)
  fun checkPath(g, us, u, v, expWt) =
    (
      (fn () => isPath(g, us))
      andthen
      (fn () => startsAt(us, u))
      andthen
      (fn () => endsAt(us, v))
      andthen
      (fn () => rightWeight(g, us, expWt))
      andthen
      (fn () => (true, ""))
    ) ()
    handle Fail msg => (false, 
      msg ^ "  Result path was " ^ 
      (ListFormat.listToString Int.toString us) ^ ".")

  (*  testDist (adjLists, u, v, expWt) = t, where t is a test that succeeds
  *   if G.dist(G.V.fromList adjLists, u, v) = expWt.
  *)
  fun testDist (adjLists : G.adjlist list, 
                u : int, v : int, expWt : real option) : U.test =
  let
    val g = G.V.fromList adjLists
    val name = (G.toString g) ^ " (u = " ^ (Int.toString u) ^ ", v = " ^
               (Int.toString v) ^ ")"

    fun test() : real option = G.dist(g, u, v)

    fun realOptEq(r0 : real option, r1 : real option) : bool =
      case (r0, r1) of
           (NONE, NONE) => true
         | (SOME x0, SOME x1) => Real.==(x0, x1)
         | _ => false

    fun realOptToString (r : real option) : string =
      case r of
           NONE => "NONE"
         | SOME x => "SOME(" ^ (Real.toString x) ^ ")"
  in
    U.makeTestEq(name, test, expWt, realOptEq, realOptToString)
  end

  (*  testPath (adjLists, u, v, expWt) = t, where t is a test that succeeds
  *   if G.path(G.V.fromList adjLists, u, v) is a path from u to v with
  *   weight expWt.
  *)
  fun testPath (adjLists : G.adjlist list, 
                u : int, v : int, expWt : real) : U.test =
  let
    val g = G.V.fromList adjLists
    val name = (G.toString g) ^ " (u = " ^ (Int.toString u) ^ ", v = " ^
               (Int.toString v) ^ ")"

    fun test() : bool*string =
    let
      val path = G.path(g, u, v)
    in
      checkPath(g, path, u, v, expWt)
    end

  in
    U.assertTrueMsg(name, test)
  end

  (*  testNoPath (adjLists, u, v) = t, where t is a test that succeeds
  *   if G.path(G.V.fromList adjlists, u, v) = [].
  *)
  fun testNoPath (adjLists : G.adjlist list, u : int, v : int) : U.test =
  let
    val g = G.V.fromList adjLists
    val name = (G.toString g) ^ " (u = " ^ (Int.toString u) ^ ", v = " ^
               (Int.toString v) ^ ")"
  in
    U.assertEqList(name, fn () => G.path(g, u, v), [], Int.toString)
  end

  (*  Mehlhorn and Sanders, Fig. 10.5.
  *)
  val msEx = [
    [(1, 3.0)],
    [(2, 2.0), (4, 1.0)],
    [],
    [(6, 4.0), (1, 5.0)],
    [(3, 0.0), (1, 9.0), (2, 8.0)],
    [(4, 7.0)],
    [(0, 2.0), (3, 10.0)]
  ]

  (*  See specification for testDist for an explanation of each of these
  *   tests.
  *)
  val distTests = ("Graph.dist tests (path exists)", [
    testDist(msEx, 6, 3, SOME 6.0)
  ])

  (*  See specification for testDist for an explanation of each of these
  *   tests.
  *)
  val noDistTests = ("Graph.dist tests (path does not exist)", [
    testDist(msEx, 3, 5, NONE)
  ])

  (*  See specification for testPath for an explanation of each of these
  *   tests.
  *)
  val pathTests = ("Graph.path tests (path exists)", [
    testPath(msEx, 6, 3, 6.0)
  ])

  (*  See specification for testNoPath for an explanation of each of these
  *   tests.
  *)
  val noPathTests = ("Graph.path tests (path does not exist)", [
    testNoPath(msEx, 3, 5)
  ])

  val allTests = [
    distTests,
    noDistTests,
    pathTests,
    noPathTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 60, true)

end
