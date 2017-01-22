(*  Tests for COMP 212 Lab 7.
*
*   NOTE:  This test code will not compile unless the irtree type
*   is correctly defined in ../src/lab7.sml.
*   
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  fun assertEqRealList(name, test, exp) =
    U.makeTestEq(name, test, exp, ListPair.all Real.==,
      ListFormat.listToString Real.toString)

  fun assertEqStrList(name, test, exp) =
    U.assertEqList(name, test, exp, String.toString)

  val toRealsTests = ("toReals tests", [
    assertEqRealList("[0, 5, ~9]", fn () => Lab7.toReals [0, 5, ~9],
      [0.0, 5.0, ~9.0])
  ])

  val toAbsRealsTests = ("toAbsReals tests", [
    assertEqRealList("[0, ~3, 18]", fn () => Lab7.toAbsReals [0, ~3, 18], 
      [0.0, 3.0, 18.0])
  ])

  val oneCharStrTests = ("oneCharStrings tests", [
    assertEqStrList("[a, abc, d, fgh]", 
      fn () => Lab7.oneCharStrings ["a", "abc", "d", "fgh"], ["a", "d"])
  ])

  val sumTests = ("sum tests", [
    U.assertEqReal("[2.3: [1:] ; [3.8:]]", fn () => Lab7.sum(
      Lab7.RNode(2.3,
        Lab7.INode(1,
          Lab7.Empty,
          Lab7.Empty),
        Lab7.RNode(3.8,
          Lab7.Empty,
          Lab7.Empty))), 7.1)
  ])

  val suites = [
    toRealsTests,
    toAbsRealsTests,
    oneCharStrTests,
    sumTests
  ]

  fun runTests(cmd : string, args : string list) : int =
    TR.runTimedTestSuites(suites, 30, true)

end
