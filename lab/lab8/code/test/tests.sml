(*  Tests for COMP 212 Lab 8.
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

  fun testConcat (ss : string list) : U.test =
  let
    val name = ListFormat.listToString String.toString ss
    val test = fn () => Lab8.concat ss
    val exp = String.concat ss
  in
    U.assertEqString(name, test, exp)
  end

  fun testImplode (cs : char list) : U.test =
  let
    val name = implode cs
    val test = fn () => Lab8.implode cs
    val exp = implode cs
  in
    U.assertEqString(name, test, exp)
  end

  local
    fun sumPairs0 (ys : (int*int) list) : int list =
      case ys of
           [] => []
         | (x, y) :: zs => (x+y) :: (sumPairs0 zs)

    fun sumUp (ys : int list) : int =
      case ys of
           [] => 0
         | y :: ys => y + (sumUp ys)

  in
    fun testSumPairs (xs : (int*int) list) : U.test =
    let
      val name = ListFormat.fmt {init="[", sep=",", final="]",
        fmt=fn (x, y) => 
              String.concat ["(", Int.toString x, ", ", Int.toString y, ")"]} xs
      val test : unit -> int list = fn () => Lab8.sumPairs xs
      val exp = sumPairs0 xs
    in
      U.assertEqList(name, test, exp, Int.toString)
    end

    fun testSumUpPairs (xs : (int*int) list) : U.test =
    let
      val name = ListFormat.fmt {init="[", sep=",", final="]",
        fmt=fn (x, y) => 
              String.concat ["(", Int.toString x, ", ", Int.toString y, ")"]} xs
      val test : unit -> int = fn () => Lab8.sumUpPairs xs
      val exp = sumUp (sumPairs0 xs)
    in
      U.assertEqInt(name, test, exp)
    end
  end

  fun testFilter (xs : int list) : U.test =
  let
    val name = ListFormat.listToString Int.toString xs
    val test = fn () => Lab8.filter (fn x => x mod 2 = 0) xs
    val exp = List.filter (fn x => x mod 2 = 0) xs
  in
    U.assertEqList(name, test, exp, Int.toString)
  end

  val concatTests = ("concat tests", [
    testConcat ["abc", "defg", "hi"]
  ])

  val implodeTests = ("implode tests", [
    testImplode [#"a", #"b"]
  ])

  val sumPairsTests = ("sumPairs tests", [
    testSumPairs [(1, 2), (3, 4), (5, 6)]
  ])

  val sumUpPairsTests = ("sumPairs tests", [
    testSumUpPairs [(1, 2), (3, 4), (5, 6)]
  ])

  val filterTests = ("filter tests", [
    testFilter [0, 2, 4]
  ])

  val suites = [
    concatTests,
    implodeTests,
    sumPairsTests,
    filterTests,
    sumUpPairsTests
  ]

  fun runTests(cmd : string, args : string list) : int =
    TR.runTimedTestSuites(suites, 30, true)

end
