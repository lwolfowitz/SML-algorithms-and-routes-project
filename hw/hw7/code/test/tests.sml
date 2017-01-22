(*  Tests for COMP 212 Homework 7: Braun vectors.
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure V = Hw7BraunVector

  (*  toString itemToString (t, cmp) = a string representation of keys(t),
  *   where the string representation of the key x is itemToString x.
  *
  *   The following is a reasonable implementation:
  *
  fun toString (itemToString : 'a -> string, s : 'a set) : string =
    ListFormat.fmt {init="{", sep=",", final="}", fmt=itemToString}
      (listItems s)
  *
  *   But for this sample code, we will expose the underlying tree
  *   structure in the string representation.
  *)
  fun toString (itemToString : 'a -> string) ((t, n) : 'a V.vector) : string =
  let
    (*  pad(s, n1, m1, m2, n2) =
    *     "    ----" ^ s ^ "----    "
    *   (n1 leading spaces, m1 leading dashes, m2 trailing dashes, n2 trailing
    *   spaces).
    *)
    fun pad(s, n1, m1, m2, n2) =
      (implode (List.tabulate(n1, fn _ => #" "))) ^
      (implode (List.tabulate(m1, fn _ => #"-"))) ^
      s ^
      (implode (List.tabulate(m2, fn _ => #"-"))) ^
      (implode (List.tabulate(n2, fn _ => #" ")))

    (*  merge ([s_0,...], n1, [s_0',...], n2, n) =
    *     [..., s_i ^ "   " ^ s_i',...]
    *     (n spaces).  Each element of the result list has length n1+n+n2.
    *)
    fun merge(ss1 : string list, n1 : int, ss2 : string list, n2 : int, n) :
        string list =
      case (ss1, ss2) of
           ([], _) => map (fn s => pad (s, n1+n, 0, 0, 0)) ss2
         | (_, []) => map (fn s => pad (s, 0, 0, 0, n2+n)) ss1
         | (s1 :: ss1, s2 :: ss2) =>
             ((pad(s1, 0, 0, 0, n div 2)) ^ (pad(s2, n-(n div 2), 0, 0, 0))) ::
               merge(ss1, n1, ss2, n2, n)

    (*  treeToString(t) = ([s_0,...,s_{m-1}], n, m1, m2), where:
    *     * s_0 ^ "\n" ^ s_1 ^ "\n" ^ ... ^ "\n" ^ s_{m-1} is a string
    *       representation of t.
    *     * String.size(s_i) = n for all i.
    *     * String.sub(s_0, m1) is the first character of the root rep'n
    *       in s_0.
    *     * String.sub(s_0, n1) is the last character of the root rep'n
    *       in s_0.
    *)
    fun treeToString (t : 'a V.tree) : (string list)*int*int*int =
      case t of
           V.E => (["[]"], 2, 0, 1)
         | V.N(x, t1, t2) =>
             let
               val (ss1, n1, m11, m12) = treeToString t1
               val (ss2, n2, m21, m22) = treeToString t2
               val s = itemToString x
               val n = String.size s
             in
               (pad(s, m11, n1-m11, m22+1, n2-m22-1) :: 
                 merge(ss1, n1, ss2, n2, n),
                 n1+n2+n, n1, n1+n-1)
             end

  in
    String.concatWith "\n" [
      String.concatWith "\n" (#1(treeToString t)),
      "(size = " ^ (Int.toString n) ^ ")"
    ]
  end

  fun leaf(n : int) = V.N(n, V.E, V.E)

  (*  A collection of "manually constructed" Braun vectors.
  *)
  val v6 = (
    V.N(0,
      V.N(3,
        leaf(9),
        leaf(15)),
      V.N(6,
        leaf(12),
        V.E)),
    6
  )
  val v6Keys = List.tabulate(6, fn i => 3*i)

  (*  A useful function for building up complex tests.
  *)
  infix andthen
  fun op andthen(t0 : unit -> bool*string, t1 : unit -> bool*string)
      : unit -> bool*string =
    case t0() of
         (true, _) => t1
       | (false, msg) => raise Fail msg

  (*  Some testing functions.
  *)
  fun checkSize((t, n) : int V.vector, m : int) : bool*string =
    case m = n of
         true => (true, "")
       | false => (
           false, 
           String.concat ["Wrong size: got ", Int.toString n, ", should be ",
             Int.toString m]
         )

  fun checkVs(v : int V.vector, xs : int list) : bool*string =
  let
    fun findBadV(i : int) : int option =
      if i = length xs then NONE
      else
        if V.sub(v, i) <> List.nth(xs, i) then SOME i
        else findBadV (i+1)
  in
    case findBadV(0) of
         NONE => (true, "")
       | SOME(i) => (
           false,
           String.concat ["sub(v, ", Int.toString i, ") = ",
             Int.toString (V.sub(v, i)), ", should be ",
             Int.toString (List.nth(xs, i)), "; vector is\n",
             toString Int.toString v]
         )
  end

  fun checkVector(v : int V.vector, xs : int list) =
    (
      (fn () => checkSize(v, length xs))
      andthen
      (fn () => checkVs(v, xs))
    ) ()
    handle Fail msg => (false, msg)

  fun testSub(name : string, v : int V.vector, xs : int list) : U.test =
  let
    fun test() = checkVector(v, xs)
  in
    U.assertTrueMsg(name, test)
  end

  fun testUpdateInternal(v : int V.vector, i : int) : U.test =
  let
    val name = Int.toString i

    fun test() =
    let
      val res = V.update(v, i, ~1)
      val exp =
        (List.take(v6Keys, i)) @ [~1] @ (List.drop(v6Keys, i+1))
    in
      checkVector(res, exp)
    end
  in
    U.assertTrueMsg(name, test)
  end

  fun testUpdateEnd(v : int V.vector, xs : int list) : U.test =
  let
    val name = ListFormat.listToString Int.toString xs

    fun test() =
    let
      fun updateWithList(v : int V.vector, i : int, ys : int list) : 
          int V.vector =
        case ys of
             [] => v
           | y :: ys =>
             let
               val v' = V.update(v, i, y)
             in
               updateWithList(v', i+1, ys)
             end

      val res = updateWithList(v6, 6, xs)

      val exp = v6Keys @ xs
    in
      checkVector(res, exp)
    end
  in
    U.assertTrueMsg(name, test)
  end

  fun testTabulate(i : int) : U.test =
  let
    val name = Int.toString i

    fun test() =
    let
      val res = V.tabulate(i, fn i => 4*i)
      val exp = List.tabulate(i, fn i => 4*i)
    in
      checkVector(res, exp)
    end
  in
    U.assertTrueMsg(name, test)
  end

  fun testFromList(i : int) : U.test =
  let
    val name = Int.toString i

    fun test() =
    let
      val exp = List.tabulate(i, fn i => 4*i)
      val res = V.fromList(exp)
    in
      checkVector(res, exp)
    end
  in
    U.assertTrueMsg(name, test)
  end

  fun testToList(i : int) : U.test =
  let
    val name = Int.toString i

    fun test() =
    let
      val v = V.tabulate(i, fn i => 4*i)
    in
      V.toList(v)
    end

    val exp = List.tabulate(i, fn i => 4*i)
  in
    U.assertEqList(name, test, exp, Int.toString)
  end

  fun testMap(i : int) : U.test =
  let
    val name = Int.toString i

    fun test() =
    let
      val v = V.tabulate(i, fn j => 3*j)
      val res = V.map (fn i => 7*i) v
      val exp = List.tabulate(i, fn i => 21*i)
    in
      checkVector(res, exp)
    end
  in
    U.assertTrueMsg(name, test)
  end


  (*  The following tests verify that your sub function is correct
  *   on the "manually constructed" Braun vectors.
  *)
  val subTests = ("sub tests", [
    testSub("v6", v6, [0, 3, 6, 9, 12, 15])
  ])

  (*  The following test verifies that update(v6, i, ~1) gives the
  *   correct result for i=0,...,5
  *
  *   testUpdateInternal(#[x_0,...,x_{n-1}], i) verifies that
  *     update(v, i, ~1) = #[x_0,...,x_{i-1},~1,x_i,...,x_{n-1}]
  *)
  val updateInternalTests = ("update tests (internal)", [
    testUpdateInternal(v6, 3)
  ])

  (*  The following tests append one item at a time to v6.
  *)
  val updateEndTests = ("update tests (append)", [
    testUpdateEnd(v6, [1, 2, 3, 4, 5])
  ])

  (*  testTabulate(n) verifies that 
  *     tabulate(n, f) = #[f(0),...,f(n-1)].
  *)
  val tabulateTests = ("tabulate tests", [
    testTabulate(3)
  ])

  (*  testFromList(n) verifies that 
  *     fromList([0,4,...,4*(n-1)]) = #[0,4,...,4*(n-1)].
  *)
  val fromListTests = ("fromList tests", [
    testFromList(3)
  ])

  (*  testToList(n) verifies that 
  *     toList(tabulate(n, fn i => 4*i)) = [0,...,4*(n-1)]
  *   This test will fail if tabulate is not correct.
  *)
  val toListTests = ("toList tests", [
    testToList(3)
  ])

  (*  testMap(n) verifies that
  *     map (fn j => 7*j) (tabulate(n, fn i => 3*i)) = #[0, 21,...,21*(n-1)].
  *)
  val mapTests = ("map tests", [
    testMap(3)
  ])

  val allTests = [
    subTests,
    updateInternalTests,
    updateEndTests,
    tabulateTests,
    fromListTests,
    toListTests,
    mapTests
  ]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
