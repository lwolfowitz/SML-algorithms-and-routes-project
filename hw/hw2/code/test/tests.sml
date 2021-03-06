(*  Tests for COMP 212 Homework 2.
*
*   See the testing code for Lab 1 for instructions on how to use this
*   testing code.
*   
*   N. Danner
*)

structure Tests =
struct

  (*  testEq(test, exp, act) = true if exp = act, false otherwise.
  *   In evaluating exp=act, this function prints test to the terminal.
  *)
  fun testEq (test : string, 
              exp : ''a, 
              act : ''a,
              toString : ''a -> string) : unit =
    (
      print (test ^ "...") ;
      if exp = act then print ("SUCCESS!")
      else print (
        String.concat ["FAILED (expected ", toString exp, ", got ",
          toString act, ")"]) ;
      print "\n"
    )

  fun testEqInt (test : string,
                 exp : int,
                 act : int) : unit =
    testEq (test, exp, act, Int.toString)

  fun testEqEds (test : string, 
    exp : ((string*string)*int) list,
    act : ((string*string)*int) list) : unit =
  let
    fun toString ((s1, s2), n) =
      String.concat ["((", s1, ", ", s2, "), ", Int.toString n, ")"]
  in
    testEq(test, exp, act, ListFormat.listToString toString)
  end

  (*  The following function defines an order on values of type
  *   ((string*string)*int).  I use it to sort the list returned by
  *   your editDistances function.  This lets me verify that you
  *   return the correct list of values without having to worry about
  *   what order you return them in.
  *)
  (*  sort (xs) = ys, where ys has the same elements as xs, sorted in
  *   non-decreasing order.  We define ((s1, s2), n) >= ((s1', s2'), n')
  *   if and only if:
  *   - s1 > s1'; or
  *   - s1 = s1' and s2 >= s2'
  *   where s > s' means String.>(s, s') = true.  Note that this order
  *   is independent of the values of n and n'.
  *)
  val sort : ((string*string)*int) list -> ((string*string)*int) list =
  let
    fun ge (((w1, w2), k), ((w1', w2'), k')) =
      String.> (w1, w1') orelse (w1 = w1' andalso String.>= (w2, w2'))
  in
    ListMergeSort.sort ge
  end

  fun runTests(cmd : string, args : string list) : int =
    (

    (*  Non-memoized edit distance tests.
    *)
    testEqInt("EditDistRec.editDist('abc', 'xabc')", 1, 
      Hw2.EditDistRec.editDist("abc", "xabc")) ;
    testEqInt("EditDistRec.editDist('abc', 'xxbc')", 2, 
      Hw2.EditDistRec.editDist("abc", "xxbc")) ;

    (*  This test should appear like an infinite loop, because will just
    *   take too long to complete.
    *
    testEqInt(
      "EditDistRec.editDist('baaaaaaaaaaaaaaaaaaa', 'bbbbbbbbbbbbbbbbbbbb'",
      19,
      Hw2.EditDistRec.editDist("baaaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbb"));
    *)

    (*  Memoized edit distance tests.
    *)
    testEqInt("EditDistMem.editDist('abc', 'xabc')", 1, 
      Hw2.EditDistMem.editDist("abc", "xabc")) ;
    testEqInt("EditDistMem.editDist('abc', 'xxbc')", 2, 
      Hw2.EditDistMem.editDist("abc", "xxbc")) ;
    testEqInt(
      "EditDistMem.editDist('baaaaaaaaaaaaaaaaaaa', 'bbbbbbbbbbbbbbbbbbbb'",
      19,
      Hw2.EditDistMem.editDist("baaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbb"));

    (*  editDistances tests.
    *)
    testEqEds("editDistances(['a', 'ab', 'abc'])",
      [(("a", "ab"), 1), (("a", "abc"), 2), (("ab", "abc"), 1)],
      sort(Hw2.EditDistances.editDistances(["a", "ab", "abc"]))) ;

      0
    )

  fun runTests0() = runTests("", [])

end
