(*  Tests for COMP 212 Homework 1.
*
*   See the testing code for Lab 1 for instructions on how to use this
*   testing code.
*   
*   N. Danner
*)

structure Tests =
struct

  fun testEq (test, exp, act) =
    (
      print (test ^ "...") ;
      if exp = act then print ("SUCCESS!")
      else print ("FAILED.") ;
      print "\n"
    )

  fun testEqReal (test, exp, act) =
    (
      print (test ^ "...") ;
      if Real.==(exp, act) then print ("SUCCESS!")
      else print ("FAILED.") ;
      print "\n"
    )

  fun runTests(cmd : string, args : string list) : int =
    (
      (*  **********
      *   round10 tests
      *)
      testEqReal ("round10 11.0", 11.0, Hw1.round10 11.0) ;
      testEqReal ("round10 11.05", 11.1, Hw1.round10 11.05) ;
      testEqReal ("round10 11.07", 11.1, Hw1.round10 11.07) ;
      testEqReal ("round10 ~11.05", ~11.1, Hw1.round10 ~11.05) ;

      (*  **********
      *   rotate1, rotate tests
      *)
      testEq ("rotate1 [1, 2, 3, 4, 5]", [2, 3, 4, 5, 1], 
        Hw1.rotate1 [1, 2, 3, 4, 5]) ;
      testEq ("rotate ([5, 4, 3, 2, 1], 3)", [2, 1, 5, 4, 3],
              Hw1.rotate([5, 4, 3, 2, 1], 3)) ;

      (*  **********
      *   editDist tests.
      *)
      testEq("editDist('abc', 'xabc')", 1, Hw1.editDist("abc", "xabc")) ;
      testEq("editDist('abc', 'xxbc')", 2, Hw1.editDist("abc", "xxbc")) ;

      0
    )

  fun runTests0() = runTests("", [])

end
