(*  Tests for COMP 212 Lab 4.
*   
*   N. Danner
*)

structure Tests =
struct

  (*  testEqBase(test, exp, act, eq, toString) = true if eq(exp, act), 
  *                                              false otherwise.
  *   In evaluating eq(exp, act), this function prints test to the terminal,
  *   and on failure, prints toString(exp) and toString(act).
  *)
  fun testEq (test : string, 
              exp : 'a, 
              act : 'a,
              eq : 'a*'a -> bool,
              toString : 'a -> string) : unit =
    (
      print (test ^ "...") ;
      if eq(exp, act) then print ("SUCCESS!")
      else print (
        String.concat [
          "FAILED (expected ", toString exp, ", got ",
          toString act, ")"
        ]) ;
      print "\n"
    )

  fun testEqInt (test : string,
                 exp : int,
                 act : int) : unit =
    testEq (test, exp, act, op=, Int.toString)

  fun testEqReal (test : string, exp : real, act : real) : unit =
    testEq (test, exp, act, Real.==, Real.toString)

  fun testEqString (test : string, exp : string, act : string) : unit =
    testEq (test, exp, act, op=, String.toString)

  fun testEqUnit (test : string, act : unit) : unit =
    testEq (test, (), (), op=, fn _ => "()")

  fun testEqList (test : string,
      exp : int list,
      act : int list) : unit =
    testEq (test, exp, act, op=, ListFormat.listToString Int.toString)

  (*  powRec(a, n) = a^n.
  *   Can't use Math.pow to compute expected values of Lab3.thouPow,
  *   because Math.pow doesn't perform repeated multiplication, and hence
  *   can compute a different result than what we expect here.
  *)
  fun powRec(a : real, n : int) : real =
    case n of
         0 => 1.0
       | _ =>
         let
           val z = powRec(a, n div 2)
         in
           if n mod 2 = 0 then z*z
           else a*z*z
         end

  fun runTests(cmd : string, args : string list) : int =
    (

      (*  These tests always succeed.  Look at the terminal output
      *   to make sure that printXs prints the right number of Xs.
      *)
      testEqUnit("printXs(3)", Lab4.printXs(3));

      testEqList("readNumbers0(nums2.txt)", 
        [23, 4523, 967, 1234], Lab4.readNumbers0("nums2.txt")) ;

      testEqList("readNumbers1(nums3.txt)", 
        [23, 4523, 967, 1234], Lab4.readNumbers1("nums3.txt")) ;

      testEqList("readNumbers2(nums4.txt)",
        [23, 4523, 967, 1234], Lab4.readNumbers2("nums4.txt")) ;

      0
    )
    handle e => 
    let
      val msg : string =
        String.concat [
          "LAST TEST (NAME NOT PRINTED) RAISED AN EXECPTION: ",
          exnName(e), "(", exnMessage(e), ")!\n"
        ]
    in
      (print msg ; 1)
    end

  fun runTests0() = runTests("", [])

end
