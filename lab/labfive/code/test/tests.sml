(*  Tests for COMP 212 Lab 5.
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
              act : unit -> 'a,
              eq : 'a*'a -> bool,
              toString : 'a -> string) : unit =
  let
    val () = print (test ^ "...")
    val res = act()
  in
    (
      if eq(exp, res) then print ("SUCCESS!")
      else print (
        String.concat [
          "FAILED (expected ", toString exp, ", got ",
          toString res, ")"
        ]) ;
      print "\n"
    )
  end

  fun testEqInt (test : string,
                 exp : int,
                 act : unit -> int) : unit =
    testEq (test, exp, act, op=, Int.toString)

  fun testEqReal (test : string, exp : real, act : unit -> real) : unit =
    testEq (test, exp, act, Real.==, Real.toString)

  fun testEqString (test : string, exp : string, act : unit -> string) : unit =
    testEq (test, exp, act, op=, String.toString)

  fun testEqUnit (test : string, act : unit -> unit) : unit =
    testEq (test, (), act, op=, fn _ => "()")

  fun testEqIntOpt (test : string, 
                    exp : int option, 
                    act : unit -> int option) : unit =
    testEq (test, exp, act, op=, fn NONE => "NONE" | SOME n => Int.toString n)

  fun testEqList (test : string,
      exp : int list,
      act : unit -> int list) : unit =
    testEq (test, exp, act, op=, ListFormat.listToString Int.toString)

  fun testExn(test : string, t : unit -> 'a) : unit =
  let
    val () = print (test ^ "...")
    val _  = t()
  in
    print "FAILURE!\n"
  end

  fun runTests(cmd : string, args : string list) : int =
    (

      (*  This test invokes Lab5.third([]).  If the invocation
      *   raises TooShort, the test is a success; otherwise
      *   a failure message is printed.
      *)
      testExn("third []", fn () => Lab5.third([]))
      handle 
          Lab5.TooShort => print ("SUCCESS!\n")
        | e => print ("FAILURE:  raised " ^ exnMessage(e) ^ "\n") ;

      testExn("take([], 1)", fn () => Lab5.take([], 1))
      handle
          Lab5.TooShort => print ("SUCCESS!\n")
        | e => print ("FAILURE:  raised " ^ exnMessage(e) ^ "\n") ;

      testExn("take2([], 1)", fn () => Lab5.take2([], 1))
      handle
          Lab5.BadIndex(n) => 
            if n = 1 then print ("SUCCESS!\n")
            else print ("FAILURE:  n = " ^ (Int.toString n) ^ "\n")
        | e => print ("FAILURE:  raised " ^ exnMessage(e) ^ "\n") ;

      testEqIntOpt("nth([], 1", NONE, fn () => Lab5.nthOpt([], 0)) ;
      testEqIntOpt("nth([7, 8, 9], 2)", 
        SOME 9, fn () => Lab5.nthOpt([7, 8, 9], 2)) ;

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
