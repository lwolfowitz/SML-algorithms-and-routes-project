(*  Tests for COMP 212 Lab 6.
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

  fun testEqRealList (test : string,
       exp : real list,
       act : unit -> real list) : unit =
     testEq (test, exp, act, ListPair.all Real.==,
       ListFormat.listToString Real.toString)

  fun testEqIntVector (test : string, 
      exp : int vector, 
      act : unit -> int vector) : unit =
  let
    fun zip (v0 : int vector, v1 : int vector) : (int*int) vector =
      Vector.tabulate(Vector.length v0,
                      fn i => (Vector.sub(v0, i), Vector.sub(v1, i)))

    val toList = Vector.foldr op:: []
  in
    testEq (test, exp, act, (Vector.all op=) o zip,
      (ListFormat.listToString Int.toString) o toList)
  end
      


  fun testExn(test : string, t : unit -> 'a) : unit =
  let
    val () = print (test ^ "...")
    val _  = t()
  in
    print "FAILURE!\n"
  end

  fun runTests(cmd : string, args : string list) : int =
    (
      testEqRealList("negsToZero [1.0, ~1.0, 2.0, ~2.0, ~3.0]", 
        [1.0, 0.0, 2.0, 0.0, 0.0],
        fn () => Lab6.negsToZero([1.0, ~1.0, 2.0, ~2.0, ~3.0])) ;

      testEqReal("min [3.0, 1.0, 2.0]", 1.0,
        fn () => Lab6.min([3.0, 1.0, 2.0])) ;

      testEqRealList("f [1.0, ~1.0, 2.0, ~2.0, ~3.0]", [1.0],
        fn () => Lab6.f([1.0, ~1.0, 2.0, ~2.0, ~3.0])) ;

      testEqIntVector("squares 3", #[0, 1, 4], fn () => Lab6.squares(3)) ;

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
