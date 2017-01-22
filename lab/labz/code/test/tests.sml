(*  Tests for COMP 212 Lab 3.
*
*   Using this testing code.
*
*   When you downloaded and unpacked the coding framework, you should
*   have ended up with the following directory hierarchy:
*
*     code/
*       src/
*         hw1.sml
*       test/
*         tests.sml
*
*   This file is tests.sml.  You must write your solutions in a file
*   named hw1.sml in the directory src.
*
*   There are (at least) two ways to use this testing code.  One is to
*   compile the code and run the testing function from within the
*   SML interpreter.  To do so, start sml, compile the code using
*   CM.make, then evaluate Tests.runTests0().  You session should
*   looks something like the following:
*
*     $ cd code/test
*     $ sml
*     Standard ML of New Jersey v110.76 [built: Thu Aug 21 11:59:56 2014]
*     - CM.make "tests.cm" ;
*     [autoloading]
*     [library $smlnj/cm/cm.cm is stable]
*     [library $smlnj/internal/cm-sig-lib.cm is stable]
*     [library $/pgraph.cm is stable]
*     [library $smlnj/internal/srcpath-lib.cm is stable]
*     ...
*     [library $smlnj/internal/smlnj-version.cm is stable]
*     [loading (tests.cm):../src/(lab1.cm):lab1.sml]
*     [compiling (tests.cm):tests.sml]
*     [creating directory .cm/GUID]
*     tests.sml:88.12 Warning: calling polyEqual
*     [creating directory .cm/x86-unix]
*     [code: 3324, data: 444, env: 228 bytes]
*     [New bindings added.]
*     val it = true : bool
*     - Tests.runTests0() ;
*     Lab1.isLeapYear 1900...SUCCESS!
*     Lab1.isLeapYear 2000...SUCCESS!
*     Lab1.validDate 15 January 1986...SUCCESS!
*     Lab1.validDate 31 August 1986...SUCCESS!
*     val it = 0 : int
*
*   If there are syntax errors in your code, then these will be reported
*   immediately after the [loading (tests.cm):../src/(lab1.cm):lab1.sml] 
*   line.  If there are no syntax errors, then this line will be followed 
*   by a sequence of lines telling you what functions you have defined and 
*   the types of those functions.  Don't worry about the 'calling polyEqual' 
*   warning.  You should hopefully then have a sequence of lines that 
*   say SUCCESS! Each of those is one test that has passed.
*
*   A second way to test your code is to use the run-tests program that
*   is distributed with the coding framework.  run-tests is a shell script
*   that does almost the same thing, but using shell scripts instead of
*   typing into the interpreter.  A session using run-tests should
*   looks something like the following:
*
*     $ cd code/test
*     $ ./run-tests 
*
*     ***
*     Building tests...
*     ***
*     Standard ML of New Jersey v110.76 [built: Thu Aug 21 11:59:56 2014]
*     [library $smlnj-tdp/back-trace.cm is stable]
*     [library $smlnj-tdp/plugins.cm is stable]
*     [library $SMLNJ-LIB/Util/smlnj-lib.cm is stable]
*     ...
*     [library $smlnj/internal/smlnj-version.cm is stable]
*     [loading (tests.cm):../src/(lab1.cm):lab1.sml]
*     [compiling (tests.cm):tests.sml]
*     [creating directory .cm/GUID]
*     tests.sml:88.12 Warning: calling polyEqual
*     [creating directory .cm/x86-unix]
*     [code: 12104, data: 1840, env: 228 bytes]
*     [scanning 37378-export.cm]
*     [scanning (37378-export.cm):tests.cm]
*     [scanning (37378-export.cm):(tests.cm):../src/lab1.cm]
*     [parsing (37378-export.cm):37378-export.sml]
*     [compiling (37378-export.cm):37378-export.sml]
*     [code: 959, data: 79, env: 40 bytes]
*     
*     ***
*     Running tests...
*     ***
*     Lab1.isLeapYear 1900...SUCCESS!
*     Lab1.isLeapYear 2000...SUCCESS!
*     Lab1.validDate 15 January 1986...SUCCESS!
*     Lab1.validDate 31 August 1986...SUCCESS!
*   
*   If there are syntax errors in your code, then these will be reported
*   immediately after the [opening ../src/hw1.sml] line.  If there are
*   none, then you will again see a sequence of lines corresponding to
*   each test with an indication of success or failure.
*
*   The tests are the function calls of the form testEq.  The 
*   testEq function is defined near the top of the Tests structure.  It takes
*   three arguments.  The first is a string, which is used to identify
*   the test.  The second is the expected value, and the third is the
*   value your function evaluated to.  For example, the test
*
*     testEq ("Lab1.validDate (15, January, 1986)", true,
*              Lab1.validDate (15, "January", 1986)) ;
*
*   says that the result of Lab1.validDate (15, "January", 1986) should be true.
*   The testEq function will print the first argument, then check
*   whether the second and third are equal.  If so, testEq prints
*   SUCCESS! and otherwise prints FAILED.
*
*   This file only has a few tests.  You should absolutely write more,
*   because these few tests do not check for every possible kind of mistake
*   you might make in your implementation.  I will be testing your
*   submissions with a much more thorough collection of tests.
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

  fun testEqReal (test : string, exp : real, act : real) =
    testEq (test, exp, act, Real.==, Real.toString)

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
      testEqList("dup [1, 2, 3]", [1, 1, 2, 2, 3, 3], Lab3.dup([1, 2, 3])) ;

      testEqInt("length [5, 4, 3, 2]", 4, Lab3.length ([5, 4, 3, 2])) ;

      testEqInt("sum [2, 4, 8]", 14, Lab3.sum ([2, 4, 8])) ;

      testEqList("altPairs [1, 2, 3]", [2, 1, 3], Lab3.altPairs [1, 2, 3]) ;
      testEqList("altPairs [1, 2, 3, 4]", [2, 1, 4, 3], 
        Lab3.altPairs [1, 2, 3, 4]) ;

      testEqReal("thouPow(2.0)", powRec(2.0, 1000), Lab3.thouPow(2.0)) ;

      testEqReal("dblExp(2.0, 3)", 256.0, Lab3.dblExp(2.0, 3)) ;

      testEqInt("index([1, 2, 3], 10)", ~1, Lab3.index([1, 2, 3], 10)) ;
      testEqInt("index([10, 2, 3], 10)", 0, Lab3.index([10, 2, 3], 10)) ;
      testEqInt("index([1, 10, 3], 10)", 1, Lab3.index([1, 10, 3], 10)) ;

      0
    )

  fun runTests0() = runTests("", [])

end
