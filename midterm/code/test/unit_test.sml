(*
*   Unit-testing framework for SML.
*
*   N. Danner.
*
*)
structure UnitTest =
struct

  (*  Possible results of running a single test.
  *)
  datatype test_status = SUCCESS | FAIL | EXN

  (*  A test_result is a record with fields for the status of the test,
  *   a failure message, and traceback.
  *)
  type test_result = {status : test_status, info : string option,
                        tb : string option}

  (*  The type of a test.  A test consists of a name and a delayed
  *   function of test_result type.
  *)
  type test = string * (unit -> test_result)


  (*  statusToString st = a string representation of the status st.
  *)
  fun statusToString SUCCESS = "SUCCESS"
    | statusToString FAIL = "FAILURE"
    | statusToString EXN = "EXCEPTION"

  (*  failMsg name exp act = s where s is a short message describing
  *   the failure of test name that had expected value exp and actual
  *   value act.
  *)
  fun failMsg(name, exp, act) =
    String.concatWith " "
        ["FAILURE (", name, "): expected", exp, "; got", act, "."]

  (*  exnMsg name = s where s is a short message describing
  *   that test name raised an exception.
  *)
  fun exnMsg name =
    String.concatWith " "
        ["Failure (", name, "): raised exception; see backtrace."]

  (*  makeTestEq name test exp eq toStr = t, where t is a test.  The
  *   name of t is name.  If test : unit -> 'a, then we say that
  *   t is an 'a-type test.  When t is run (usually via runTest t),
  *   there are three possibilities, where we write t() when we
  *   really mean (#2 t)():
  *
  *   eq (exp, test()) = true; we say t succeeds.  t() evaluates
  *   to {SUCCESS, NONE, NONE}.
  *
  *   eq (exp, test()) = false; we say t fails.  t() evaluates
  *   to {FAIL, SOME info, NONE}, where info is an error message that
  *   identifies both the expected and actual value (exp and test(),
  *   respectively), converting both to strings with toStr.
  *
  *   test() raises an exception; we say t has an exception.  t()
  *   evaluates to {EXN, SOME info, SOME tb}, where info is an
  *   error message indicating that test() raises an exception and
  *   tb is a traceback for the exception.  Note that t() itself
  *   does not raise an exception in this case!
  *)
  fun makeTestEq (test_name : string, 
                 test : unit -> 'a, 
                 exp : 'a,
                 eq : 'a*'a -> bool,
                 to_str : 'a -> string) : test =
  let
    fun test_fn () =
    let 
      val tb = ref("") : string ref 
    in
      (
        Compiler.Control.Print.out := {
                                        say=fn s => tb := concat [!tb, s], 
                                        flush=fn() => ()
                                      } ;
        BackTrace.monitor(
        fn () =>
        let
          val act = test()
        in
          if eq(exp, act) then {status = SUCCESS, info = NONE, tb = NONE}
          else {status = FAIL, 
                info = SOME (failMsg(test_name, to_str(exp), to_str(act))), 
                tb = NONE}
        end
        ) 
        handle _ => 
          { status = EXN,
            info = SOME(exnMsg(test_name)),
            tb = SOME(!tb)
          }
      )
    end
  in
    (test_name, test_fn)
  end

  (*  assertEq name test exp toStr = t, where t is a test with name name
  *   that succeeds just in case test() = exp (i.e., this test constructor
  *   is for tests on equality types).
  *)
  fun assertEq (test_name : string, 
                    test : unit -> ''a, 
                    exp : ''a,
                    to_str : ''a -> string) : test =
    makeTestEq(test_name, test, exp, op=, to_str)

  (*  assertTrue name test = t, where t is a bool-type test with name name that
  *   succeed just in case test() = true.
  *)
  fun assertTrue(name : string,
                  test : unit -> bool) : test =
    assertEq(name, test, true, Bool.toString)

  (*  assertTrue name test = t, where t is a bool-type test with name name that
  *   succeed just in case test() = false.
  *)
  fun assertFalse(name : string,
                  test : unit -> bool) : test =
    assertEq(name, test, false, Bool.toString)

  (*  assertTrueMsg name test = t, where t is a bool-type test with name 
  *   name that succeeds just in case #1(test()) = true.  If the test fails,
  *   then #2(test()) is reported as the message.
  *)
  fun assertTrueMsg(name : string,
                    test : unit -> bool*string) : test =
    makeTestEq(name,
        test,
        (true, "true"),
        fn (z1, z2) => (#1(z1) = #1(z2)),
        fn (_, s) => s)

  (*  assertTrueMsg name test = t, where t is a bool-type test with name 
  *   name that succeeds just in case #1(test()) = false.  If the test fails,
  *   then #2(test()) is reported as the message.
  *)
  fun assertFalseMsg(name : string,
                    test : unit -> bool*string) : test =
    makeTestEq(name,
        test,
        (false, "false"),
        fn (z1, z2) => (#1(z1) = #1(z2)),
        fn (_, s) => s)

  (*  assertEqInt name test exp = t, where t is an int-type test
  *   with name name that succeeds just in case test() = exp.
  *)
  fun assertEqInt (test_name : string,
                     test : unit -> int,
                     exp : int) : test =
    assertEq(test_name, test, exp, Int.toString)

  (*  assertEqStr name test exp = t, where t is an string-type test
  *   with name name that succeeds just in case test() = exp.
  *)
  fun assertEqStr (name : string, test : unit -> string, exp : string) : test =
    assertEq(name, test, exp, String.toString)

  (*  assertEqReal name test exp = t, where t is an real-type test
  *   with name name that succeeds just in case test() = exp.  Equality
  *   of expected and actual value is tested with Real.==.
  *)
  fun assertEqReal (test_name : string,
                      test : unit -> real,
                      exp : real) : test =
    makeTestEq(test_name, test, exp, Real.==, Real.toString)

  (*  assertAlmostEqReal (name, test, exp, err) = t, where t is a
  *   real-type test that succeeds when |test() - exp| < err.
  *)
  fun assertAlmostEqReal (name, test, exp, err) =
    makeTestEq(name, test, exp, fn (x, y) => Real.<(Real.abs(x-y), err),
               Real.toString)


  (*  assertEqList name test exp toStr = t, where t is an ''a-list type test
  *   with name name that succeeds just in case test() = exp.  toStr
  *   is used to convert list items to strings on failure.
  *)
  local
    (*  listToString itemToString [x_0,...,x_n] =
    *     "[s_0,...,s_n]" where s_i = itemToString x_i.  Empty and
    *     one-element lists are handled properly.
    *)
    fun listToString itemToString xs =
      concat ["[", String.concatWith "," (map itemToString xs), "]"]
  in
    fun assertEqList (test_name, test, exp, itemToString) =
      assertEq(test_name, test, exp, listToString itemToString)
  end

end

structure TestRunner =
struct

  structure U = UnitTest
  open U

  val maxMsgs = 5

  (*  runTest test = (name, result), where name is the name of test
  *   and result is the test_result that describes the result of running
  *   the test.
  *)
  fun runTest (test : test) : string*test_result =
  let
    val (test_name, test_fn) = test
    val () = print (concat [test_name, "..."])
    val result = test_fn()
    val () = print (concat [statusToString (#status(result)), "\n"])
  in
    (test_name, result)
  end

  fun printV (verbose : bool) (s : string) : unit =
    if verbose then print s else ()

  fun runTest (verbose : bool) (test : test) : string*test_result =
  let
    val (test_name, test_fn) = test
    val () = printV verbose (concat [test_name, "..."])
    val result = test_fn()
    val () = printV verbose (concat [statusToString (#status(result)), "\n"])
  in
    (test_name, result)
  end

  (*  printList ss prints each string s that occurs as SOME(s) in ss.
  *)
  (*
  fun printList ([] : string option list) : unit = ()
    | printList ((SOME s)::ss) = (print (s ^ "\n") ; printList ss)
    | printList (NONE::ss) = printList ss
  *)
  fun printList ss =
  let
    val realMsgs = List.filter (fn x => x <> NONE) ss
    val msgs = 
      if maxMsgs = ~1 then 
        realMsgs 
      else 
        List.take(realMsgs, Int.min(length realMsgs, maxMsgs))
    val msgsS = ListFormat.fmt 
      {init="", sep="\n", final="", fmt=(fn s => (valOf s))} 
      msgs
    val msgsS =
      if length msgs < length realMsgs then
        msgsS ^ "\n***ERROR MESSAGES TRUNCATED***\n"
      else
        msgsS
  in
    print msgsS
  end

  (*  print tbs [(name_1, tb_1), (name_2, tb_2),...] prints each name_i
  *   and then tb_i for each non-NONE tb_i.  The intent is that
  *   if tb_i is not NONE, then its value is a traceback for the
  *   test name_i.
  *)
  fun printTbs (tbs) =
  let
    val tbs = List.filter (fn (_, x) => x <> NONE) tbs
    val realTbs =
      if maxMsgs = ~1 then
        tbs
      else
        List.take(tbs, Int.min(length tbs, maxMsgs))
    val tbMsgs = ListFormat.fmt
      {init="", 
        sep="\n", final="", 
        fmt=(fn (name, tb) => concat [name, "\n", valOf tb])}
      realTbs
    val tbMsgs =
      if length realTbs < length tbs then
        tbMsgs ^ "\n***TRACEBACKS TRUNCATED***\n"
      else
        tbMsgs
  in
    print tbMsgs
  end

  (*  runTests [t_1,t_2,...,t_n] = 0 if every test t_i is successful.
  *                               = 1 if some test t_i is not successful.
  *   As a side-effect, the tests t_1, t_2,... are run in order,
  *   and messages are printed to the terminal indicate success,
  *   failure, or exception.  One summary line is printed per test,
  *   and then failure messages and tracebacks are printed after
  *   the summary lines.
  *)
  fun runTests (tests : test list, verbose : bool) : int =
  let
    val rt = Timer.startRealTimer ()
    val names_results = map (runTest verbose) tests
    val elapsed = Timer.checkRealTimer rt
    val n_succ = length (List.filter 
          (fn nr => case nr of (_, {status=SUCCESS,...}) => true 
                             | _ => false) 
          names_results)
    val n_fail = length (List.filter 
          (fn nr => case nr of (_, {status=FAIL,...}) => true 
                             | _ => false) 
          names_results)
    val n_exn = length (List.filter 
          (fn nr => case nr of (_, {status=EXN,...}) => true 
                             | _ => false) 
          names_results)
    val failMsgs = map (#info o #2) names_results
    val tbs = map (fn nr => (#1(nr), (#tb o #2)(nr))) names_results
    val return = n_fail + n_exn
  in
    (
      print "\n\n" ; 
      print (concat ["Ran ", Int.toString(length(tests)), " tests in about ",
        IntInf.toString (Time.toMilliseconds elapsed), " ms.\n"]) ;
      print (concat ["Passed: ", Int.toString(n_succ), "; ",
                     "Failed: ", Int.toString(n_fail), "; ",
                     "Exceptions: ", Int.toString(n_exn), "."]) ;
      print "\n\n" ;
      printList failMsgs ;
      print "\n\n" ; 
      printTbs tbs ;
      return
    )
  end

  fun runTestSuites(tests: test list list, verbose : bool) =
    case tests of
         [s] => runTests(s, verbose)
       | s :: ss => runTests(s, verbose) + runTestSuites(ss, verbose)

  fun runTimedTests(tests : test list, waitTime : int, verbose : bool) =
  let
    val dieMsg = String.concatWith " "
      ["\n\nRunning tests taking >", Int.toString waitTime, "seconds;",
       "assuming failure and terminating.\n"]
    fun die (s, n, k) =
    let
      val () = print dieMsg
    in
      OS.Process.exit(OS.Process.failure)
    end
    val _ = Signals.setHandler(Signals.sigALRM, Signals.HANDLER(die))
    val _ =
      SMLofNJ.IntervalTimer.setIntTimer(SOME(Time.fromSeconds(IntInf.fromInt
      waitTime)))
  in
    runTests(tests, verbose)
  end

  fun runTimedTestSuites(tests: (string *(test list)) list, 
                         waitTime : int, 
                         verbose : bool) =
  let
    fun runTestSuites (tests : (string*(test list)) list) =
      case tests of
           [(name, s)] => 
           let
             val () = print ("Suite: " ^ name ^ "\n")
           in
             runTests(s, verbose)
           end
         | (name, s) :: ss => 
             let
               val () = print ("Suite: " ^ name ^ "\n")
               val thisRet = runTests(s, verbose)
               val restRet = runTestSuites(ss)
             in
               thisRet + restRet
             end

    val dieMsg = String.concatWith " "
      ["\n\nRunning tests taking >", Int.toString waitTime, "seconds;",
       "assuming failure and terminating.\n"]
    fun die (s, n, k) =
    let
      val () = print dieMsg
    in
      OS.Process.exit(OS.Process.failure)
    end
    val _ = Signals.setHandler(Signals.sigALRM, Signals.HANDLER(die))
    val _ =
      SMLofNJ.IntervalTimer.setIntTimer(SOME(Time.fromSeconds(IntInf.fromInt
      waitTime)))
    val ret = runTestSuites tests
  in
    (
      print "Tests completed.\n" ;
      print (String.concat [Int.toString ret, " failures and exceptions.\n"]) ;
      ret
    )
  end


end

