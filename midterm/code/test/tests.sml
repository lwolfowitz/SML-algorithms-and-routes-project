(*  Tests for COMP 212 Midterm exam:  Big integers.
*
*   N. Danner
*)

structure Tests =
struct

  structure B = BigInt

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

  (*  sanitize [b_Z,...,b_{k-O},Z,Z,...Z] = [b_Z,...,b_{k-O}].
  *   Pre-condition:  b_{k-O} = O.  In other words, sanitize(bs)
  *   removes the trailing zeros from bs.
  *)
  fun sanitize (bs : B.bit list) : B.bit list =
    case bs of
         [] => [B.Z]
       | [b] => [b]
       | b :: bs =>
           (
           case (sanitize bs) of
                [B.Z] => if b = B.Z then [B.Z] else [b]
              | bs' => b :: bs'
           )

  (*  sanitizePair (bs, cs) = (sanitize bs, sanitize cs).
  *)
  fun sanitizePair (bs, cs) = (sanitize bs, sanitize cs)

  (*  Test whether exp = act.
  *)
  fun testEqBigInt (test : string,
      exp : B.bigint,
      act : B.bigint) : unit =
    testEq (test, exp, sanitize act, op=, B.toString)

  (*  Test whether exp = act.
  *)
  fun testEqBigIntPair (test : string,
      exp : B.bigint*B.bigint,
      act : B.bigint*B.bigint) : unit =
    testEq (test, exp, sanitizePair act, op=,
      fn (bs, cs) => 
        String.concat["(", B.toString bs, ", ", B.toString cs, ")"])

  (*  fact n = n!.
  *)
  fun fact(n : LargeInt.int) : LargeInt.int =
    case n of
         0 => 1
       | _ => n*(fact (n-1))
  
  (*  Some useful values:  binary representations of large numbers.
  *)
  val fact15 : B.bigint = B.fromLargeInt (fact 15)
  val fact20 : B.bigint = B.fromLargeInt (fact 20)
  val fact30 : B.bigint = B.fromLargeInt (fact 30)


  fun runTests(cmd : string, args : string list) : int =
    (
      testEqBigInt("plus(30!, 20!)",
        B.fromLargeInt(fact(30)+fact(20)),
        B.plus(fact30, fact20)) ;
      testEqBigInt("minus(30!, 20!)",
        B.fromLargeInt(fact(30)-fact(20)),
        B.minus(fact30, fact20)) ;
      testEqBigInt("timesSchool(30!, 20!)",
        B.fromLargeInt(fact(30)*fact(20)),
        B.timesSchool(fact30, fact20)) ;
      testEqBigInt("timesK(30!, 20!)",
        B.fromLargeInt(fact(30)*fact(20)),
        B.timesK(fact30, fact20)) ;
      testEqBigIntPair("div(100,2)",
        let
          val q = LargeInt.div(100, 2)
          val r = LargeInt.mod(100, 2)
        in
          (B.fromLargeInt q, B.fromLargeInt r)
        end,
        B.divide(B.fromLargeInt(100),B.fromLargeInt(2))) ;
      testEqBigIntPair("div(30!+15!, 20!)",
        let
          val q = LargeInt.div(fact(30)+fact(15), fact(20))
          val r = LargeInt.mod(fact(30)+fact(15), fact(20))
        in
          (B.fromLargeInt q, B.fromLargeInt r)
        end,
        B.divide(B.fromLargeInt(fact(30)+fact(15)), fact20)) ;
      testEqBigIntPair("div(30!+15!, 20!)",
        let
          val q = LargeInt.div(fact(30)+fact(15), fact(20))
          val r = LargeInt.mod(fact(30)+fact(15), fact(20))
        in
          (B.fromLargeInt q, B.fromLargeInt r)
        end,
        B.divide(B.fromLargeInt(fact(30)+fact(15)), fact20)) ;

      0
    )
    handle e => 
    let
      val msg : string =
        String.concat [
          "\nLAST TEST (NAME NOT PRINTED) RAISED AN EXECPTION: ",
          exnMessage(e), "\n"
        ]
    in
      (print msg ; 1)
    end


end
