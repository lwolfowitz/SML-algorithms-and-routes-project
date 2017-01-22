(*  Tests for COMP 212 Homework 3.
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure S = TextIO.StreamIO

  (*  checkFmt(s, n, exp) = t, where t is a test that performs essentially the
  *   following:
  *   - Creates a file with contents s.
  *   - Opens the file for reading to an S.instream ins.
  *   - Opens a temporary file for writing to an S.outstream outs.
  *   - Calls Hw3.fmt(ins, outs, n).
  *   - Reads the contents of the temporary file to a string s'.
  *   - t succeeds if s' = exp and fails otherwise.
  *)
  fun checkFmt (s : string, n : int, exp : string) =
  let
    val name = String.concat ["fmt(", s, ", ", Int.toString n, ")"]

    fun test() : string =
    let
      val ins = TextIO.getInstream(TextIO.openString s)
      val outfile = OS.FileSys.tmpName()
      val outs = TextIO.getOutstream(TextIO.openOut(outfile))
      val () = Hw3.fmt(ins, outs, n)
      val () = S.closeIn ins
      val () = S.closeOut outs
      val (res, _) =
      let
        val ins = TextIO.getInstream(TextIO.openIn outfile)
      in
        S.inputAll(ins) before S.closeIn ins
      end
    in
      res
    end
  in
    U.assertEqStr(name, test, exp)
  end

  val fmtTests = ("fmt tests", [
    checkFmt("ab cd  ef gh", 5, "ab cd\nef gh\n")
  ])

  val allTests = [fmtTests]

  fun runTests(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests, 30, true)


end
