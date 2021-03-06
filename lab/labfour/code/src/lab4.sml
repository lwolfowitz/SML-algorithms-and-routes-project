(*  COMP 212 Lab 4:  I/O.
*   N. Danner
*)

structure Lab4 =
struct

  structure S = TextIO.StreamIO

  (*  printXs(n) = ().  When invoked, printXs(n) prints 2^n "X"'s to
  *   the terminal followed by a newline.
  *
  *   Hint:  write an auxilliary function that prints 2^n "X"'s to the
  *   terminal without a newline.
  *)
  fun printXs(n : int) : unit =
    let
	fun expo(i :int): int =
	 case i of
	     0 => 1
	     |_ => 2 * expo(i -1)
	fun write (p :int): unit = 
	 case p of
	      0 => print("X")
	      |_=>(print("X"); write(p-1))
     in
       write(expo(n));
       print("\n")
     end
	      

  (*  readNumbers0(filename) = [n_0,...,n_{k-1}], where filename
  *   is a text file with one integer number per line in the order
  *   n_0, n_1,...,n_{k-1}.  If the file is empty, this function
  *   returns [].
  *
  *   Pre-condition:  each line of filename has consists of a string
  *   of the form s, where Int.fromString(s) does not raise
  *   an exception.  Note that Int.fromString automatically skips
  *   leading whitespace and parses the longest prefix of s that
  *   represents an integer.  In particular, you need not remove
  *   the trailing newline from s.
  *)
  fun readNumbers0(filename : string) : int list =
    let
	val ins = TextIO.getInstream(TextIO.openIn(filename))
	fun numreader(i : S.instream) : int list =
	 case S.inputLine(i) of
	       NONE => []
	       |SOME(c, ins') => valOf(Int.fromString(c))::numreader(ins')
     in
     numreader(ins)
    end
	 

  (*  readNumbers1(filename) = [n_0,...,n_{k-1}], where filename
  *   is a text file with one integer number per line in the order
  *   n_0, n_1,...,n_{k-1}.  filename may contain blank lines.  If
  *   the file is empty or contains only blank lines, this function
  *   returns [].
  *
  *   Pre-condition:  each line of filename has consists of a string
  *   of the form s, where Int.fromString(s) does not raise
  *   an exception, or is blank.
  *)
  fun readNumbers1(filename : string) : int list =
     let
	val ins = TextIO.getInstream(TextIO.openIn(filename))
	fun numreader(i : S.instream) : int list =
	 case S.inputLine(i) of
	       NONE => []
	       |SOME("\n", ins') => numreader(ins')
	       |SOME(c, ins') =>  valOf(Int.fromString(c))::numreader(ins')
     in
     numreader(ins)
    end

  (*  readNumbers2(filename) = [n_0,...,n_{k-1}], where filename
  *   is a text file with a sequence of integer numbers separated by
  *   whitespace characters (#" ", #"\t", or #"\n").
  *
  *   This function is a bit more involved than the previous two.
  *   It involves looking ahead on the stream, such as the implementation
  *   of inputLine on p. 70 of Gansner & Reppy.  You need not submit
  *   this function for the lab, but you will have to do something
  *   similar in the homework, so I recommend working on it if you
  *   have time.  I've provided the specifications of some auxilliary
  *   functions that I found useful in my solution.
  *)
  fun readNumbers2(filename : string) : int list =
  let
    (*  isWS(c) = true if c is a whitespace character, false otherwise.
    *)
    fun isWS(c : char) : bool =
      false

    (*  readAhead(<c_0,c_1,...>, n) = m+n, where isWS(c_m) and
    *   not isWS(c_i) for i < m.
    *
    *   In other words, readAhead(ins, n) = m+n, where m is the number
    *   of non-whitespace characters in ins.
    *)
    fun readAhead(ins : S.instream, n : int) : int =
      0

    (*  skipWS(<c_0,c_1,...>) = <c_m,c_{m+1},...>, where
    *   isWS(c_i) for i<m and not isWS(c_m).
    *
    *   In other words, skipWS(ins) = ins', where ins' is the
    *   result of discarding the leading whitespace characters of ins.
    *)
    fun skipWS(ins : S.instream) : S.instream =
      ins

  in
    []
  end

end
