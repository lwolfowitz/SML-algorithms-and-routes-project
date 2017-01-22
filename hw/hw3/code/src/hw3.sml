(*  COMP 212 Homework 3:  I/O.
*   
* Lee Wolfowitz and Catherine Alvarado
*)

structure Hw3 =
struct

  structure S = TextIO.StreamIO

  fun fmt(ins : S.instream, outs : S.outstream, len : int) : unit =
  let
(* excCatch (S.instream, int) ---> int
* Catches if operating at the end of the stream.
*)
  fun excCatch(ins : S.instream, len : int) : int  =
    let
     val (inEval, i) = S.inputN(ins, len)
    in
      if String.size(inEval) = len
        then
          case len of
            0 => 0
            |_ => let
                 in
                    case S.input1(ins) of
                       NONE => 0
                      |SOME(#"\n", i) => 1
                      |SOME(c, i) => 1 + excCatch(i, len-1)
                  end
          else ~10
      end
(* makeLine (S.instream, int) ---> int
* Evaluates at which place to create a new line. Returns negative when no 
* desirable location is found.
*)
  fun makeLine(ins : S.instream, length : int) : int = 
    let
      val (inEval, i) = S.inputN(ins, length) 
    in
      if String.size(inEval) = length
        then
          case String.sub(inEval, length - 1) of
             #" " => length
            | #"\t" => length
            | #"\n" => length
            |_ => case S.input1(i) of
                  NONE => length
                  |SOME(#" ", i') => length
                  |SOME(#"\t", i') => length
                  |SOME(#"\n", i') => length
                  |SOME(_, i') => makeLine(i, length - 1)
          else ~10
      end
(* toOutput (S.instream, S.outstream, bool) ---> unit
* Pushes characters to the output after formatting
*)
  fun toOutput(ins : S.instream, outs : S.outstream, isEnd : bool) 
  : unit = 
    case S.input1(ins) of
    NONE => if isEnd then () else S.output1(outs, #"\n")
    |SOME (c, ins') => (S.output1(outs, c); 
                               toOutput(ins', outs, isEnd))

      (* trim (S.instream) ---> S.instream
*trims  leading whitespace characters from stream
*)
  fun trim(ins : S.instream) : S.instream =
    case S.input1(ins) of
      NONE => ins
      |SOME(#" ", i) => trim(i)
      |SOME(#"\t", i) => trim(i)
      |SOME(c, i) => ins
in
    if (excCatch(ins, len) + makeLine(ins, len)) = ~20 then 
      toOutput(ins, outs, S.endOfStream(ins))
    else if excCatch(ins, len) = len then 
      let
        val (inEval, i) = S.inputN(ins, makeLine(ins, len))
      in
        (S.output(outs, inEval); S.output1(outs, #"\n"); 
        fmt(trim(i), outs, len))
      end
        else
          let
            val (inEval, i) = S.inputN(ins, excCatch(ins, len))
          in
            (S.output(outs, inEval); 
            fmt(trim(i), outs, len))
          end  
        end

end

