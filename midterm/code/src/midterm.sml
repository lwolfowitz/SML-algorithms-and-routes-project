(*  COMP 212 Midterm exam:  Big integers.
*   
*   N. Danner
* By Lee Wolfowitz
* Source(s) Used: 
* Elements of ML Programming
*
*
*)

structure BigInt = 
struct

  (* **********
  *  Do not change any of the following code, and do not redefine
  *  any types or functions defined here.
  *)

  exception Undefined
  exception Neg

  datatype bit = Z | O
  type bigint = bit list

  (*  toLargeInt n = m, where m is the LargeInt.int represented by n.
  *)
  fun toLargeInt (n : bigint) : LargeInt.int =
      case n of
           [] => 0
         | (Z :: bs) => 2*(toLargeInt bs)
         | (O :: bs) => 2*(toLargeInt bs) + 1

  (*  fromLargeInt n = bs, where bs is the binary representation of n.
  *   bs is guaranteed to be non-empty and have no trailing Z's (i.e.,
  *   viewed as a binary numeral, has no leading zeros).
  *)
  fun fromLargeInt (n : LargeInt.int) : bigint =
  let
    fun toBit (b : Int.int) : bit =
      case b of
           0 => Z
         | 1 => O
  in
    case n of
         0 => [Z]
       | 1 => [O]
       | _ => (toBit (LargeInt.toInt(n mod 2))) :: (fromLargeInt (n div 2))
  end

  (*  bitToString Z = "0"
  *               O = "1"
  *)
  fun bitToString (b : bit) : string =
    case b of
         Z => "0"
       | O => "1"

  (*  toString bs = s, where s is a string representation of the number
  *   represented by bs.  s has the most-significant bit (i.e., last
  *   element of bs) first.  s will display all zeros, even trailing
  *   zeros in bs.  s = "[]" if bs = [] so as to distinguish a 
  *   the representation [] from [0].
  *)
  fun toString (bs : bigint) : string = 
    case bs of
         [] => "[]"
       | _ =>
          ListFormat.fmt
            {init="", sep="", final="", fmt=bitToString} (rev bs)

  (*  fromString s = bs, where bs is the representation of the number
  *   represented by s in binary.
  *)
  fun fromString (s : string) : bigint =
    fromLargeInt(valOf (LargeInt.fromString s))

  (*  **********
  *   Add your code below.
  *)

  (*  The specifications of the following functions make use of two
  *   undefined functions, toInt and fromInt.  These two functions
  *   are for use in the specifications and proofs of correctness
  *   only.  You are not to implement these functions.  The specifications
  *   for these functions are as follows:
  *
  *   toInt([])                = 0
  *   toInt([b_0,...,b_{n-1}]) = b_{n-1}*2^{n-1} + ... + b_1*2^1 + b_0
  *
  *   fromInt(n) = bs, where toInt(bs) = n.
  *
  *   I.e., toInt(bs) is the integer represented by bs, and fromInt(n)
  *   is a bit list that represents n.
  *
  *   Thus, when we write, e.g.,
  *
  *     plus(x, y) = fromInt(toInt(x) + toInt(y))
  *
  *   the x and y stand for bit lists, and the specification says
  *   (in words):  plus(x, y) = bs, where bs is a bit list that
  *   represents toInt(x)+toInt(y).
  *)


  (*  plus(x, y) = fromInt(toInt(x) + toInt(y)).
  *)
  fun plus(x : bigint, y : bigint) : bigint =
    case (x, y) of
      (xs,[]) => xs
      | ([], ys) => ys
      | (Z::xs, Z::ys) => Z::(plus(xs, ys))
      | (Z::xs, O::ys) => O::(plus(xs, ys))
      | (O::xs, Z::ys) => O::(plus(xs, ys))
      | (O::xs, O::ys) => Z::(plus(plus(xs,[O]), ys))

  (*  minus (x, y) = fromInt(toInt(x) - toInt(y)).  Pre-cond:  x>=y.
  *)
  fun minus(x : bigint, y : bigint) : bigint =
    case (x, y) of
      (xs, []) => xs
      |([], _) => []
      | (Z::xs, Z::ys) => Z::(minus(xs, ys))
      | (Z::xs, O::ys) => O::(minus(minus(xs, [O]), ys))
      | (O::xs, Z::ys) => O::(minus(xs, ys))
      | (O::xs, O::ys) => Z::(minus(xs, ys))
  (*  timesSchool(x, y) = fromInt(toInt(x)*toInt(y)).
  *   
  *   The implementation of timesSchool should be the implementation you
  *   get directly from our basic strategy for designing recursive
  *   functions.  It should correspond to your usual "school-method"
  *   of multiplying numbers.
  *)
  fun timesSchool(x : bigint, y : bigint) : bigint =
    case (x, y) of
      (xs, []) => []
      |([], ys) => []
      | (Z::xs, ys) => Z:: timesSchool(xs, ys)
      | (O::xs, ys) => plus(Z::timesSchool(xs,ys), ys)

 (* bestSplit(n, m) = n if (2*n <= m), m if (2*m <= n), 
  * (m div 2) if (n <= m), and n div 2 otherwise *)
  fun bestSplit(n : int , m : int ) : int = 
       if 2*n <= m then n
       else if 2*m <= n then m
       else if n <= m then m div 2
       else n div 2
  (* shift (x, y) = x*2^y *)
  fun shift(x : bigint, y : int) : bigint =
        case (x,y) of
           (a,0)=>a
          |(a,b)=>Z::shift(a,b-1)
  (* carve (x, y) = ([x_0, ..., x_y-1], x/(2^y)*)
  fun carve(x : bigint, y : int): bigint * bigint =
      case (x,y) of
        (p,0)=>([],p)
        |(p::ps,n)=> let 
                        val (qs, rs)= carve(ps,n-1)
                     in
                        (p::qs, rs)
                     end
  (*  timesK(x, y) = fromInt(toInt(x)*toInt(y)).
  *   
  *   The implementation of timesK must be your implementation of
  *   Karatsuba multiplication.
  *)
  fun timesK(x : bigint, y : bigint) : bigint = 
    case (x,y) of
     ([],y)=>[]
     |(x,[])=>[]
     |(x,y::[])=>timesSchool(x,[y])
     |(x::[],y)=>timesSchool(y,[x])
     |(x,y)=>
       let 
         val n = length(x)
         val m = length(y)
         val s = bestSplit(n,m)
         val (T,U)= carve(x,s)
         val (V,W)= carve(y,s)
         val TV= timesK(T,V)
         val UW= timesK(U,W)
         val TUVW = timesK(plus(T,U), plus(V,W))
         val middle = minus(minus(TUVW, TV), UW)
       in
         plus(plus(TV,shift(middle,s)),shift(UW,2*s))
       end

  (*  divide (x, y) = (fromInt(q), fromInt(r)), where 
  *   toInt(x) = q*toInt(y) + r.
  *
  *   In other words, divide(x, y) = (w, z), where w and z represent
  *   the (trunctaed) quotient and remainder of toInt(x)/toInt(y),
  *   respectively.
  *)
    fun divide(x : bigint, y : bigint) : bigint*bigint =
      let 
      (* isItZero (x) = true, where toInt(x) = 0, otherwise false *)
         fun isItZero (x :bigint) : bool = 
          case (x) of
            ([]) => true
            |(Z::xs)=> isItZero(xs)
            |(O::xs)=> false
      (* minus2 (x, y) = fromInt(toInt(x) - toInt(y)). Raise exception if x < y *)
        fun minus2(x : bigint, y : bigint) : bigint =
          case (x, y) of 
            (xs, []) => xs
            |([], ys) => if isItZero(ys) then [] else raise Neg
            | (Z::xs, Z::ys) => Z::(minus2(xs, ys))
            | (Z::xs, O::ys) => O::(minus2(minus2(xs, [O]), ys))
            | (O::xs, Z::ys) => O::(minus2(xs, ys))
            | (O::xs, O::ys) => Z::(minus2(xs, ys))
      (* divide2 (x, y, q, r) = (fromInt(q), fromInt(r)), where 
        *   toInt(x) = q*toInt(y) + r.
    *)
        fun divide2 (x : bigint, y : bigint, q : bigint, r : bigint) : bigint*bigint =
          case rev(x) of
            [] => (q,r)
            | b :: bs => divide2 (rev(bs), y, O::q, minus2(b::r, y))
                  handle Neg => divide2(rev(bs), y, Z::q, b::r)
      in
        divide2(x, y, [Z], [Z])
end
end
