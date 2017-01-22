(*  COMP 212 Lab 3:  case expressions and let expressions.
*   N. Danner
*)

structure Lab3 =
struct

  (*  length [x_0,...,x_{n-1}] = n.
  *)
  fun length (xs : int list) : int =
    case xs of
    	  nil => 0
	 |y::ys => 1 + length(ys)

  (*  sum [x_0,...,x_{n-1}] = x_0 + x_1 + ... + x_{n-1}.
  *)
  fun sum (xs : int list) : int =
    case xs of
    	  nil => 0
	 |y::ys => y + sum(ys) 

  (*  dup [x_0,...,x_{n-1}] = [x_0, x_0, x_1, x_1,...,x_{n-1}, x_{n-1}].
  *)
  fun dup (xs : int list) : int list =
    case xs of
    	 nil => []
	|y::ys => y::y:: dup(ys)

  (*  altPairs [x_0,,...,x_{n-1}] 
  *   = [x_1, x_0, x_3, x_2,..., x_{n-1}, x_{n-2}], n even
  *   = [x_1, x_0, x_3, x_2,..., x_{n-2}, x_{n-3}, x_{n-1}], n odd.
  *   In other words, altPairs(xs) is the list obtained by flipping
  *   consecutive pairs of values in xs.  If length(xs) is odd, then
  *   the last element of xs is left in place.
  *)
  fun altPairs (xs : int list) : int list =
    case xs of
    	 nil => []
	 |[y] => [y]
	 |y::z::ys => z::y::altPairs(ys)

  (*  thouPow(x) = x^{1000}.  Write this function non-recursively
  *   using a few let-bindings.
  *)
  fun thouPow(x : real) : real =
    let 
    	val ten = x*x*x*x*x*x*x*x*x*x
	val hun = ten*ten*ten*ten*ten*ten*ten*ten*ten*ten
    in
	hun*hun*hun*hun*hun*hun*hun*hun*hun*hun
    end

 (*  dblExp(x, i) = x^{2^i}.
  *)
  fun dblExp(x : real, i : int) : real =
      let
	  fun twopow (n : int) : int =
	      case n of
		  0 => 1
                 |_ => 2 * twopow(n - 1)
          val n = twopow(i)
	  fun expo(x : real, n : int) : real = 
	      case n of
	        0 => 1.0
	       |_ => x * expo(x, n - 1) 
      in
          expo(x, n)
      end 

  (*  index([x_0,...,x_{n-1}], x)
  *   = k, if x_k = x and x_i <> x for all i < k.
  *   = ~1, if there is no k such that x_k = x.
  *)
  fun index(xs : int list, x : int) : int =
    case xs of 
	nil => ~1
        |y::ys => if y = x then 0 else 
	    let
		val i = index(ys, x) 
	    in
		if i = ~1 then ~1
	        else 1 + i
            end
end
