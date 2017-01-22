(*  dblExp(x, i) = x^{2^i}.
  *)
  fun dblExp(x : real, i : int) : real =
      let
	  fun twotothe (n : int) : int =
	      case n of
		  0 => 1
                 |_ => 2 * twotothe(n - 1)
          val n = twotothe(i)
	  fun exp(x : real, n : int) : real = 
	      case n of
	        0 => 1.0
	       |_ => x * exp(x, n - 1) 
      in
          exp(x, n)
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