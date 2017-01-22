(*  COMP 212 Homework 4:  Graphs.
*   Hailey Sholty and Lee Wolfowitz
*   N. Danner
*)

structure Hw4Graph =
struct

  structure T = TextIO
  structure S = T.StreamIO

  (*  The type of a vertex.
  *)
  type vertex = int

  (*  The type of a graph.
  *)
  type graph = vertex list vector

  (*  fromEdgeList(n, es) =
  *     #[ [v_{00},...,v_{0,m_0}],...,[v_{n-1,0},...,v_{n-1,m_{n-1}] ], where
  *     - For each v_{i,j}, (i, v_{ij}) is an element of es;
  *     - If (u, v) is an element of es, then there is j such that
  *       v = v_{u, j};
  *     - If j <> j', then v_{i,j} <> v_{i, j'}.
  *   In other words, (n, es) is an edge-list representation of some graph g,
  *   and fromEdgeList(n, es) is the adjacency-list representation of g.
  *   Note that edges may be repeated in es.
  *)
  fun fromEdgeList (n : int, es : (vertex*vertex) list) : graph =
    let 
	fun separate (n : int, es : (vertex*vertex) list) : int list = 
	    case es of 
		[] => []
	      | e'::es' => if #1(e') = n 
			   then (#2(e'))::separate(n, es') 
			   else separate(n, es')
	fun combine (x, n, es) : (int list) list=
	    if x = n then [] else separate(x, es) :: combine(x+1, n, es)

    in
	Vector.fromList(combine(0, n, es))
    end

  (*  fromEdgeListFile(filename) = fromEdgeList(n, es), where
  *   filename is a text file of the form
  *     n
  *     u_0 v_0
  *     u_1 v_1
  *     ...
  *     u_{m-1} v_{m-1}
  *   That is, the first line of the file consists of a single non-negative
  *   integer n.  All remaining lines consist of two non-negative integers
  *   separated by whitespace, both of which are < n.  es is
  *   [(u_0, v_0),...,(u_{m-1}, v_{m-1})].
  *)

fun fromEdgeListFile (filename : string) : graph = 
    let
(* Spec: isWS(c) = true if c is a whitespace character and false otherwise
 *)
	fun isWS (c : char) : bool = 
	    case c of 
		#" " => true
	      | #"\t" => true
	      | #"\n" => true
	      | _ => false
	

    (*  skipWS(<c_0,c_1,...>) = <c_m,c_{m+1},...>, where
    *   isWS(c_i) for i<m and not isWS(c_m).
    *
    *   In other words, skipWS(ins) = ins', where ins' is the
    *   result of discarding the leading whitespace characters of ins.
    *)
	fun skipWS (cs : char list) : char list =
	    case cs of 
		[] => []
	      | c::cs' => 
		case isWS(c) of
		    false => c::cs'
		  | true => skipWS(cs')

(* Spec: convert(c) = i, where c = #"i" and i is an int 
*)
	fun convert (c : char) : int =
	    valOf(Int.fromString(Char.toString(c)))   
  
(* Spec: intstr([f_0, ..., f_{n-1}]) = "f_0 ^ ... ^ f_{m-1}" where for
 * all i<m, isWS(f_i) = false 
*)	    
	fun intstr (f : char list) : string  = 
	    case f of 
		[] => ""
	      | c::f' => if isWS(c) then "" else Char.toString(c)^(intstr(f')) 
					
(* Spec: intlist([#"f_0", ..., #"f_{n-1}"]) = [f_0, ..., f_{n-1}] where
 * f_0, ..., f_{n-1} are integers
*)	 
	fun intlist (f : char list) : int list = 
	    case f of 
		[] => []
	      | _  => 
		let
		    val i = intstr(f)
		    val f' = skipWS(List.drop(f, String.size(i)))
		in 
		    valOf(Int.fromString(i)) :: intlist(f')
		end 

		
(* Spec: read([f_0, ..., f_{n-1}]) = [(f_0, f_1), (f_2, f_3), ..., (f_{n-2}, f_{n-1})]
 * pre-condition: n is even 
 *)
	fun read (f : int list) : (vertex*vertex) list = 
	    case f of 
		[] => []
	      | c'::c''::f' => (c', c'') :: read(f')

	val int_list = intlist(explode(filename))
	val n = hd(int_list)
	val cs = tl(int_list)
    in 
	fromEdgeList(n, read(cs))

    end 
	      

  (*  isAcyclic(g) = true if g represents an acyclic graph,
  *                  false otherwise.
  *)
  fun isAcyclic (g : graph) : bool =
      let

(* Spec: empty(x, g) = true if g is empty or contains only empty lists and false otherwise.
 * Pre-condition: x always starts at 0
 *)
	  fun empty (x, g : graph) : bool =
	      if g = #[] then true else 
	      if x = Vector.length(g) then true else 
	      if Vector.sub(g, x) = [] then empty(x+1, g) else false 

(* Spec: updateList(n, [e_0,..., e_{i-1}, e_i, e_{i+1}, ..., e_{m-1}]) = 
 * [e_0, ..., e_{i-1}, e_{i+1}, ..., e_{m-1}] where e_i = n and m is the length of the
 * original list. 
 *)
	  fun updateList (n : vertex, es : vertex list) : vertex list = 
	      case es of 
		  [] => []
		| e::es' => if n = e then es' else e::(updateList(n, es'))

(* Spec: removeSink (n, x, g) = g such for all x < Vector.length(g), 
 *not(contains(Vector.sub(g, x)), n)
 *)						   
	  fun removeSink (n : vertex, x : int, g : graph) : graph = 
	      if x > Vector.length(g) - 1 then g else
	      if updateList(n, Vector.sub(g,x)) = Vector.sub(g, x) 
	      then removeSink(n, x+1, g)
	      else removeSink(n, x+1, Vector.update(g, x, updateList(n, Vector.sub(g, x))))
		 
(* Spec: findSink (x, g) = NONE if for all x < Vector.length(g), Vector.sub(g, x) != []
 * and SOME(x) if Vector.sub(g, x) = [] 
 *)
	  fun findSink (x: int, g : graph) : int option  =
	      if empty(0, g) then NONE else 
	      if x > Vector.length(g) - 1 then NONE else
	      if Vector.sub(g, x) = [] then SOME(x) else findSink(x+1, g)
			     
      in
	  case findSink(0, g) of 
	      NONE => if empty(0, g) then true else false 
	    | SOME(x) => isAcyclic(removeSink(valOf(SOME(x)), 0, g))
      end




fun isAcyclicLee (g : graph) : bool =
    let 
	val len = Vector.length(g)
	fun contains (x : int, ls : int list) : bool = 
	    case ls of 
		[] => false 
	      | n::ls' => if n = x then true else contains(x, ls)  
	fun isEmpty (x, g :graph) : bool = 
(* start x as 0, use as a counter *)
	    if g = #[] then true else 
	    if Vector.sub(g, x) = [] then isEmpty(x+1, g) else false 
(*
            case (g) of 
		[] => true
              |[]::es => isEmpty(es)
              |_::es => false  *)

	fun isNodeIrrel (n : int, x: int, g : graph) : bool =
(* again, x is a counter, start at 0 *)
	    if g = #[] then true else 
	    if contains(n, Vector.sub(g, x)) then false else isNodeIrrel(n, x+1, g)

(*            case (g) of
		[] => true
              |e::es => if List.contains(e, n) then false else isNodeIrrel(n, es) *)

	fun nodeRemover (n : int, cap : int, g : graph) : graph =
            if (n = cap) then g else 
            case (Vector.sub(g, n), isNodeIrrel(n, 0, g)) of
(* why Vector.sub(g,n)? *)
		([], _ )=> nodeRemover(n+1, cap, g)
              |(_, true) => nodeRemover(0, len, Vector.update(g, n, []))
              |(_, false) => nodeRemover(n+1, cap, g)
    in 
        isEmpty(0, nodeRemover(0, len, g))
    end
    




(**** These are just the functions in my fromEdgeListFile that I've been using
 **** to manually test my fromEdgeListFile code *)

  fun isWS (c : char) : bool = 
      case c of 
	  #" " => true
	| #"\t" => true
	| #"\n" => true
	| _ => false
	       
  fun skipWS (cs : char list) : char list =
      case cs of 
	  [] => []
	| c::cs' => 
	  case isWS(c) of
	      false => c::cs'
	    | true => skipWS(cs')
		      
   fun convert (c : char) : int =
	  valOf(Int.fromString(Char.toString(c)))   
		      
  fun intstr (f : char list) : string  = 
      case f of 
	  [] => ""
	| c::f' => if isWS(c) then "" else Char.toString(c)^(intstr(f'))
					   
  fun intlist (f : char list) : int list = 
      case f of 
	  [] => []
	| _  => 
	  let
	      val i = intstr(f)
	      val f' = skipWS(List.drop(f, String.size(i)))
	  in 
	      valOf(Int.fromString(i)) :: intlist(f')
	  end
	  
  fun read (f : int list) : (vertex*vertex) list = 
      case f of 
	  [] => []
	| c'::c''::f' => (c', c'') :: read(f')
	| c'::f' => []						       
		
end 
