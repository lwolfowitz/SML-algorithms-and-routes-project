(*  COMP 212 Homework 2:  Recursive functions.
*   N. Danner
*)

structure Hw2 =
struct

  (*  This structure defines a type of maps from keys of type
  *   (char list)*(char list) to values of type int.  It is basically
  *   a wrapper around a module defined in the SML/NJ library, wherein
  *   I have modified some of the functions to avoid some ML constructs
  *   that we haven't learned about yet.  The main point is that
  *   it defines for you a type (Map.map) of such maps, along with
  *   functions insert, contains, and find.  See the specifications
  *   of these functions below.  If m is such a map, I write
  *   m[s] for the value of m associated to the key s.
  *)
  structure Map =
  struct

    structure CharListPairKey : ORD_KEY =
    struct
      type ord_key = (char list)*(char list)

      fun lex (xs : char list, ys : char list) : order =
        case (xs, ys) of
             ([], []) => EQUAL
           | ([], _) => LESS
           | (_, []) => GREATER
           | (x :: xs, y :: ys) =>
               case Char.compare(x, y) of
                    GREATER => GREATER
                  | LESS => LESS
                  | EQUAL => lex(xs, ys)

      fun compare((s1, s2) : ord_key, (s1', s2') : ord_key) : order =
        case lex(s1, s1') of
             LESS => LESS
           | GREATER => GREATER
           | EQUAL => lex(s2, s2')
    end

    structure M = SplayMapFn(CharListPairKey)

    (*  The type of maps from (char list)*(char list) keys to int values.  If
    *   m : map, then we write m[s] for the value of m on key s.
    *)
    type map = int M.map

    (*  empty is an empty map.
    *)
    val empty : map = M.empty

    (*  insert (m, s, n) = m', where
    *     m'[s'] = m[s'], s' <> s
    *              n,     s' = s
    *)
    val insert : (map * ((char list)*(char list)) * int) -> map = M.insert

    (*  contains (m, s) = true if s is a key in m, false otherwise.
    *)
    fun contains (m : map, s : ((char list)*(char list))) : bool =
      case M.find(m, s) of
           NONE => false
         | SOME _ => true

    (*  find (m, s) = m[s].  Pre-condition:  contains(m, s) = true.
    *)
    fun find (m : map, s : ((char list)*(char list))) : int =
      valOf (M.find(m, s))

  end


 structure EditDistRec =
  struct

    (*  editDist (u, v) = the edit distance from u to v, as defined in
    *   the assignment.
    *)
    fun editDist(u : string, v : string) =
    let
      val us : char list = String.explode (u)
      val vs : char list = String.explode (v)
      (*  editDistRec(us, vs) = ed(implode us, implode vs), where ed(s, t)
      *   is the edit distance between u and v.
      *)
      fun editDistRec(us : char list, vs : char list) : int =
        case (us, vs) of
              ([], []) => 0
            | ([], vo :: v') => List.length(vs) (*this is just length of vs, which better?*)
            | (uo :: u', []) => List.length(us) (*just length of us*)
            | (uo :: u', vo :: v') => if (uo = vo) 
                                      then editDistRec(u', v')
                                      else
                                        let
                                          val a : int = editDistRec(us, v')
                                          val b : int = editDistRec(u', vs)
                                          val c : int = editDistRec(u', v')
                                        in
                                          1 + Int.min(Int.min(a,b),c)
                                        end
    in
      editDistRec(us, vs)
    end
  end
          (*
        if (us = vs) then 0 else
          if (List.length(us) = 0) then List.length(vs) else
            if(List.length(vs) = 0) then List.length (us) else
              if hd (us) = hd(vs) 
                then editDist(String.implode(tl us), String.implode(tl vs)) 
                else
                  1 + Int.min(Int.min (editDist(u, String.implode(tl vs)), 
                    editDist(String.implode(tl us),v)), 
                    editDist(String.implode(tl us),String.implode(tl vs)))
                  *)

  structure EditDistMem =
  struct

    fun editDist(u : string, v : string) : int = 
      let
        val us : char list = String.explode (u)
        val vs : char list = String.explode (v)
        fun EditDistMem(us : char list, vs : char list, mem : Map.map) : Map.map = 
          if Map.contains(mem, (us,vs)) then mem
          else 
            case (us, vs) of
                  ([], []) => Map.insert(mem, ([],[]), 0)
                | ([], vo :: v') => Map.insert(mem, ([], vs), List.length(vs))
                | (uo :: u', []) => Map.insert(mem, (us, []), List.length(us))
                | (uo :: u', vo :: v') => 
                    if (uo = vo) then
                      let
                        val mem' : Map.map = EditDistMem(u', v', mem)
                        val ed : int = Map.find(mem', (u', v'))
                      in
                        Map.insert(mem', (us, vs), ed)
                      end
                    else
                      let
                        val mem1 : Map.map = EditDistMem(u', v', mem) 
                        val mem2 : Map.map = EditDistMem(us, v', mem1)
                        val mem3 : Map.map = EditDistMem(u', vs, mem2)
                        val a : int = Map.find(mem3, (u', v'))
                        val b : int = Map.find(mem3, (us, v'))
                        val c : int = Map.find(mem3, (u',  vs))
                      in
                        Map.insert(mem3, (us,vs), 1 + Int.min(Int.min(a, b), c))
                      end
        val mem = EditDistMem(us, vs, Map.empty)
      in
        Map.find(mem, (us,vs))
      end
  end


  structure EditDistances =
  struct

    (*  editDistances [w_0,...,w_{n-1}] =
    *     [((w_0,w_1),d_{01}), ((w_0,w_2),d_{02}),...,((w_0,w_{n-1}),d_{0,n-1}),
    *     ((w_1,w_2),d_{12}), ((w_1,w_3),d_{13}),...,((w_1,w_{n-1}),d_{1,n-1}),
    *     ...,
    *     ((w_{n-2}, w_{n-1}), d_{n-2,n-1})],
    *   where d_{ij} = editDist(w_i, w_j).
    *
    *   In other words, editDistances [w_0,...,w_{n-1}] returns the list
    *   of all triples of the form ((w_i, w_j), d_{ij}) where 0 <= i < j <= n-1
    *   and d_{ij} = ed(w_i, w_j).  The order in which the triples
    *   appear is not important.
    *)
    fun editDistances(ws : string list) : ((string*string)*int) list =
      []

  end

end
