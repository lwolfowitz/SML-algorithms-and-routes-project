(*  A list-backed updateable priority queue.
*   
*   See priqueue.sig for documentation conventions.
*
*   N. Danner
*)

structure ListPQ : PRI_QUEUE =
struct

  (*  The type of an element in a priority queue.
  *   we write a < b to mean #2(a) < #2(b)
  *)
  type 'a elt = 'a * real

  (*  The type of a priority queue.
  *   Invariant:  if [x_0,...,x_{n-1}] : 'a pri_queue, then
  *   x_0 <= x_1 <= ... <= x_{n-1}.
  *
  *   Representations:  [x_0,...,x_{n-1}] represents
  *   <|x_0,...,x_{n-1}|>.
  *)
  type 'a pri_queue = ('a elt list)

  (*  Convenience abbreviation for this implementation.
  *)
  type 'a pq = 'a pri_queue

  (*  Exception raised when delMin or updatePri applied to an empty queue.
  *)
  exception Empty

  (*  Exception raised when updatePri applied to key that is not in the queue.
  *)
  exception BadKey

  (*  The empty queue.
  *)
  val empty : 'a pq = []

  (*  isEmpty q = true, if q = <| |>
  *             = false, otherwise.
  *)
  val isEmpty : 'a pq -> bool = null

  (*  insert (<|x_0,...|>, (x, q)) = <|x_0,...,x_{i-1},x,x_{i+1},...|>,
  *   where priority(x_{i-1}) <= q <= priority(x_i).
  *)
  fun insert (xs : 'a pq, (x, q) : 'a elt) : 'a pq =
  let
    (*  insertQ([(x_0,q_0),...(x_{n-1},q_{n-1})]) =
    *     <|(x_0,q_0),...,(x_{i-1},q_{i-1}), (x, q), (x_i,q_i),...|>
    *   where q_{i-1} <= q <= q_i.
    *)
    fun insertQ (xs : 'a elt list) : 'a pq =
      case xs of
           [] => [(x, q)]
         | (y, p) :: ys =>
             if p < q then (y, p) :: (insertQ ys)
             else (x, q) :: xs
  in
    insertQ xs
  end

  (*  insertMany (q, [(y_0,q_0),...(y_{n-1},q_{n-1})]) =
  *     insert(...insert(
  *       insert(q, (y_0, q_0)), (y_1, q_1))...,(y_{n-1},q_{n-1})).
  *)
  fun insertMany (xs : 'a pq, ys : 'a elt list) : 'a pq =
    foldr (fn (y, q) => insert(q, y)) xs ys

  (*  delMin (<|x_0,x_1,...|>) = (x_0, <|x_1,...|>).
  *)
  fun delMin (q : 'a pq) : 'a * ('a pq) =
    case q of
         [] => raise Empty
       | (x, _) :: xs => (x, xs)

  (*  updatePri(pq, x, p) = pq', where
  *     - keys(pq') = keys(pq)
  *)
  fun updatePri(q : ''a pq, x : ''a, p : real) : ''a pq =
    if isEmpty q then raise Empty
    else
      let
        (*  split [(y_0, p_0),...,(y_{n-1}, p_{n-1})] =
        *     ([(y_0, p_0),...,(y_{i-1}, p_{i-1})],
        *      [(y_{i+1},p_{i+1}),...,(y_{n-1},p_{n-1})]),
        *   where y_i = x.
        *   Raises BadKey if there is no i such that y_i = x.
        *)
        fun split (ys : ''a pq) : (''a pq)*(''a pq) =
          case ys of
               [] => raise BadKey
             | (y, q) :: ys =>
                 if  x = y then ([], ys)
                 else 
                 let
                   val (b, a) = split ys
                 in
                   ((y, q) :: b, a)
                 end
        val (b, a) = split q
      in
        insert(b @ a, (x, p))
      end

end

