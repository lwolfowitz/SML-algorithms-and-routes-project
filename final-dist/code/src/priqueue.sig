(*  COMP 212 final exam:  a priority queue signature.
*   
*   N. Danner
*
*   This is a signature for priority queues, in which each element
*   of the priority queue has a real-valued priority.  Lower-value
*   priorities are higher priority.
*
*   Documentation conventions:
*     - priority(x) is the priority of x.
*     - <|x_0,...,x_{n-1}|> is a length-n priority queue with keys
*       x_0,...,x_{n-1}, where priority(x_0) <= ... <= priority(x_{n-1}).
*     - <|(x_0,p_0),...,(x_{n-1}, p_{n-1})|> is a length-n priority
*       queue with keys x_0,...,x_{n-1}, where priority(x_i) = p_i.
*     - <|x_0,...|> and <|(x_0,p_0),...|> are priority queues of some
*       length, where the length is not relevant to the specification.
*
*   In other words, if we don't want to mention the priorities explicitly,
*   we write <|x_0,...|>, and if we do we write <|(x_0,p_0),...|>.
*)

signature PRI_QUEUE =
sig

  (*  The type of a priority queue with elements of type 'a.
  *)
  type 'a pri_queue

  (*  Exception raised when delMin or updatePri applied to an empty queue.
  *)
  exception Empty

  (*  Exception raised when updatePri applied to key that is not in the queue.
  *)
  exception BadKey

  (*  An empty priority queue.
  *)
  val empty : 'a pri_queue

  (*  isEmpty q = true, if q = <| |>
  *             = false, otherwise
  *)
  val isEmpty : 'a pri_queue -> bool

  (*  insert (<|x_0,...|>, (x, q)) = <|x_0,...,x_{i-1},x,x_{i+1},...|>,
  *   where priority(x_{i-1}) <= q <= priority(x_i).
  *)
  val insert : 'a pri_queue * ('a*real) -> 'a pri_queue

  (*  insertMany (q, [(y_0,q_0),...(y_{n-1},q_{n-1})]) =
  *     insert(...insert(
  *       insert(q, (y_0, q_0)), (y_1, q_1))...,(y_{n-1},q_{n-1})).
  *)
  val insertMany : 'a pri_queue * ('a*real) list -> 'a pri_queue

  (*  delMin (<|x_0,x_1,...,x_{n-1}|>) = (x_0, <|x_1,...|>).
  *   
  *   Raises Empty if n = 0.
  *)
  val delMin : 'a pri_queue -> 'a * 'a pri_queue

  (*  updatePri(pq, x, p) = pq', where
  *     - keys(pq') = keys(pq)
  *     - priority(pq', y) = priority(pq, y) if y <> x
  *     - priority(pq', x) = p.
  *   Raises Empty if isEmpty pq.
  *   Raises BadKey if x is not a key in pq.
  *
  *   Note that updatePri can only be applied to priority queues whose
  *   keys belong to some equality type.
  *)
  val updatePri : ''a pri_queue * ''a * real -> ''a pri_queue

end
