open Types

type t = { scale : int; slots : (vertex * weight) list Atomic.t array }

let twopow x = 1 lsl x

let create ~scale =
  assert (scale <= 60); (* More than 2^60 vertices seems a bit much. *)
  { scale; slots = Array.init (twopow scale) (fun _ -> Atomic.make []) }

let rec add_edge (s,e,w) g =
  let old = Atomic.get g.slots.(s) in
  let new_ = (e,w) :: old in
  if Atomic.compare_and_set g.slots.(s) old new_ then ()
  else add_edge (s,e,w) g

let from s g =
  Atomic.get g.slots.(s)
