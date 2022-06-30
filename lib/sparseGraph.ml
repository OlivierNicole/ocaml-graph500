open Types

module Int =
  struct
    include Int
    let hash i = i
  end

module Int_hashtbl = Hashtbl.Make(Int)

type t = (vertex * weight) list Int_hashtbl.t

let create = Int_hashtbl.create

let add_edge (s,e,w) g =
  match Int_hashtbl.find_opt g s with
  | None -> Int_hashtbl.add g s [(e,w)]
  | Some others -> Int_hashtbl.replace g s ((e,w) :: others)
