open Types

type t

(** Create a sparse graph representation able to contain vertices with labels
    in [0, 2^scale-1]. Note that this data structure becomes {i incorrect} if
      vertices with greater labels are used! *)
val create : scale:int -> t

val add_edge : edge -> t -> unit
val from : vertex -> t -> (vertex * weight) list
