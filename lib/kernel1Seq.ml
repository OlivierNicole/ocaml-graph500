(* Kernel 1 is a basic construction of an adjacency HashMap for undirected
   graphs which corresponds to a sparse graph implementation.
   INPUTS : an array of edges as (start vertex, end vertex, weight) tuples. *)

open Types

(* Ensure that for every edge (start, end), start > end, swapping start and
   end if necessary. Remove self-loops. Also returns the maximum edge label. *)
let normalize : edge array -> edge array * vertex = fun edges ->
  let edges, max_label = Array.fold_left
      (fun (edges, max_label) (s,e,w) ->
        if s > e then (s,e,w) :: edges, max s max_label
        else if s = e then edges, max s max_label
        else (e,s,w) :: edges, max e max_label
      )
      ([],0)
      edges
  in
  Array.of_list (List.rev edges), max_label

let build_sparse ~scale ar =
  let g = SparseGraph.create ~scale in
  ar |> Array.iter (fun (s,e,w) ->
      SparseGraph.add_edge (s,e,w) g;
      SparseGraph.add_edge (e,s,w) g);
  g

let kernel1 ~scale edges =
  let ar, _ = normalize edges in
  build_sparse ~scale ar
