(* Kernel 1 is a basic construction of an adjacency HashMap for undirected
   graphs which corresponds to a sparse graph implementation.
   INPUTS : an array of edges as (start vertex, end vertex, weight) tuples. *)

open Types

let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edge_factor = try int_of_string Sys.argv.(2) with _ -> 16

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

let build_sparse ~max_edge_label ar =
  let g = SparseGraph.create (max_edge_label + 1) in
  ar |> Array.iter (fun (s,e,w) ->
      SparseGraph.add_edge (s,e,w) g;
      SparseGraph.add_edge (e,s,w) g);
  g

let kernel1 edges =
  let ar, max_edge_label = normalize edges in
  build_sparse ~max_edge_label ar

let () =
  Printf.printf "Generating edge list...\n%!";
  let edges = Generate.go ~scale ~edge_factor in
  Printf.printf "Generated. Building sparse representation...\n%!";
  let t0 = Unix.gettimeofday () in
  ignore @@ Sys.opaque_identity @@ kernel1 edges;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Done. Time: %f\n" (t1 -. t0)

