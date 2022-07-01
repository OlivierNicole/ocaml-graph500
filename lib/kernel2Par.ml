module T = Domainslib.Task

type mark = Unmarked | Marked

let rec bfs ~pool g ~parent_vec ~marks root =
  if Atomic.compare_and_set marks.(root) Unmarked Marked then begin
    let destinations = SparseGraph.from root g in
    let promises = destinations |> List.map (fun (dst,_) ->
      parent_vec.(dst) <- root;
      T.async pool @@ fun () -> bfs ~pool g ~parent_vec ~marks dst
    )
    in
    List.iter (T.await pool) promises
  end
  (* If vertex has already been marked, do nothing *)

let parent_vec ~pool root g =
  let n = SparseGraph.max_vertex_label g + 1 in
  let parent_vec = Array.make n (-1) in
  parent_vec.(root) <- root;
  (* Mapping from vertex to marks. A vertex is marked if it has been or is
     being searched. *)
  let marks = Array.make n (Atomic.make Unmarked) in
  bfs ~pool g ~parent_vec ~marks root;
  parent_vec
