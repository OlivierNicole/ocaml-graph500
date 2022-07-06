let validate = ref true
let num_domains = ref 1
let filename = ref ""

let speclist =
  [ ("-nocheck", Arg.Clear validate, "Run property-based checks");
    ("-ndomains", Arg.Set_int num_domains, "number of domains");
  ]

module T = Domainslib.Task

let sample_vertices n g =
  List.init n @@ fun _ -> SparseGraph.sample_vertex g

let sample_and_bfs ~pool sparse root =
  if !validate then Printf.printf "Performing BFS from vertex %d...\n%!" root;
  let parent_vec =
    T.run pool @@ fun () -> Kernel2Par.parent_vec ~pool root sparse in
  if !validate then begin
    Printf.printf "Checking BFS result...\n%!";
    parent_vec |> Array.iteri (fun vertex parent ->
      if parent = -1 then ()
      else if vertex = root then begin
        if parent <> root then begin
          Printf.eprintf "Parent of root %d should be itself, but is %d" root parent;
          exit 1
        end
      end else
        let children = SparseGraph.from parent sparse in
        if not @@ List.exists (fun (e,_) -> e = vertex) children then begin
              Printf.eprintf
                "parent_vec.(%d) = %d and yet children of %d are %s\n"
                vertex parent parent
                (QCheck.Print.(list (pair int float)) children);
              exit 1
        end
    );
    Printf.printf "Done. ";
    let size = ref 0 in
    Array.iter (fun v -> if v <> -1 then incr size) parent_vec;
    Printf.printf "Size of BFS tree: %d.\n" !size;
  end

let () =
  Random.self_init ();
  Arg.parse speclist
    (fun s -> filename := s)
    "kernel2Par [-nocheck] <edge list file>";
  if !filename = "" then begin
    Printf.eprintf "Must provide graph file argument.\n"; exit 1
  end;
  Printf.printf "Reading edge list from %s...\n%!" !filename;
  let t0 = Unix.gettimeofday () in
  let edges = Generate.from_file !filename in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Done. Time: %f s.\nBuilding sparse representation...\n%!" (t1 -. t0);
  let pool = T.setup_pool ~num_additional_domains:(!num_domains-1) () in
  let t0 = Unix.gettimeofday () in
  let sparse = T.run pool @@ fun () -> Kernel1Par.kernel1 ~pool edges in
  let t1 = Unix.gettimeofday () in
  T.teardown_pool pool;
  Printf.printf "Done. Time: %f s.\n" (t1 -. t0);
  Printf.printf "Sampling 64 vertices...\n%!";
  let roots = sample_vertices 64 sparse in
  let pool = T.setup_pool ~num_additional_domains:(!num_domains-1) () in
  let t0 = Unix.gettimeofday () in
  List.iter (sample_and_bfs ~pool sparse) roots;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Total BFS time: %f s.\n" (t1 -. t0);
  T.teardown_pool pool;
