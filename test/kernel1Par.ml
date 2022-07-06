let run_prop_test = ref true
let num_domains = ref 1
let filename = ref ""

let speclist =
  [ ("-nocheck", Arg.Clear run_prop_test, "Run property-based checks");
    ("-ndomains", Arg.Set_int num_domains, "number of domains");
  ]

let destinations_included ~edges ~sparse start =
  let dests1 =
    Array.fold_left (fun acc (s,e,w) -> if s = start then (e,w)::acc else acc)
      [] edges
  in
  let dests2 = SparseGraph.from start sparse in
  dests1 |> List.for_all @@ fun (e,w) ->
      e = start (* Self-loops are removed by kernel 1 *)
      || List.mem (e,w) dests2

module T = Domainslib.Task

let () =
  Random.self_init ();
  Arg.parse speclist (fun s -> filename := s)
    "kernel1Par.exe [-nocheck] EDGE_LIST_FILE";
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
  Printf.printf "Done. Time: %f\n" (t1 -. t0);
  if !run_prop_test then begin
    Printf.printf "Doing some checks...\n%!";
    let pool = T.setup_pool ~num_additional_domains:(!num_domains-1) () in
    let max_vertex_label = T.run pool @@
      fun () -> Kernel1Par.max_vertex_label ~pool edges in
    T.teardown_pool pool;
    let test = QCheck.Test.make ~count:1000 ~name:"destinations included"
      QCheck.(int_bound (max_vertex_label + 1))
      (fun vertex -> destinations_included ~edges ~sparse vertex)
    in
    exit @@ QCheck_runner.run_tests ~verbose:true [test]
  end
