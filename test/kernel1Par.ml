let run_prop_test = ref true
let scale = ref 12
let edge_factor = ref 16
let num_domains = ref 1

let speclist =
  [ ("-nocheck", Arg.Clear run_prop_test, "Run property-based checks");
    ("-scale", Arg.Set_int scale, "SCALE");
    ("-edgefactor", Arg.Set_int edge_factor, "edge factor");
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

let twopow x = 1 lsl x

module T = Domainslib.Task

let () =
  Random.self_init ();
  Arg.parse speclist
    (fun _ -> Printf.eprintf "Unsupported anonymous argument\n"; exit 1)
    "kernel1Seq [-scale <SCALE>] [-edgefactor <edge factor>] [-nocheck]";
  let scale = !scale and edge_factor = !edge_factor in
  Printf.printf "Generating edge list...\n%!";
  let edges = Generate.go ~scale ~edge_factor in
  Printf.printf "Generated. Building sparse representation...\n%!";
  ignore @@ read_line ();
  let pool = T.setup_pool ~num_additional_domains:(!num_domains-1) () in
  let t0 = Unix.gettimeofday () in
  let sparse = T.run pool @@ fun () -> Kernel1Par.kernel1 ~pool edges in
  let t1 = Unix.gettimeofday () in
  T.teardown_pool pool;
  Printf.printf "Done. Time: %f\n" (t1 -. t0);
  if !run_prop_test then begin
    Printf.printf "Doing some checks...\n%!";
    let test = QCheck.Test.make ~count:1000 ~name:"destinations included"
      QCheck.(int_bound (twopow scale - 1))
      (fun vertex -> destinations_included ~edges ~sparse vertex)
    in
    exit @@ QCheck_runner.run_tests ~verbose:true [test]
  end
