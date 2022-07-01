let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edge_factor = try int_of_string Sys.argv.(2) with _ -> 16

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

let () =
  Random.self_init ();
  Printf.printf "Generating edge list...\n%!";
  let edges = Generate.go ~scale ~edge_factor in
  Printf.printf "Generated. Building sparse representation...\n%!";
  let t0 = Unix.gettimeofday () in
  let sparse = Kernel1Seq.kernel1 ~scale edges in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Done. Time: %f\n" (t1 -. t0);
  Printf.printf "Doing some checks...\n%!";
  let test = QCheck.Test.make ~count:1000 ~name:"destinations included"
    QCheck.(int_bound (twopow scale - 1))
    (fun vertex -> destinations_included ~edges ~sparse vertex)
  in
  exit @@ QCheck_runner.run_tests ~verbose:true [test]
