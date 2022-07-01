let scale = ref 12
let edge_factor = ref 16
let num_domains = ref 1
let filename = ref ""

let speclist =
  [ ("-scale", Arg.Set_int scale, "SCALE");
    ("-edgefactor", Arg.Set_int edge_factor, "edge factor");
    ("-ndomains", Arg.Set_int num_domains, "number of domains");
  ]

module T = Domainslib.Task

let () =
  Random.self_init ();
  Arg.parse speclist
    (fun s -> filename := s)
    "kernel1Seq [-scale <SCALE>] [-edgefactor <edge factor>] [-nocheck]";
  if !filename = "" then begin
    Printf.eprintf "Must provide graph file argument.\n"; exit 1
  end;
  let scale = !scale and edge_factor = !edge_factor in
  Printf.printf "Generating edge list...\n%!";
  let t0 = Unix.gettimeofday () in
  let edges = Generate.go ~scale ~edge_factor in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Generated. Time: %f s.\n" (t1 -. t0);
  Generate.to_file ~filename:!filename edges;
