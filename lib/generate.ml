(*Kronecker is using the following algorithm :
 Function Kronecker generator(scale, edgefactor) :
 	N = 2^scale
 	M = edgefactor * N (No of edges)
 	[A,B,C] = [0.57, 0.19, 0.19]
 	ijw = {	{1,1,1,1,1,...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			}
 	ab = A + B;
  	c_norm = C/(1 - (A + B));
  	a_norm = A/(A + B);
        for ib = 1:SCALE,
          %% Compare with probabilities and set bits of indices.
          ii_bit = rand (1, M) > ab;
          jj_bit = rand (1, M) > ( c_norm * ii_bit + a_norm * not (ii_bit) );
          ijw(1:2,:) = ijw(1:2,:) + 2^(ib-1) * [ii_bit; jj_bit];
        end
  	ijw(3,:) = unifrnd(0, 1, 1, M);//produce values from 0 to 1 for 1*M array.

  	p = randperm (N);
  	ijw(1:2,:) = p(ijw(1:2,:));
  	p = randperm (M);
  	ijw = ijw(:, p);
  	ijw(1:2,:) = ijw(1:2,:) - 1;
	Here, the labels are from 0 to N-1.
*)

let gen_ii_bit ~length ~ab =
  Array.init length @@ fun _ -> Random.float 1. > ab

let gen_jj_bit ~ii_bit ~length ~a_norm ~c_norm =
  Array.init length @@ fun i ->
    Random.float 1. > (if ii_bit.(i) then c_norm else a_norm)

open Types

(* [twopow x] is 2 to the power [x]. *)
let twopow x = 1 lsl x

let compute_ijw m ab a_norm c_norm scale
    : vertex array * vertex array * float array =
  let fst_row = Array.make m 1 in
  let snd_row = Array.make m 1 in
  for ib = 1 to scale do
    let ii_bit = gen_ii_bit ~length:m ~ab in
    let jj_bit = gen_jj_bit ~ii_bit ~length:m ~a_norm ~c_norm in
    for i = 0 to m - 1 do
      if ii_bit.(i) then fst_row.(i) <- fst_row.(i) + twopow (ib-1);
      if jj_bit.(i) then snd_row.(i) <- snd_row.(i) + twopow (ib-1);
    done;
  done;
  fst_row, snd_row, Array.init m (fun _ -> Random.float 1.)

let permute : 'a array -> 'a array = fun ar ->
  let with_random_int = Array.map (fun x -> Random.bits (), x) ar in
  Array.sort (fun (a,_) (b,_) -> compare a b) with_random_int;
  Array.map (fun (_,x) -> x) with_random_int

let compute_number scale edge_factor =
  let n = int_of_float (2. ** float_of_int scale) in
  let m = edge_factor * n in
  (n, m)

(* Permutation function in [0,n]. *)
let permutation (n : int) : int -> int =
  let with_random_int = Array.init n @@ fun i -> Random.bits (), i in
  Array.sort (fun (a,_) (b,_) -> compare a b) with_random_int;
  fun i -> snd with_random_int.(i)

(* Generates a matrix with 3 rows and M columns. Each column represents an edge
   as the vector (start vertex, end vertex, weight). *)
let kronecker scale edge_factor : (vertex * vertex * weight) array =
  let n, m = compute_number scale edge_factor in
  let a, b, c = (0.57, 0.19, 0.19) in
  let ab = a +. b in
  let c_norm = c /. (1. -. (a +. b)) in
  let a_norm = a /. (a +. b) in
  let fst_row, snd_row, weights = compute_ijw m ab a_norm c_norm scale in
  (* Switch from edge labels starting at 1 to edge labels starting at 0 *)
  let fst_row = Array.map pred fst_row in
  let snd_row = Array.map pred snd_row in
  (* Permute start and end vertices  *)
  let p = permutation n in
  let fst_row = Array.map p fst_row in
  let snd_row = Array.map p snd_row in
  let edges = Array.init m @@ fun i -> fst_row.(i), snd_row.(i), weights.(i) in
  (* Permute edges *)
  permute edges

let go ~scale ~edge_factor =
  kronecker scale edge_factor

let to_file ~filename (edges : edge array) =
  let out = open_out filename in
  Marshal.to_channel out edges [];
  close_out out

let from_file filename =
  let in_ = open_in filename in
  let res : edge array = Marshal.from_channel in_ in
  close_in in_;
  res

let generate_to_file ~scale ~edge_factor ~filename =
  go ~scale ~edge_factor |> to_file ~filename
