Parallel implementation of the [Graph500
benchmark](https://graph500.org/?page_id=12) in OCaml.

## Usage

This is the result of a two day's hacking so it's a bit messy.

`bin/gen.exe` generates a list of graph edges given a “scale” and an edge factor
(see Graph500 reference) and dumps it to a file. It's not very efficient and
single-core. To give an idea, a scale of 21 and an edge factor of 16 yields a
~700 MB file.

As of now, only the kernels no. 1 and 2 are implemented. They are in the form of
libraries in `lib/`, for ease of use in OCaml code.

**Note:** I realized that kernel 2 (`lib/kernel2Par.ml`) does not respect the
specifications of the Graph500 benchmark, as the spanning tree that it
outputs has no guarantee to be a BFS tree.

There is a number of test executables:
- `test/kernel1Seq.exe` generates a list of edges and runs kernel 1 on it,
  using only one OCaml domain (outdated).
- `test/kernel1Par.exe` generates a list of edges and runs kernel 1 on it. It
  takes a number of parameters, including the number of OCaml domains to use.
- `test/kernel2Par.exe` reads a list of edges from a file, then runs kernels 1
  and 2 on it in sequence, with some printing and validation of the results. It
  takes a number of parameters, including the number of domains.
