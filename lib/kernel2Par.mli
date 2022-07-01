open Types

module T = Domainslib.Task

val parent_vec : pool:T.pool -> vertex -> SparseGraph.t -> vertex array
