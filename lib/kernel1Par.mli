open Types

module T = Domainslib.Task

val kernel1 : pool:Domainslib.Task.pool -> edge array -> SparseGraph.t
