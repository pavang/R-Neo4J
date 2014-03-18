\name{buildAdjMatrix}
\alias{buildAdjMatrix}
\title{This function builds an Adjacency Matrix representaion out of a graph on Neo4j.}
\usage{
buildAdjMatrix(handle, subgraph = NULL)
}
\arguments{
  \item{subgraph}{Numeric vector. The indices of the nodes
  to include in the adjacency matrix.}

  \item{handle}{The handle on the Neo4j database as created
  by connectGraphDb().}
}
\description{
This function builds an Adjacency Matrix representaion out
of a graph on Neo4j.
}
\details{
This works as a helper function for plotNeo4j. The
resulting matrix actually contains not only the nodes on
subgraph, but also those that are linked to them.
}
\examples{
\dontrun{
## This will produce the adjacency matrix representation of the entire graph ##
adjMatrix<-buildAdjMatrix(neo4j_handle)
}
}
\keyword{graph}

