\name{getRelationships}
\alias{getRelationships}
\title{This function retrieves all relationships - coming or going from a node.}
\usage{
getRelationships(index, handle)
}
\arguments{
  \item{index}{Numeric.The index of the node whose
  relationships will be retrieved.}

  \item{handle}{The handle on the Neo4j database as created
  by connectGraphDb().}
}
\description{
This function retrieves all relationships - coming or going
from a node.
}
\examples{
\dontrun{
getRelationships(100,neo4j_handle)
}
}
\keyword{graph}

