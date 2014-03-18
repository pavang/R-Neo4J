\name{createRelationship}
\alias{createRelationship}
\title{This function creates a (directed) relationship between to nodes on the Neo4j graph DB specified by the handle.}
\usage{
createRelationship(start, end, type, properties = NULL, handle)
}
\arguments{
  \item{start}{Numeric.The node at the bottom of the
  relationship edge.}

  \item{end}{Numeric.The node at the tip of the edge.}

  \item{type}{A character string. The type of the
  relationship between the two nodes.}

  \item{properties}{A named list.The properties of the
  relationship.}

  \item{handle}{The handle on the Neo4j database as created
  by connectGraphDb().}
}
\description{
This function creates a (directed) relationship between to
nodes on the Neo4j graph DB specified by the handle.
}
\details{
This function does not (yet) support creation of undirected
relationships. That said, the parameter "type" is Neo4j
lingo and should be understood as the relationship's label
and NOT whether it is directed or undirected.
}
\examples{
\dontrun{
## This creates a directed edge from node 1 to node 2 ##
createRelationship(1,2,type="likes",properties=list(to_do="Play with",where="At the park"),handle=neo4j_handle)

##By creating another relationship, only inverting start and end, one gets an undirected edge ##
createRelationship(2,1,type="likes",properties=list(to_do="Play with",where="At the park"),handle=neo4j_handle)
}
}
\keyword{graph}

