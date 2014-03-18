\name{getNode}
\alias{getNode}
\title{This function fetches a node on the Neo4j graph DB specified by the handle and returns it as an R named list.}
\usage{
getNode(index, handle)
}
\arguments{
  \item{index}{The index of the node to be retrieved.}

  \item{handle}{The handle on the graph database, as
  created by connectGraphDb().}
}
\description{
This function fetches a node on the Neo4j graph DB
specified by the handle and returns it as an R named list.
}
\details{
The function execution will stop if the node corresponding
to the specified index does not exist in the database.
}
\examples{
\dontrun{
some_node<-getNode(100,neo4j_handle)
}
}
\keyword{graph}

