\name{plotNeo4j}
\alias{plotNeo4j}
\title{This function generates a plot depicting a graph, or a subgraph, from a Neo4j database.}
\usage{
plotNeo4j(handle, subgraph = NULL, legend = TRUE, ...)
}
\arguments{
  \item{subgraph}{Numeric vector. The indices of the nodes
  to include in the plot.}

  \item{legend}{Boolean. Whether or not to produce a legend
  for the plot.}

  \item{handle}{The handle on the Neo4j database as created
  by connectGraphDb().}

  \item{...}{Additional arguments to both 'gplot' and
  'legend'. Defaults to TRUE.}
}
\description{
This function generates a plot depicting a graph, or a
subgraph, from a Neo4j database.
}
\examples{
\dontrun{
plotNeo4j(neo4j_handle)
}
}
\keyword{graph}

