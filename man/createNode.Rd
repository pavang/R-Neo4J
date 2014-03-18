\name{createNode}
\alias{createNode}
\title{This function creates a node on the Neo4j graph DB specified by the handle.}
\usage{
createNode(contents, label = NULL, handle)
}
\arguments{
  \item{contents}{A named list containing the properties of
  the node to be created.}

  \item{label}{Defaults to NULL. The label of the node in
  the graph.}

  \item{handle}{The handle on the graph database, as
  created by connectGraphDb().}
}
\description{
This function creates a node on the Neo4j graph DB
specified by the handle.
}
\details{
The function execution will stop if the named list provided
as input does not translate to proper JSON. This function
will also output a string containing the response from the
server. If that response is a string containing a JSON
object with the properties specified on the input of the
function, the node was created successfully.
}
\examples{
\dontrun{
## This translates to proper JSON ##
some_node<-list(name="John",lastname="Doe",school="Stanford",degree="Statistics")
its_label<-"Person"
createNode(some_node,its_label,neo4j_handle)

## This doesn't ##
some_node<-list(name="John",lastname="Doe",school=list(name="Stanford",degree="Statistics"))
}
}
\keyword{graph}

