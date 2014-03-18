\name{graphTraverse}
\alias{graphTraverse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Graph Traverse function
}
\description{
    Simple routine to traverse through graph
}
\usage{
graphTraverse(db, startNode = 0, returnType = "path", order = "breadth_first", uniqueness = "none", language = "builtin", retPropName = "property", retPropVal = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
    database connection
}
  \item{startNode}{
    index of the node from where the traversal needs to begin
}
  \item{returnType}{
    see Neo4J documentation for all the possible values
}
  \item{order}{
    see Neo4J documentation for all the possible values
}
  \item{uniqueness}{
    see Neo4J documentation for all the possible values
}
  \item{language}{
    see Neo4J documentation for all the possible values
}
  \item{retPropName}{
    return property name
}
  \item{retPropVal}{
    return property values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Returns the traversed graph information
}
\references{
    http://docs.neo4j.org/chunked/stable/rest-api.html
}
\author{
    Lucas, Kranthi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
db<-connectGraphDb("http://finalprojtest.sb01.stations.graphenedb.com",24789,"finalproj_test","VD8XYRgY8SwO4RdCEKqc")
traverse<-graphTraverse(db,startNode=52,retPropName="name")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
