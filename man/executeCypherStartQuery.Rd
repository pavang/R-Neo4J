\name{executeCypherStartQuery}
\alias{executeCypherStartQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execute cypher query with a simple start
}
\description{
  Execute cypher query to a given nodex index and return the corresponding property
}
\usage{
executeCypherStartQuery(db, startNode, returnProp = "property")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
    Database connection details
}
  \item{startNode}{
    index of the node to be queried
}
  \item{returnProp}{
    return attribute
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns all the rows resulting from the match cypher query
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
\dontrun{
db<-connectGraphDb("http://finalprojtest.sb01.stations.graphenedb.com",24789,"finalproj_test","VD8XYRgY8SwO4RdCEKqc")
executeCypherStartQuery(db=db,startNode=15,returnProp="tagline")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
