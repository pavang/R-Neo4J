\name{executeCypherMatchQuery}
\alias{executeCypherMatchQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execute Match Query
}
\description{
  Run cypher match query using REST API call. Pass the node name
}
\usage{
executeCypherMatchQuery(db, nodeName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
     database connection
}
  \item{nodeName}{
   Type of the node
}
}
\details{
   None
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
db<-connectGraphDb("http://finalprojtest.sb01.stations.graphenedb.com",24789,"finalproj_test","VD8XYRgY8SwO4RdCEKqc")
out<-executeCypherMatchQuery(db=db,nodeName="Person")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
