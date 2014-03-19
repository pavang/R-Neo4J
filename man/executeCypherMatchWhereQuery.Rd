\name{executeCypherMatchWhereQuery}
\alias{executeCypherMatchWhereQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execute Match Query with filtering
}
\description{
  Run cypher match query with a filter value using REST API call. Pass the node name to match with the property name      and value
}
\usage{
executeCypherMatchWhereQuery(db, nodeName, whereCond, whereVal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
      database connection
}
  \item{nodeName}{
      Type of the node
}
  \item{whereCond}{
      Name of the attribute on the node to filter
}
  \item{whereVal}{
      Value of the attribute on the node to filter
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
executeCypherMatchWhereQuery(db=db,nodeName="Person",whereCond="born",whereVal=1971)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
