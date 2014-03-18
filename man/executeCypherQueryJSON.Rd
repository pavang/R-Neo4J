\name{executeCypherQueryJSON}
\alias{executeCypherQueryJSON}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execute Cypher query via pre-built JSON
}
\description{
  Pass pre-built JSON as a param for the Cypher call using REST API
}
\usage{
executeCypherQueryJSON(db, json = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
    database connection
}
  \item{json}{
    cypher query pre-built
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
db<-connectGraphDb("http://finalprojtest.sb01.stations.graphenedb.com",24789,"finalproj_test","VD8XYRgY8SwO4RdCEKqc")
str<-"{\n \"query\": \"start x = node(15) return x.tagline\",\n\"params\": null \n}"
executeCypherQueryJSON(db,str)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
