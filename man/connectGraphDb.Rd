\name{connectGraphDb}
\alias{connectGraphDb}
\title{This function creates a database handle that can be passed as an
argument to several other functions in this package.
Output is a named list with the Neo4j base URL relative to the specidied server.}
\usage{
connectGraphDb(dburl, port, usr, pwd)
}
\arguments{
  \item{dburl}{The (http) URL of the host to which you wish
  to connect.}

  \item{port}{The port on the host where a Neo4j instance
  is running.}

  \item{usr}{A character string with a valid user name for
  the specified Neo4j database.}

  \item{pwd}{The database's REST password.}
}
\description{
This function creates a database handle that can be passed
as an argument to several other functions in this package.
Output is a named list with the Neo4j base URL relative to
the specidied server.
}
\details{
The function execution will stop if authentication on the
server fails. However, it will not specify the reason,
since Neo4j server does not return this information.
}
\examples{
\dontrun{
username<-"finalproj_test"
pwd<-"VD8XYRgY8SwO4RdCEKqckwkw"
dburl<-"http://finalprojtest.sb01.stations.graphenedb.com"
port<-24789
neo4j_handle <- connectGraphDb(dburl,port,username,pwd)
getNode(100,neo4j_handle)
}
}
\keyword{graph}

