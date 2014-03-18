#' This function creates a database handle that can be passed as an
#' argument to several other functions in this package.
#' Output is a named list with the Neo4j base URL relative to the specidied server.
#' @param dburl The (http) URL of the host to which you wish to connect.
#' @param port The port on the host where a Neo4j instance is running. 
#' @param usr  A character string with a valid user name for the specified Neo4j database.
#' @param pwd The database's REST password.
#' @details The function execution will stop if authentication on the server fails. However, it will not specify the reason, since Neo4j server does not return this information.
#' @value a named list containing several URLs, relevant for REST manipulation of Neo4j objects, and an authentication string.
#' @keywords graph
#' @export
#' @examples
#' \dontrun{
#' username<-"finalproj_test"
#' pwd<-"VD8XYRgY8SwO4RdCEKqckwkw"
#' dburl<-"http://finalprojtest.sb01.stations.graphenedb.com"
#' port<-24789
#' neo4j_handle <- connectGraphDb(dburl,port,username,pwd)
#' getNode(100,neo4j_handle)
#' }

connectGraphDb<-function(dburl,port,usr,pwd){
	auth<-paste(usr,":",pwd,sep='')
	baseurl<-paste(dburl,":",port,"/db/data/",sep="")
	db<-getURL(paste(baseurl),userpwd=paste(auth))
	cat(paste("Server returned:",db))
	if(!isValidJSON(db,TRUE))stop("Can't authenticate on server. Please check if you used correct username, password and port")
	return(list(db=fromJSON(db),auth=auth))
}
