#' This function builds an Adjacency Matrix representaion out of a graph on Neo4j.
#' @param subgraph Numeric vector. The indices of the nodes to include in the adjacency matrix.
#' @param handle The handle on the Neo4j database as created by connectGraphDb().
#' @value A Matrix. More precisely, the adjacency matrix representation of the graph or subgraph specified in the inputs.
#' @details This works as a helper function for plotNeo4j. The resulting matrix actually contains not only the nodes on subgraph, but also those that are linked to them.
#' @keywords graph
#' @examples
#' \dontrun{
#' ## This will produce the adjacency matrix representation of the entire graph ##
#' adjMatrix<-buildAdjMatrix(neo4j_handle)
#' }

buildAdjMatrix<-function(handle,subgraph=NULL){
	i<-1
	allnodes<-vector("list")
	alllabels<-vector("list")
	node<-getNode((i-1),handle)
	while(!("stacktrace"%in%names(node))){
		if((i-1) %in% subgraph || is.null(subgraph)){
			alllabels[[i]]<-fromJSON(getURL(paste(node$labels),userpwd=handle$auth))
			allnodes[[i]]<-getRelationships((i-1),handle)
		}
		i<-i+1
		node<-getNode((i-1),handle)
	}
	i<-i-1
	adjMatrix<-matrix(rep(0,(i^2)),i,i)
	lapply(allnodes,f<-function(x)if(sum(dim(x))>0){lapply(x,function(x){print(x);adjMatrix[(x[1]+1),(x[2]+1)]<<-1})})
	return(list(adjMatrix=adjMatrix,labels=factor(unlist(alllabels))))							
}
