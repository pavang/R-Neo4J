#' This function creates a (directed) relationship between to nodes on the Neo4j graph DB specified by the handle.
#' @param start Numeric.The node at the bottom of the relationship edge.
#' @param end Numeric.The node at the tip of the edge. 
#' @param type A character string. The type of the relationship between the two nodes.
#' @param properties A named list.The properties of the relationship.
#' @param handle The handle on the Neo4j database as created by connectGraphDb().
#' @details This function does not (yet) support creation of undirected relationships. That said, the parameter "type" is Neo4j lingo and should be understood as the relationship's label and NOT whether it is directed or undirected.
#' @keywords graph
#' @export
#' @examples
#' \dontrun{
#' ## This creates a directed edge from node 1 to node 2 ##
#' createRelationship(1,2,type="likes",properties=list(to_do="Play with",where="At the park"),handle=neo4j_handle) 
#' 
#' ##By creating another relationship, only inverting start and end, one gets an undirected edge ##
#' createRelationship(2,1,type="likes",properties=list(to_do="Play with",where="At the park"),handle=neo4j_handle)
#'}

createRelationship<-function(start,end,type,properties=NULL,handle){
	to<-paste(handle$db$node,"/",end,sep='')
	type<-type
	if(!is.null(properties)){
		data<-properties
		relationship<-list(to=to,type=type,data=data)
	}
	else{
		relationship<-list(to=to,type=type)
	}
	relationship<-toJSON(relationship)
	serverReturns<-getURL(paste(handle$db$node,"/",start,"/relationships",sep=''),postfields=relationship,httpheader="Content-type:application/json",customrequest="POST",userpwd=handle$auth)
	cat(paste("Server Retuns:",serverReturns))
}
