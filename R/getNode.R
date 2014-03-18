#' This function fetches a node on the Neo4j graph DB specified by the handle and returns it as an R named list.
#' @param index The index of the node to be retrieved. 
#' @param handle  The handle on the graph database, as created by connectGraphDb().
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value A named list containing the properties of the specified node. 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' some_node<-getNode(100,neo4j_handle)
#' }

getNode<-function(index,handle){
	node<-getURL(paste(handle$db$node,"/",index,sep=''),userpwd=handle$auth)
	contents<-fromJSON(node)
	if("stacktrace"%in%names(contents))stop(paste(contents$message))
	return(contents)
}
