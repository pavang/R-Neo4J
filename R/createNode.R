#' This function creates a node on the Neo4j graph DB specified by the handle.
#' @param contents A named list containing the properties of the node to be created.
#' @param label Defaults to NULL. The label of the node in the graph. 
#' @param handle  The handle on the graph database, as created by connectGraphDb().
#' @details The function execution will stop if the named list provided as input does not translate to proper JSON. This function will also output a string containing the response from the server. If that response is a string containing a JSON object with the properties specified on the input of the function, the node was created successfully.
#' @keywords graph
#' @export
#' @examples
#' \dontrun{
#' ## This translates to proper JSON ##
#' some_node<-list(name="John",lastname="Doe",school="Stanford",degree="Statistics")
#' its_label<-"Person"
#' createNode(some_node,its_label,neo4j_handle)
#'
#' ## This doesn't ##
#' some_node<-list(name="John",lastname="Doe",school=list(name="Stanford",degree="Statistics"))
#' }

createNode<-function(contents,label=NULL,handle){
	jsonContents<-toJSON(contents)
	if(!isValidJSON(jsonContents))stop("Invalid node contents")
	jsonLabel<-toJSON(label)	
	serverReturns<-getURL(paste(handle$db$node),postfields=jsonContents,httpheader="Content-type:application/json",customrequest="POST",userpwd=handle$auth)
	if(!is.null(label)){
		nodeIndex<-unlist(strsplit(fromJSON(serverReturns)$self,"/db/data/node/"))[2]
		labelCreated<-getURL(paste(handle$db$node,"/",nodeIndex,"/labels",sep=''),postfields=jsonLabel,httpheader="Content-type:application/json",customrequest="POST",userpwd=handle$auth)
		cat(paste("Node Index:",nodeIndex))
	}
	cat(paste("Server Returned:",serverReturns))	
}
