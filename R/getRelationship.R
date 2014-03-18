#' This function retrieves all relationships - coming or going from a node.
#' @param index Numeric.The index of the node whose relationships will be retrieved.
#' @param handle The handle on the Neo4j database as created by connectGraphDb().
#' @keywords graph
#' @export
#' @examples
#' \dontrun{
#' getRelationships(100,neo4j_handle)
#' }

getRelationships<-function(index,handle){
	relationships<-fromJSON(getURL(paste(handle$db$node,"/",index,"/relationships/all",sep=''),userpwd=graphdb$auth))
	kv<-lapply(relationships,f<-function(x){start=x$start
							end=x$end
							start<-unlist(strsplit(start,"/"))
							start<-start[length(start)]
							end<-unlist(strsplit(end,"/"))
							end<-end[length(end)]
							return(c(as.numeric(start),as.numeric(end)))
						}
		)
	node_relationships<-as.data.frame(kv)
	return(node_relationships)
}
