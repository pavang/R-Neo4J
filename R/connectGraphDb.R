#' This function generates a plot depicting a graph, or a subgraph, from a Neo4j database.
#' @param subgraph Numeric vector. The indices of the nodes to include in the plot.
#' @param legend Boolean. Whether or not to produce a legend for the plot.
#' @param handle The handle on the Neo4j database as created by connectGraphDb().
#' @param ... Additional arguments to both 'gplot' and 'legend'. Defaults to TRUE.
#' @Details The most plot possible is one of a graph without any or with only a single label. In this case, the default colour is red. In case of multiple labels, plotNeo4j will print all nodes with different labels using different colours. The format of the nodes, as well as that of the edges might be changed by using '...' to control the underlying call to gplot.
#' @keywords graph
#' @export
#' @examples
#' \dontrun{
#' plotNeo4j(neo4j_handle)
#' }

plotNeo4j<-function(handle,subgraph=NULL,legend=TRUE,...){
	print_isolates<-ifelse(!is.null(subgraph),FALSE,TRUE)
	graph_to_plot<-buildAdjMatrix(handle,subgraph)
	gplot(graph_to_plot$adjMatrix,edge.col=8,vertex.col=graph_to_plot$labels,displayisolates=print_isolates)
	labels<-attr(graph_to_plot$labels,"levels")
	legend("bottomleft",title="Label",legend=labels,col=seq(1:length(labels)),pch=21,pt.bg=seq(1:length(labels)))
}
