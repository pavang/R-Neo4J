library("RCurl")
library("RJSONIO")

#' Simple routine to traverse through graph
#' @param db The index of the node to be retrieved. 
#' @param startNode  The handle on the graph database, as created by connectGraphDb().
#' @param returnType  see Neo4J documentation for all the possible values
#' @param order  see Neo4J documentation for all the possible values
#' @param uniqueness  see Neo4J documentation for all the possible values
#' @param language  see Neo4J documentation for all the possible values
#' @param retPropName  Return property name of the node
#' @param retPropVal  Return property value of the node
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value Returns the traversed graph information 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' graphTraverse(db,startNode=52,retPropName="name")
#' }

graphTraverse <- function(db,startNode=0,returnType="path",order="breadth_first",uniqueness="none",language="builtin",retPropName="property",retPropVal="all") {
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  returnVal<-list(language=language,retPropName=retPropVal)
  names(returnVal)[2]<-eval(retPropName)
  param<-list(order=order,uniqueness=uniqueness,return_filter=returnVal)
  uriAppend<-paste("node",startNode,"traverse",returnType,sep="/")
  uri<-paste(db$baseurl,uriAppend,sep="")
  curlPerform(url=uri,.opts=list(userpwd=db$auth,postfields=toJSON(param), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#' Execute cypher query to a given nodex index and return the corresponding property
#' @param db Database connection details 
#' @param startNode  The handle on the graph database, as created by connectGraphDb().
#' @param returnProp  Return property name of the node
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value Returns all the rows resulting from the match cypher query 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' executeCypherStartQuery(db=db,startNode=15,returnProp="tagline")
#' }
executeCypherStartQuery <- function(db,startNode,returnProp="property"){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("start x = node(",startNode,") return x.",eval(returnProp),sep=""),params={})
  print(toJSON(executeQuery))
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#' Run cypher match query using REST API call. Pass the node name
#' @param db Database connection details 
#' @param nodeName  The name of the node
#' @param returnProp  Return property name of the node
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value Returns all the rows resulting from the match cypher query 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' executeCypherMatchQuery(db=db,nodeName="Person")
#' }
executeCypherMatchQuery <- function(db,nodeName){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("match (x:`",nodeName,"`) return x",sep=""),params={})
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}  

#' Run cypher match query using REST API call. Pass the node name
#' @param db Database connection details 
#' @param nodeName  The name of the node
#' @param returnProp  Return property name of the node
#' @param whereCond  where condition
#' @param whereVal  Return property name of the node
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value Returns all the rows resulting from the match cypher query 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' executeCypherMatchQuery(db=db,nodeName="Person")
#' }
executeCypherMatchWhereQuery <- function(db,nodeName,whereCond,whereVal){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("match (x:`",nodeName,"`) where x.",whereCond,"=", whereVal,"return x",sep=""),params={})
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#' Run cypher match query using REST API call. Pass the node name
#' @param db Database connection details 
#' @param json  Cypher query in JSON format
#' @details The function execution will stop if the node corresponding to the specified index does not exist in the database.
#' @value Returns all the rows resulting from the match cypher query 
#' @keywords graph
#' @depends RCurl RJSONIO
#' @export
#' @examples
#' \dontrun{
#' str<-"{\n \"query\": \"start x = node(15) return x.tagline\",\n\"params\": null \n}"
#' executeCypherQueryJSON(db,str)
#' }
executeCypherQueryJSON <- function(db,json=""){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=json, httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}
