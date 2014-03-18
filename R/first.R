library("RCurl")
library("RJSONIO")

#Utility to connect to the graph DB
connectGraphDb<-function(dburl,port,usr,pwd){
  auth<-paste(usr,":",pwd,sep='')
  baseurl<-paste(dburl,":",port,"/db/data/",sep="")
  db<-getURL(paste(baseurl),userpwd=paste(auth))
  return(list(db=fromJSON(db),auth=auth,baseurl=baseurl))
}

#Function to perform graph traversal given the start node
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

#Function to do simple query given the node
executeCypherStartQuery <- function(db,startNode,returnProp="property"){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("start x = node(",startNode,") return x.",eval(returnProp),sep=""),params={})
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#Function to retrieve all the nodes matching a name
executeCypherMatchQuery <- function(db,nodeName){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("match (x:`",nodeName,"`) return x",sep=""),params={})
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#Function to retrieve all the nodes matching a name and with the provided value
executeCypherMatchWhereQuery <- function(db,nodeName,whereCond,whereVal){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("match (x:`",nodeName,"`) where x.",whereCond,"=", whereVal,"return x",sep=""),params={})
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

#Function to execute CypherQuery given the JSON input
executeCypherQueryJSON <- function(db,json=""){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  curlPerform(url=db$db$cypher,.opts=list(userpwd=db$auth,postfields=json, httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

db<-connectGraphDb("http://finalprojtest.sb01.stations.graphenedb.com",24789,"finalproj_test","VD8XYRgY8SwO4RdCEKqc")
graphTraverse(db,startNode=52,retPropName="name")
executeCypherStartQuery(db=db,startNode=15,returnProp="tagline")
executeCypherMatchQuery(db=db,nodeName="Person")
executeCypherMatchWhereQuery(db=db,nodeName="Person",whereCond="born",whereVal=1971)

createNode(list(name="John",lastname="Doe"),label="Person",handle=db)
getNode(300)
createRelationship(1,330,"test",handle=db)
getRelationships(330,handle=db)
plotNeo4j(handle=db) ##This may take a few minutes for the movie graph example

