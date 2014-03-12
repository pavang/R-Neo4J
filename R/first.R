library("RCurl")
library("RJSONIO")

graphDB <- setRefClass("graphDB",
                       fields=list(uri="character",creds="character",graphRoot="list"),
                       methods=list(
                          getGraphRoot=function(){
                            graphRoot
                         }))

connectGraph <- function(uri,userName,password) {
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  creds<-paste(userName,password,sep=":")
  curlPerform(url=uri,.opts=list(userpwd=creds, httpheader=headers),writefunction = h$update)
  root<-fromJSON(h$value())
  graphDB$new(uri=uri,creds=creds,graphRoot=root)
}

graphTraverse <- function(db,startNode=0,returnType="path",order="breadth_first",uniqueness="none",language="builtin",retPropName="property",retPropVal="all") {
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  returnVal<-list(language=language,retPropName=retPropVal)
  names(returnVal)[2]<-eval(retPropName)
  param<-list(order=order,uniqueness=uniqueness,return_filter=returnVal)
  uriAppend<-paste("node",startNode,"traverse",returnType,sep="/")
  uri<-paste(db$uri,uriAppend,sep="")
  curlPerform(url=uri,.opts=list(userpwd=db$creds,postfields=toJSON(param), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

executeCypher <- function(db,startNode,returnProp="property"){
  h <- basicTextGatherer()
  headers <- list('Accept' = 'application/json;charset=UTF-8', 'Content-Type' = 'application/json')
  executeQuery<-list(query=paste("start x = node(",startNode,") return x.",eval(returnProp),sep=""),params={})
  print(toJSON(executeQuery))
  curlPerform(url=db$graphRoot$cypher,.opts=list(userpwd=db$creds,postfields=toJSON(executeQuery), httpheader=headers),writefunction = h$update)
  fromJSON(h$value())
}

db<-connectGraph("http://test.sb01.stations.graphenedb.com:24789/db/data/","Test","WZKpp22qJnYd9LGBFNPN")
traverse<-graphTraverse(db,startNode=52,retPropName="name")
executeCypher(db=db,startNode=15,returnProp="tagline")


