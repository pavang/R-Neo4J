connectGraphDb<-function(dburl,port,usr,pwd){
	auth<-paste(usr,":",pwd,sep='')
	baseurl<-paste(dburl,":",port,"/db/data/",sep="")
	db<-getURL(paste(baseurl),userpwd=paste(auth))
	return(list(db=fromJSON(db),auth=auth))
}

createNode<-function(contents,handle){
	jsonContents<-toJSON(contents)	
	getURL(paste(handle$db$node),postfields=jsonContents,httpheader="Content-type:application/json",
	customrequest="POST",userpwd=handle$auth)
}

getNode<-function(index,handle){
	node<-getURL(paste(handle$db$node,"/",index,sep=''),userpwd=handle$auth)
	contents<-fromJSON(node)
	return(contents)
}	

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

buildAdjMatrix<-function(graphdb){
	i<-1
	allnodes<-vector("list")
	node<-getNode((i-1),graphdb)
	while(!("stacktrace"%in%names(node))){
		relationships<-fromJSON(getURL(paste(node$all_relationships),userpwd=graphdb$auth))
		kv<-lapply(relationships,f<-function(x){start=x$start
								end=x$end
								start<-unlist(strsplit(start,"/"))
								start<-start[length(start)]
								end<-unlist(strsplit(end,"/"))
								end<-end[length(end)]
								return(c(as.numeric(start),as.numeric(end)))
								}
		)
		kvtable<-as.data.frame(kv)
		allnodes[[i]]<-kvtable
		i<-i+1
		node<-getNode((i-1),graphdb)
	}
	i<-i-1
	adjMatrix<-matrix(rep(0,(i^2)),i,i)

	lapply(allnodes,f<-function(x){lapply(x,function(x){adjMatrix[(x[1]+1),(x[2]+1)]<<-1})})

	return(adjMatrix)							
}
		
	
library("RCurl")
library("RJSONIO")
library("sna")
	
username<-"finalproj_test"
pwd<-"VD8XYRgY8SwO4RdCEKqc"
dburl<-"http://finalprojtest.sb01.stations.graphenedb.com"
port<-24789

handle<-connectGraphDb(dburl,port,username,pwd)

someNode<-list(name="john",last_name="doe",institution="Stanford",degree="Statistics")

createNode(someNode,handle)

someNodeFromDb<-getNode(349,handle)

createRelationship(349,351,type="Knows",handle=handle)

adjMatrix<-buildAdjMatrix(handle) ##This takes a minute or so to run with the movie DB

gplot(adjMatrix)
