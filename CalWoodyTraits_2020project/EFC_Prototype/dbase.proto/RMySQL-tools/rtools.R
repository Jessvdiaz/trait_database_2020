## Rtools to access efc dbase through mysql (localhost)

require(RMySQL)
con = dbConnect(MySQL(),user='david',password='dacksql')
#dbGetQuery(con, "show databases")
dbGetQuery(con, "use efc")
dbGetQuery(con, "show tables")
dbGetQuery(con, "describe means")

listTraits = function(table = 'means') {
	traits = dbGetQuery(con, paste('select trait from ',table,sep=''))
	return(as.array(unique(traits[,1])))
}

selectTrait = function(trait.name,table='means') {
	t = dbGetQuery(con, paste("select * from ",table,"  where trait = ",dQuote(trait.name),sep=""))
	return(t)
}

write.table(paste("select * from means where trait = ",trait.name,"",sep="'"),row.names=FALSE,col.names=FALSE,qmethod='d')