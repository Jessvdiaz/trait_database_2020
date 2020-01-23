#Preprocessing functions for new data matrices
taxa.check = function(d,tjm1) {
	tx2tjm1 = match(d$taxon,tjm1$taxon)
	tx.miss = which(is.na(tx2tjm1))
	if (length(tx.miss)>0) {
		tx.miss.names = cbind(d$id[tx.miss],
		as.character(d$taxon[tx.miss]))
	} else tx.miss.names=NULL
	return(tx.miss.names)
}


matrix2records = function(d,copy.cols,trait.cols,		trait.names=NA,trait.units) {
	
	records=NULL

	for (i in 1:length(trait.cols)) {
		t = trait.cols[i]
		value = d[,t]
		have.data = which(!is.na(value))
		rec = cbind(d[have.data,copy.cols],
			as.character(trait.names[i]),
			value[have.data],
			as.character(trait.units[i]))
		records=rbind(records,rec)
	}
	records=data.frame(records)
	return(records)
}