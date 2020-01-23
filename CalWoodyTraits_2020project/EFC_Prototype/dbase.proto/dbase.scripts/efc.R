# This function loads background tables and the list of
# files that are in the 'sources.to.enter' directory
# It checks that all sources to be loaded are listed in
# the sources table
load.tables = function(table.dir) {

	#load traits table
	traits = read.delim(paste(table.dir,'traits.txt',sep='/'))

	#load discrete states table
	dstates = read.delim(paste(table.dir,'dstates.txt',sep='/'))

	#load sources table
	sources = read.delim(paste(table.dir,'sources.txt',sep='/'),header=T,nrows=4)
	#str(source.list)
	sources$files = paste(sources$name,'txt',sep='.')

	#load list of data source tables and check if any are missing from sources table
	files.in.dir = dir('sources.to.enter')
	file2list = match(files.in.dir,sources$files)

	f.miss = which(is.na(file2list))
	if (length(f.miss)>0) print(paste('following files are missing 	from sources table:',dir('sources.to.enter')[f.miss]))
	
	return(list(traits,dstates,sources,files.in.dir))
}

# This function is called by build.data to check the data
# in each source file (indat). 
# check.tjm: flag to request checking tjm names
# taxa.check, trait.check, units.check, dstates.check are
# cumulative variables of cases that violate various
# checks, to report after build.data is complete
check.indat = function(f,source.name,tjm1,traits,sources,files.in.dir,
			check.tjm,indat,taxa.check,trait.check,
			units.check,dstates.check) {
		# if check.tjm = TRUE: check for taxa not in TJM1
		if (check.tjm) {
			tx2tjm1 = match(indat$taxon,tjm1$taxon)
			tx.miss = which(is.na(tx2tjm1))
			if (length(tx.miss)>0) {
				taxa.check = rbind(taxa.check,rbind(
					c(as.character(source.name),
						'taxa not matching TJM1 deleted'),
					cbind(indat$id[tx.miss],
					as.character(indat$taxon[tx.miss]))))
				#indat = indat[-tx.miss,]
			} else {
				tx.miss = NULL
				taxa.check = rbind(taxa.check,
					cbind(as.character(source.name),					'all taxa match TJM1'))
			}
			
			# check which taxa are in tjm1 but are not minimum
			# rank taxa; report but do not delete
			not.mrt = which(tjm1$mrt[tx2tjm1]==0)
			if (length(not.mrt)>0) {
				taxa.check = rbind(taxa.check,rbind(
					c(as.character(source.name),
						'warning: taxa not TJM1 mrt'),
					cbind(indat$id[not.mrt],
					as.character(indat$taxon[not.mrt]))))			} else {
				taxa.check = rbind(taxa.check,
					cbind(as.character(source.name),
					'all taxa are mrt'))					}
		} else {
			tx.miss = NULL
		}

		#check for records whose traits are not in traits table
		tr2trts = match(indat$trait,traits$trait)

		tr.miss = which(is.na(tr2trts))
		if (length(tr.miss)>0) {
			trait.check = rbind(trait.check,rbind(
				c(as.character(source.name),
					'traits not matching trait list deleted'),
				cbind(indat$id[tr.miss],
				as.character(indat$trait[tr.miss]))))
			#indat = indat[-tr.miss,]
		} else {
			tr.miss = NULL
			trait.check = rbind(trait.check,
				cbind(as.character(source.name),
				'all traits match'))
		}

		#check for records whose units don't match traits table
		units.mismatch = which(as.character(
			traits$units[tr2trts])!=as.character(indat$units))
		if (length(units.mismatch)>0) {
			units.check = rbind(units.check,rbind(
				c(as.character(source.name),
					'units not matching dstate list deleted'),
				cbind(indat$id[units.mismatch],
				as.character(indat$units[units.mismatch]))))
			#indat = indat[-tr.miss,]
		} else {
			units.mismatch = NULL
			units.check = rbind(units.check,
				cbind(as.character(source.name),
				'all units match'))
		}
	
		# check nominal traits to be sure all states match
		# dstates table
		nom.traits = which(traits$type[tr2trts]=='nominal')
	 	trait_states = paste(indat$trait[nom.traits],
	 		indat$dstate[nom.traits])
	 	
		dstates.mismatch = which(
			(trait_states%in%paste
			(dstates$trait,dstates$dstate))==FALSE)
			
		if (length(dstates.mismatch)>0) {
			trst.mismatch = nom.traits[dstates.mismatch]
			dstates.check = rbind(dstates.check,rbind(
				c(as.character(source.name),
					'dstates not matching trait list deleted'),
				cbind(indat$id[trst.mismatch],
				as.character(trait_states[dstates.mismatch]))))
			#indat = indat[-tr.miss,]
		} else {
			trst.mismatch = NULL
			dstates.check = rbind(dstates.check,
				cbind(as.character(source.name),
				'all dstates match'))
		}
		
		# xx.check variables are for reporting
		# xx.miss and xx.mismatch are lists of records to
		# be deleted
		return(list(taxa.check,trait.check,
			units.check,dstates.check,
			tr.miss,tx.miss,units.mismatch,trst.mismatch))
}


# build.data reads in data files from 'sources.to.enter', 
# calls 'check.indat', removes records that don't check
# and builds results into sdata data.frame
# newbuild: flag whether to build data file from scratch (TRUE)
# 	or whether to replace records of previously added sources
#	and add new records from new sources

# ADD features: check for missing columns of list needed for sdata
# and add NA by default
build.data = function(tjm1,traits,sources,files.in.dir,newbuild,check.tjm,sdata=NULL) {
	nfiles = length(files.in.dir)
	file2list = match(files.in.dir,sources$files)

	# if newbuild = FALSE then check if current sources are
	# already in data.txt, and remove matching records
	if (!newbuild) {
		replace.lines = which(sdata$source%in%sources$name[file2list])
		if (length(replace.lines)>0) {
			sdata = sdata[-replace.lines,]
			if (nrow(sdata)>0) {
				for (c in ncol(sdata)) {
					if (class(sdata[,c])=='factor') 
						sdata[,c] = as.character(sdata[,c])
					}
				} else newbuild = TRUE 
		}
	}
	
	taxa.check = NULL
	trait.check = NULL
	units.check = NULL
	dstates.check = NULL
	
	# process source files
	for (f in 1:nfiles) {
		source.file = files.in.dir[f]
		source.name = sources$name[file2list[f]]
		print(source.file)
		indat = read.delim(paste
			('~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/sources.to.enter',source.file,sep='/'))
		
		check.results = check.indat(f,
			source.name,tjm1,traits,sources,
			files.in.dir,check.tjm,indat,
			taxa.check,trait.check,units.check,dstates.check)
		taxa.check = check.results[[1]]
		trait.check = check.results[[2]]
		units.check = check.results[[3]]
		dstates.check = check.results[[4]]		
		tr.miss = check.results[[5]]
		tx.miss = check.results[[6]]
		units.mismatch = check.results[[7]]
		trst.mismatch = check.results[[8]]
		
		#remove all records with name, trait or units problems
		indat.orig = indat
		remove = unique(c(tr.miss,tx.miss,
			units.mismatch,trst.mismatch))
		if (!is.null(remove)) indat = indat[-remove,]
		
		#reset factor levels after removing records
		for (i in 1:ncol(indat)) 
			if (class(indat[,i])=='factor') 
					indat[,i] = as.factor(as.character(indat[,i]))
		
		#create newdata for firest source, or append new source
		source.nam.rep = rep(source.name,nrow(indat))

		#create NA variables for missing columns
		if (!'dstate'%in%names(indat)) indat$dstate=NA
		if (!'units'%in%names(indat)) indat$units=NA
		if (!'country'%in%names(indat)) indat$country =NA
		if (!'state'%in%names(indat)) indat$state =NA
		if (!'county'%in%names(indat)) indat$county =NA
		if (!'place.name'%in%names(indat)) indat$place.name=NA
		if (!'lat'%in%names(indat)) indat$lat =NA
		if (!'long'%in%names(indat)) indat$long =NA
		if (!'latlong.conf'%in%names(indat)) 
			indat$latlong.conf =NA
		if (!'collection'%in%names(indat)) indat$collection =NA
		if (!'accession'%in%names(indat)) indat$accession =NA
		
		tmp.data = data.frame(source.name,indat$id,indat$taxon,
			indat$trait,indat$dstate,indat$value,
			indat$units,indat$country,indat$state,
			indat$county,indat$place.name,
			indat$lat,indat$long,
			indat$collection,indat$accession)

		if (f==1) newdata=tmp.data else 
			newdata = rbind(newdata,tmp.data)
		
	} #end f loop

	# rename columns
	names(newdata) = c('source','source.id',
		'taxon','trait','dstate','value','units','country',
		'state','county','place','lat','long',
		'collection','accession')

	# create unique id field and means.id field (to match 
	# data records to means.txt records
	newdata$id = rep(NA,nrow(newdata))
	newdata$means.id = rep(NA,nrow(newdata))
	nc = ncol(newdata)
	newdata = newdata[,c((nc-1),nc,1:(nc-2))]

	# replace data with enwdata, or append 
	if (newbuild) sdata = newdata else sdata=rbind(sdata,newdata)
	sdata$id = 1:nrow(sdata)
	sdata$means.id = rep(NA,nrow(sdata))
	return(list(taxa.check,trait.check,units.check,		dstates.check,sdata,indat.orig))
	}

# check sdata for potential duplicates records, based on
# identical values for records with same
# taxon/trait/dstate (if present)
check.duplicates = function(sdata) {
	pot.dups = NULL
	tt=paste(as.character(sdata$taxon),
		as.character(sdata$trait),
		as.character(sdata$dstate))
	ttu = unique(tt)
	for (i in 1:length(ttu)) {
		t = ttu[i]
		sdtt = subset(sdata[which(tt==t),])
		sdtt = sdtt[order(sdtt$value),]
		if (nrow(sdtt)>1) {
			#print(t)
			same.value=which(diff(sdtt$value)==0)
			same.value=unique(c(same.value,same.value+1))
			#print(same.value)
			if (length(same.value)>1) pot.dups=rbind(pot.dups,				sdtt[same.value,3:9])
			}
		}
	return(pot.dups)
	}

# build.means calculates means and ordered dstate values
# from sdata file
build.means = function(tjm1,traits,sdata) {
	trait.list = unique(sdata$trait)
	ntraits = length(trait.list)
	
	for (t in 1:ntraits) {
		#print(t)
		tr2trt = match(trait.list[t],traits$trait)
		dt = subset(sdata,(sdata$trait==trait.list[t])&!is.na(sdata$value))
		if (traits$type[tr2trt]=='continuous') {
			tm = tapply(dt$value,dt$taxon,mean,na.rm=TRUE)
			tm = tm[which(!is.na(tm))]
			tm = signif(tm,digits=3)
			ts = rep(NA,length(tm)) #placeholder for dstate
			tN = tapply(dt$value,dt$taxon,length)
			tN = tN[which(!is.na(tN))]
		} else {
			# summary stats for multiple observations of nominal states needed here
			tN = tapply(dt$dstate,dt$taxon,length)
			tN = tN[which(!is.na(tN))]	
			ts = rep(NA,length(tN))
			tm = rep(NA,length(tN)) #placeholder for value
			st.mn = aggregate(dt$value,
				list(dt$dstate,dt$taxon),mean,na.rm=TRUE)
			for (tx in 1:length(tN)) {
				st.mnx = subset(st.mn,(st.mn[,2]==names(tN)[tx])					&(st.mn$x!=0))
				ord.st = st.mnx[,1][order(st.mnx$x,
					decreasing=TRUE)]
				for (s in 1:length(ord.st)) {
					if (s==1) ts[tx]=paste(ord.st[1]) else
						ts[tx] = paste(ts[tx],ord.st[s],sep='/')
					}
			}
		}
		tname = rep(trait.list[t],length(tN))
		taxon = row.names(tN)
		units = rep(traits$units[match(trait.list[t],
			traits$trait)],length(tN))
		tmean = data.frame(taxon,tname,ts,array(tm),units,array(tN))
		names(tmean) = c('taxon','trait','dstate','value','units','N')
		if (t>1) {
			means = rbind(means,tmean)
			} else means = tmean
	}
	t2m = match(means$taxon,tjm1$taxon)
	means$family = tjm1$family[t2m]
	means$genus = tjm1$genus[t2m]
	means$id = 1:nrow(means)
	means = means[,c(9,7,8,1:6)]
	
	# now do match of taxon&trait in means to data to fill in means.id in data table
	means2data = match(paste(sdata$taxon,sdata$trait),
		paste(means$taxon,means$trait))
	sdata$means.id = means$id[means2data]
	
	return(list(sdata,means))
	}
	
write.efc.tables = function(sources,files.in.dir,sdata,means,commit) {
	if (commit) efc.dir = '~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/efc.tables' else efc.dir = '~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/efc.tables.tmp'
	if (!file.exists(efc.dir)) dir.create(efc.dir)
	datafile = paste(efc.dir,'data.txt',sep='/')
	meansfile = paste(efc.dir,'means.txt',sep='/')
	write.table(sdata,file=datafile,
		sep='\t',
		quote=FALSE,
		col.names=TRUE,
		row.names=FALSE)
	write.table(means,file=meansfile,
		sep='\t',
		quote=FALSE,
		col.names=TRUE,
		row.names=FALSE)

	#set date/time of last time sources were processed
	# Something's not working - dates are coming in as large integers
	if (FALSE) {
	sources$last.processed = as.character(sources$last.processed)
	sources$last.processed[match(files.in.dir,sources$files)]=Sys.Date()
	
	#last col of sources is $files, which is created when it's read in
	#to avoid redundant storage with $name, so it's not written back
	#out here.
	write.table(sources[,-ncol(sources)],file='../info.tables/sources.txt',
		sep='\t',
		quote=FALSE,
		col.names=TRUE,
		row.names=FALSE)	
	}
}
	
	
latlong.convert = function(deg,min,sec) {
	ll = deg + min/60 + sec/3600
	return(ll)
}
