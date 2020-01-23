#Sequence of commands to build efc database
rm(list=ls())
source('~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/dbase.scripts/efc.R')
setwd('/Users/david/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto')
#check wd has 'info.tables', 'sources.to.enter' directories
print(dir())
table.dir = '~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/info.tables'

#load jepson93 taxonomy
tjm1=read.delim(paste(table.dir,'tjm1.txt',sep='/'))

# get list of source files in 'sources.to.enter'
tables = load.tables(table.dir)
traits = tables[[1]]
dstates = tables[[2]]
sources = tables[[3]]
files.in.dir = tables[[4]]

#call new.build.data
# newbuild = TRUE to build new data.txt from sources.to.enter
# newbuild = FALSE to add or replace source to existing data.txt
# check.tjm = TRUE to build data.txt matching TJM1
# check.tjm =FALSE to build data.txt from all records
newbuild = TRUE
check.tjm = TRUE

if (newbuild) {
	data.build = build.data(tjm1,traits,sources,
		files.in.dir,newbuild,check.tjm)
	} else {
	datafile = '~/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto/efc.tables/data.txt'
	sdata = read.delim(datafile)
	print(dim(sdata))
	data.build = build.data(tjm1,traits,sources,
		files.in.dir,newbuild,check.tjm,sdata)
	}
taxa.check = data.build[[1]]
trait.check = data.build[[2]]
units.check = data.build[[3]]
dstates.check = data.build[[4]]
new.sdata = data.build[[5]]
indat.orig = data.build[[6]]
print(dim(new.sdata))

print(unique(taxa.check[,2]))
print(unique(trait.check[,2]))
print(unique(units.check[,2]))
print(unique(dstates.check[,2]))

pot.dups = check.duplicates(sdata)

#build means table from data table
mtables = build.means(tjm1,traits,new.sdata)
new.sdata = mtables[[1]]
new.means = mtables[[2]]

#write data and means tables to efc.tables dir
#setting to true will overwrite previous data and means tables!
commit=TRUE 
write.efc.tables(sources,files.in.dir,new.sdata,new.means,commit)

