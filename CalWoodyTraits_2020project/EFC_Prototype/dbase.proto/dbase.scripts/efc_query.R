## Query EFC database

rm(list=ls())
setwd('/Users/david/Documents/Projects/EcologicalFloraCal/Prototype/dbase.proto')
source('dbase.scripts/efc.R')

table.dir = 'info.tables'

#load jepson93 taxonomy
tjm1=read.delim(paste(table.dir,'tjm1.txt',sep='/'))

# get list of source files in 'sources.to.enter'
tables = load.tables(table.dir)
traits = tables[[1]]
dstates = tables[[2]]
sources = tables[[3]]
files.in.dir = tables[[4]]

dim(traits)
head(traits)
dim(dstates)
head(dstates)
dim(sources)
head(sources)

dall <- read.csv('efc.tables/cdata-all.csv')
dst <- read.csv('efc.tables/ctaxontrait.csv')

dim(dall)
head(dall)

dim(dst)
head(dst)

## Read in query list
qdir <- '/Users/david/Documents/Projects/Pepperwood/VegPlots_traits/'
qf <- dir(qdir)
qf
if ('species_list.csv' %in% qf) sq <- read.csv(paste(qdir,'species_list.csv',sep=''),as.is=T)
head(sq)

# query all trait means
cbind(sq$efc.taxon,sq$efc.taxon %in% dst$taxon)

qdst <- dst[dst$taxon %in% sq$efc.taxon,]
dim(qdst)
head(qdst)

qdall <- dall[dall$taxon %in% sq$efc.taxon,]
dim(qdall)
head(qdall)

write.csv(qdall,paste(qdir,'efc_trait_query.csv',sep=''),quote=F,row.names = F)
