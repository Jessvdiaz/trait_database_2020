
## R sandbox


NB<- read.csv("north_bay_species_list.csv",header=T)
PWD <- read.csv("PWD_woody_veg.csv",header=T)

# filter data
head(NB)
head(PWD)

 names(NB)
 names(PWD)

## Merging data frames example #####
# x <- data.frame(species=c('a','b','c'),traitx=c(1,2,3),traity=c(10,12,14))
# y <- data.frame(species=c('b','c','d'),traitx=c(2,3,4),traitz=c(100,101,102))
# x
# y
 
###############################
#### Merging species List #####
 ##############################
 
(both<- merge(NB,PWD,by.x="species", by.y="SPECIES",all=T))



# ## checking names against a master list###
#  # another way to merge data and check names##
#  
# master <- data.frame(species=c('a','b','c','d','e'))
# plist <- data.frame(species=c('b','a','f'))
# 
# (bad.name.check <- match(plist$species,master$species))
# plist$species[is.na(bad.name.check)]
# 
# (!plist$species %in% master$species)
# plist$species[!plist$species %in% master$species]
