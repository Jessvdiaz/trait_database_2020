## R sandbox
rm(list=ls()) # erase everything in current environment
getwd() # get working directory

splist_dir <- "CalWoodyTraits_2020project/data/species_lists/"
CF <- read.csv(paste(splist_dir,"chilefornia_species.csv",sep=""),header=T,as.is=T)
PWD <- read.csv("CalWoodyTraits_2020project/data/species_lists/PWD_woody_veg.csv",header=T,as.is=T,na.strings = "NA")

# remove NA species - subset worked!!
dim(PWD)
PWD <- subset(PWD,!is.na(PWD$CF_species_name))
dim(PWD)

# remove non-California species from CF and create CA
dim(CF)
CA <- subset(CF,!is.na(CF$chilefornia_california))
dim(CA)
rm('CF')

# filter data
head(CA)
head(PWD)

names(CA)
names(PWD)

## Merging data frames example #####
# x <- data.frame(species=c('a','b','c'),traitx=c(1,2,3),traity=c(10,12,14))
# y <- data.frame(species=c('b','c','d'),traitx=c(2,3,4),traitz=c(100,101,102))
# x
# y
 
###############################
#### Merging species List #####
 ##############################

#Are all PWD species in CF list?
PWD_in_CA <- PWD$CF_species_name %in% CA$species

# print species that are present
PWD$CF_species_name[PWD_in_CA]

# print species that are absent
PWD$CF_species_name[!PWD_in_CA]

sp_list <- merge(CA,PWD,by.x="species", by.y="CF_species_name",all=T)

names(sp_list)

# look at a record that came in from PWD for a species that was not in the CA tree list - see NAs for the columns from the original CA file
sp_list[sp_list$species=='Ceanothus cuneatus',]



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
