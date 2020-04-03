all_sp <-read.csv("sp_all.csv",header = T)
trydb <- read.csv("TryAccSpecies.csv",header=T)

#head(trydb)

# head(all_sp)




require(dplyr)
require(stringr)
full_sp <- left_join(all_sp, trydb, by=c("species"="AccSpeciesName"))
  # or could use merge(
missing.spp <- all_sp$species[which(is.na(all_sp$Species.ID.with.commas))]
missing.spp 

trydb[grep(pattern = "Pseudotsuga", x=as.character(trydb$AccSpeciesName)),]



# misspelled menziesii
all_sp <- all_sp[which(all_sp$species!="Pseudotsuga menzesii"),]  # to exclude a row
#all_sp$species[which(all_sp$species=="Pseudotsuga menzesii")] <- "Pseudotsuga menziesii"


trydb[grep(pattern = "Arbutus", x=as.character(trydb$AccSpeciesName)),]
all_sp[grep(pattern = "Arbutus", x=as.character(all_sp$species)),]

all_sp <- all_sp[which(all_sp$species!="Arbutus menzesii"),]  # to exclude a row
#all_sp$species[which(all_sp$species=="Arbutus menzesii")] <- "Arbutus menziesii"
 



trydb[grep(pattern = "Artemesia", x=as.character(trydb$AccSpeciesName)),]

all_sp[grep(pattern = "Artemesia", x=as.character(all_sp$species)),]


# misspelled Artemesia that should be Artemisia (but Artemisia isn't in dataset, so need to edit existing entry
#all_sp <- all_sp[which(all_sp$species!="Artemesia californica"),]  # to exclude a row
all_sp$species <- as.character(all_sp$species)
all_sp$species[which(all_sp$species=="Artemesia californica")] <- "Artemisia californica"



### Change genus Hesperocyparis to Cupressus
# using "str_replace" from the {stringr}
all_sp$species <- str_replace(all_sp$species, "Hesperocyparis", "Cupressus")



### write.csv(all_sp,file="all_sp.csv")

#### write.csv(missing.spp,"missing.spp.csv")




#### Merge cleaned all_sp database
all_sp$Species.ID.with.commas <- trydb$species.ID.with.commas[match(all_sp$species, trydb$AccSpeciesName)]
