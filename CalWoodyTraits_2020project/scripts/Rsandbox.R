## R sandbox

## Merging species lists
x <- data.frame(species=c('a','b','c'),traitx=c(1,2,3),traity=c(10,12,14))
y <- data.frame(species=c('b','c','d'),traitx=c(2,3,4),traitz=c(100,101,102))
x
y
xy <- merge(x,y,by = 'species',all = T)
xy

## checking names against a master list
master <- data.frame(species=c('a','b','c','d','e'))
plist <- data.frame(species=c('b','a','f'))

(bad.name.check <- match(plist$species,master$species))
plist$species[is.na(bad.name.check)]

(!plist$species %in% master$species)
plist$species[!plist$species %in% master$species]
