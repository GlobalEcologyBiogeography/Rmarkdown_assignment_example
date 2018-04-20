# Libraries ---------------------------------------------------------------
library(tidyverse)
library(foreach)
library(dismo)
library(raster)

# Data --------------------------------------------------------------------
# 1. Life history traits for Amniote data
download.file("http://www.esapubs.org/archive/ecol/E096/269/Data_Files/Amniote_Database_Aug_2015.csv",
              "./data/base/Amniote_Database_Aug_2015.csv")

Amniotes<-read.csv("./data/base/Amniote_Database_Aug_2015.csv")

# 2. Climatic variables
bio_ly <- getData("worldclim",var="bio",res=10, path="./data/base/")

# Cleaning and subsetting ---------------------------------------------------
Amniotes[Amniotes==-999]<-NA

# Subsetting data
Hyracoidea<-
  Amniotes %>% 
  filter(class == "Mammalia" & order =="Hyracoidea")


# Download GBIF data ------------------------------------------------------
Hyracoidea$Binomial<-paste(Hyracoidea$genus,Hyracoidea$species)

Hyracoidea$genus<-as.character(Hyracoidea$genus)
Hyracoidea$species<-as.character(Hyracoidea$species)

Hyracoidea_records <-
  foreach(i=1:length(Hyracoidea$Binomial),.combine = rbind)%do%{
    
    print(paste("Downloading data for",Hyracoidea$genus[i],Hyracoidea$species[i]))
    gbif(Hyracoidea$genus[i],Hyracoidea$species[i])[c("species","lat","lon","fullCountry")]
    
}

# Cleaning georeferences --------------------------------------------
# Delete occurrence without geographic information
Hyracoidea_records<-
  Hyracoidea_records %>% 
  filter(!is.na(lon)&!is.na(lat))

## Get rid of duplicate occurrences
dups=duplicated(Hyracoidea_records[, c("lon", "lat")])
Hyracoidea_records <-Hyracoidea_records[!dups, ]

## Exclude records from Brasil and Peru
Hyracoidea_records<-
  Hyracoidea_records %>% 
  filter(lon>(-30))


# Extracting climatic information --------------------------------------------
tempRast <- bio_ly$bio1/10
precipRast <- bio_ly$bio12

##Convert Ocurrence data to spatial points 
Hyracoidea_records_geo<-Hyracoidea_records
coordinates(Hyracoidea_records_geo)<-~lon + lat

## Changing projection (tempRast and precipiRast have the same projection)
proj4string(Hyracoidea_records_geo)<-proj4string(tempRast)

Hyracoidea_records$temp<-raster::extract(tempRast,Hyracoidea_records_geo)
Hyracoidea_records$precip<-raster::extract(precipRast,Hyracoidea_records_geo)


# Merge and write data frames --------------------------------------------
# Merge dataframes
Hyracoidea_traits_geo<-merge(Hyracoidea,Hyracoidea_records,by.x="Binomial",by.y="species")

# write data frames
write.csv(Hyracoidea_traits_geo, "./data/processed/Hyracoidea_traits_geo.csv")

