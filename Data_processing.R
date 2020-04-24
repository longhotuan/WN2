library(tidyverse)
library(rgeos)
library(rworldmap)
library(feather)

water_nexus <- read_csv('data_24_04_20.csv', locale = readr::locale(encoding = "latin1"))
for (i in 1:nrow(water_nexus)){ 
    if(sum(is.na(water_nexus[i,])) == ncol(water_nexus)){
        water_nexus <- water_nexus[-i,]
    }
}
# water_nexus <- water_nexus[1:(which(is.na(water_nexus$`N°`))[1]-1),]

water_nexus <- water_nexus[,-1]
water_nexus <- water_nexus[c(2:5,1,6:ncol(water_nexus))]

region <- read_csv('Region_names.csv')
b <- region$Regions
water_nexus <- water_nexus %>% dplyr::select(-intersect(colnames(water_nexus), b)) # remove regions 

lat_long <- read_csv('lat_long.csv', locale = readr::locale(encoding = "latin1"))

# Remove NA columns
water_nexus <- water_nexus[ , colSums(is.na(water_nexus)) < nrow(water_nexus)]

#  connect with lat_long df
first_country <- which(colnames(water_nexus) == 'Benin')
last_country <- which(colnames(water_nexus) == 'Switzerland')
setdiff(colnames(water_nexus[,first_country:last_country]),lat_long$Country)
colnames(water_nexus)[ which(colnames(water_nexus) == 'Syrian Arab Republic')] <- "Syria"
colnames(water_nexus)[ which(colnames(water_nexus) == 'Palestinian territories')] <- "Palestine"
# colnames(water_nexus)[ which(colnames(water_nexus) == 'Afghanistan_1')] <- "Afghanistan"
# colnames(water_nexus)[ which(colnames(water_nexus) == 'Iran (Islamic Republic of)')] <- "Iran"

c <- setdiff(lat_long$Country,colnames(water_nexus[, first_country:last_country]))
lat_long <- lat_long %>% filter(!(Country %in% c)) # remove the country is not 
lat_long <- lat_long[order(lat_long$Country),]


# Sector column 
water_nexus$Sector[water_nexus$Sector == "Other (Please specify)"] <- "Other"
water_nexus$Sector[is.na(water_nexus$Sector)] <- "Other"


# order column name

x <- water_nexus[, first_country:last_country, drop = F] 
x <- x[, order(names(x))]

water_nexus[, first_country:last_country] <- NULL
water_nexus <- bind_cols(water_nexus,x)

first_country <- which(colnames(water_nexus) == 'Afghanistan')
last_country <- which(colnames(water_nexus) == 'Zimbabwe')


# change columns of x to character

x <- rbind_list(lapply(x, as.character))

# Making new lat and long columns

x_lat <- water_nexus[, first_country:last_country, drop = F]
colnames(x_lat) <- paste("lat",colnames(water_nexus[,first_country:last_country]), sep = "_")
x_long <- water_nexus[, first_country:last_country, drop = F]
colnames(x_long) <- paste("long",colnames(water_nexus[,first_country:last_country]), sep = "_")

for(i in 1:ncol(x)){
    for(j in 1:nrow(x)){
        if(!is.na(x[j,i]) & i != 125){ # some thing wrong with Turkmenistan --> super weird!!!!!
            x[j,i] <- lat_long$Country[i]
            x_lat[j,i] <- lat_long$x[i]
            x_long[j,i] <- lat_long$y[i]
        }
    }
}

x_lat$lat_Turkmenistan[which(!is.na(x_lat$lat_Turkmenistan))] <- lat_long$x[which(lat_long$Country == "Turkmenistan")]
x_long$long_Turkmenistan[which(!is.na(x_long$long_Turkmenistan))] <- lat_long$x[which(lat_long$Country == "Turkmenistan")]
    
    
    
water_nexus[, first_country:last_country] <- NULL
water_nexus <- bind_cols(water_nexus, x, x_lat, x_long)

# add https in the link of the website

for (i in 1:nrow(water_nexus)){
    if (!is.na(water_nexus$`Open-Ended Response`[i]) & str_detect(water_nexus$`Open-Ended Response`[i], "www") & !str_detect(water_nexus$`Open-Ended Response`[i], "http")){
        water_nexus$`Open-Ended Response`[i] <- str_c("https://", water_nexus$`Open-Ended Response`[i], sep = "")
    }
}


# the sectors 

first_sector <- which(colnames(water_nexus) == 'Water and agriculture')
last_sector <- which(colnames(water_nexus) == 'Finance')

water_nexus[, first_sector:last_sector] <- rbind_list(lapply(water_nexus[, first_sector:last_sector], as.character))

for (i in seq(first_sector, last_sector)){
    water_nexus[as.logical(!is.na(water_nexus[,i])),i] <- colnames(water_nexus[i])
}

write.csv(water_nexus, "WN2_v15.csv", row.names = FALSE)
write_feather(water_nexus, "WN2_v2_23_04_20.feather")
