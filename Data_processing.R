library(tidyverse)
library(rgeos)
library(rworldmap)
        
water_nexus <- read_csv('WN2_v11.csv', locale = readr::locale(encoding = "latin1"))
water_nexus <- water_nexus[1:(which(is.na(water_nexus$`N°`))[1]-1),]

region <- read_csv('Region_names.csv')
b <- region$Regions
water_nexus <- water_nexus %>% select(-b) # remove regions 

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


# Response column 
water_nexus$Response[water_nexus$Response == "Other (Please specify)"] <- "Other"
water_nexus$Response[is.na(water_nexus$Response)] <- "Other"

# order column name

x <- water_nexus[, first_country:last_country, drop = F] 
x <- x[, order(names(x))]

water_nexus[, first_country:last_country] <- NULL
water_nexus <- bind_cols(water_nexus,x)

first_country <- which(colnames(water_nexus) == 'Afghanistan')
last_country <- which(colnames(water_nexus) == 'Zimbabwe')

# Making new lat and long columns

x_lat <- water_nexus[, first_country:last_country, drop = F]
colnames(x_lat) <- paste("lat",colnames(water_nexus[,first_country:last_country]), sep = "_")
x_long <- water_nexus[, first_country:last_country, drop = F]
colnames(x_long) <- paste("long",colnames(water_nexus[,first_country:last_country]), sep = "_")

for(i in 1:ncol(x)){
    for(j in 1:nrow(x)){
        if(!is.na(x[j,i])){
            x[j,i] <- lat_long$Country[i]
            x_lat[j,i] <- lat_long$x[i]
            x_long[j,i] <- lat_long$y[i]
        }
    }
}


water_nexus[, first_country:last_country] <- NULL
water_nexus <- bind_cols(water_nexus, x, x_lat, x_long)

# add https in the link of the website

for (i in 1:nrow(water_nexus)){
    if (!is.na(water_nexus$`Open-Ended Response`[i]) & str_detect(water_nexus$`Open-Ended Response`[i], "www") & !str_detect(water_nexus$`Open-Ended Response`[i], "http")){
        water_nexus$`Open-Ended Response`[i] <- str_c("https://", water_nexus$`Open-Ended Response`[i], sep = "")
    }
}

water_nexus <- water_nexus[,-1]

write.csv(water_nexus, "WN2_v12.csv", row.names = FALSE)
