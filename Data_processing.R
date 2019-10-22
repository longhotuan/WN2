library(tidyverse)

WN <- read_csv("WN2_v5.csv", locale = readr::locale(encoding = "latin1"))

WN<-WN[,-1]
Water_Nexus <- read_csv('WN2_v4.csv', locale = readr::locale(encoding = "latin1"))
region <- read_csv('Region_names.csv')
b <- region$Regions

WN <- WN %>% select(-b,b)

library(rgeos)
library(rworldmap)
lat_long <- read_csv('lat_long.csv', locale = readr::locale(encoding = "latin1"))

first_country <- which(colnames(Water_Nexus) == 'Benin')
last_country <- which(colnames(Water_Nexus) == 'Switzerland')
setdiff(colnames(WN[,first_country:last_country]),lat_long$Country)
# WN$Reponse <- str_replace_all(WN$Response, string = "Other (Please specify)", pattern = "Other")
WN$Response[WN$Response == "Other (Please specify)"] <- "Other"
WN$Response[is.na(WN$Response)] <- "Other"
levels(as.factor(WN$Response))

write.csv(WN, 'WN2_v6.csv', row.names = FALSE)

Water_Nexus <- read_csv('WN2_v6.csv', locale = readr::locale(encoding = "latin1"))
first_country <- which(colnames(Water_Nexus) == 'Benin')
last_country <- which(colnames(Water_Nexus) == 'Switzerland')
lat_long <- read_csv("lat_long.csv", locale = readr::locale(encoding = "latin1"))



setdiff(colnames(Water_Nexus[,30:226]),lat_long$Country)

colnames(Water_Nexus)[ which(colnames(Water_Nexus) == 'Afghanistan_1')] <- "Afghanistan"
colnames(Water_Nexus)[ which(colnames(Water_Nexus) == 'Iran (Islamic Republic of)')] <- "Iran"
colnames(Water_Nexus)[ which(colnames(Water_Nexus) == 'Syrian Arab Republic')] <- "Syria"
colnames(Water_Nexus)[ which(colnames(Water_Nexus) == 'Palestinian territories')] <- "Palestine"
setdiff(colnames(Water_Nexus[,30:226]),lat_long$Country)

b <- as.data.frame(matrix(data = NA, nrow = nrow(Water_Nexus), ncol = 227-30))
colnames(b) <- paste("lat",colnames(Water_Nexus[,30:226]), sep = "_")
WN2 <- bind_cols(Water_Nexus, b)
colnames(b) <- paste("long",colnames(Water_Nexus[,30:226]), sep = "_")
WN2 <- bind_cols(WN2, b)

for(i in 1:nrow(WN2)){
    for(j in 30:226){
        for(k in 1:nrow(lat_long)){
            if(!is.na(WN2[i,j]) & WN2[i,j] == lat_long$Country[k]){
                WN2[i,j+248] <- lat_long$x[k]
                WN2[i, j+248+197] <- lat_long$y[k]
            }
        }
    }
}


write.csv(WN2, "WN2_v7.csv", row.names = FALSE)

Water_Nexus$lat_Palestine[which(!is.na(Water_Nexus$Palestine))] <- lat_long$x[which(lat_long$Country == "Palestine")]
Water_Nexus$long_Palestine[which(!is.na(Water_Nexus$Palestine))] <- lat_long$y[which(lat_long$Country == "Palestine")]

Water_Nexus$`lat_Sao Tome & Principe`[which(!is.na(Water_Nexus$`Sao Tome & Principe`))] <- lat_long$x[which(lat_long$Country == "Sao Tome & Principe")]
Water_Nexus$`long_Sao Tome & Principe`[which(!is.na(Water_Nexus$`Sao Tome & Principe`))] <- lat_long$y[which(lat_long$Country == "Sao Tome & Principe")]

Water_Nexus$lat_Syria[which(!is.na(Water_Nexus$Syria))] <- lat_long$x[which(lat_long$Country == "Syria")]
Water_Nexus$long_Syria[which(!is.na(Water_Nexus$Syria))] <- lat_long$y[which(lat_long$Country == "Syria")]
Water_Nexus[63,53+445] <- Water_Nexus[72,53+445]
Water_Nexus[63,53+248] <- Water_Nexus[72,53+248]

# test
a <- vector(mode = "character", 0)
for (i in 30:226){
    if(sum(which(is.na(Water_Nexus[,i])) != which(is.na(Water_Nexus[,i + 248]))) >0){
        a <- c(a, i)
    }
}
b <- vector(mode = "character", 0)
for (i in 30:226){
    if(sum(which(is.na(Water_Nexus[,i])) != which(is.na(Water_Nexus[,i + 445]))) >0){
        b <- c(b, i)
    }
}

write.csv(Water_Nexus, "WN2_v8.csv", row.names = FALSE)
