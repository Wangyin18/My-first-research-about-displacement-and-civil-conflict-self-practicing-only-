library (tidyr)
library(tidyverse)
library (dplyr)
setwd ("/Users/zhaowangyin/Desktop/Research_displaced_2/Original Data/Data_Extract_From_World_Development_Indicators-5")
setwd ("/Users/zhaowangyin/Desktop/Research_displaced_2/Original Data")

Displaced <- read.csv ("49d3ef67-2c4f-48c8-b07b-432efc6f4710_Data.csv")
Displaced <- Displaced [, -c(1,2,3)]
Displaced.longer <- pivot_longer(data = Displaced, cols = -Country.Code, names_to = "Year", values_to = "number")

# Displaced 
Displaced.longer$number[Displaced.longer$number == ".."] <- NA
as.data.frame(Displaced.longer)
class (Displaced.longer)
nrow (na.omit (Displaced.longer))
Displaced.longer$year1 <- substr(Displaced.longer$Year,2,5)
Displaced.longer$state_year <- paste (Displaced.longer$Country.Code, Displaced.longer$year1, sep = "+")
Database$number <- as.numeric(Database$number)

UCDP <- read.csv("ged201.csv")
UCDP_new <- dplyr::select (UCDP, relid, year, country, region, type_of_violence, best)

UCDP_new$relid <- substr(UCDP_new$relid,1,3)
UCDP_new$state_year <- paste (UCDP_new$relid, UCDP_new$year, sep = "+")

# number of all events in organized violence
library (dplyr)


UCDP_all_n <- summarize(group_by(UCDP_new, state_year), 
                                all.number = length (state_year))
Database <- merge(Displaced.longer, UCDP_all_n, by.x="state_year",by.y="state_year", all=TRUE)

Database$all.number[is.na(Database$all.number)] <- 0



#conflict > 25 (did not apply into analysis as an dependent variable)

UCDP_new$conflict_dummy_geo <- ifelse(UCDP_new$best >= 25, 1, 0)
UCDP_new_dummy <- summarize(group_by(UCDP_new, state_year), 
                            conflict.dummy_geo = mean(conflict_dummy_geo))
UCDP_new_dummy$conflict.dummy_geo <- ifelse(UCDP_new_dummy$conflict.dummy_geo > 0, 1, 0)
Database <- merge(Database, UCDP_new_dummy ,by.x="state_year",by.y="state_year", all=TRUE)

Database$conflict.dummy_geo[is.na(Database$conflict.dummy_geo)] <- 0



# intrastate

UCDP_intr <- read.csv("ucdp-prio-acd-201.csv")
UCDP_intr_select <- dplyr::select (UCDP_intr, conflict_id, location, side_a_id, incompatibility, year, type_of_conflict, cumulative_intensity, gwno_loc, region)
UCDP_intr_select <- UCDP_intr_select[UCDP_intr_select$year > 2009 & UCDP_intr_select$year < 2019 & UCDP_intr_select$type_of_conflict == 3,] 
iso3c <- read.csv ("gwstates.csv")
UCDP_intr_select <- merge (UCDP_intr_select, iso3c, by.x = "gwno_loc", by.y = "gwcode")
UCDP_intr_select$state_year <- paste (UCDP_intr_select$iso3c, UCDP_intr_select$year, sep = "+")

UCDP_intr_select_2 <- summarize(group_by(UCDP_intr_select, state_year), 
                                intrastate.dummy = length (state_year))
UCDP_intr_select_2$intrastate.dummy [UCDP_intr_select_2$intrastate.dummy > 0] <- 1 

Database <- merge(Database, UCDP_intr_select_2 ,by.x="state_year",by.y="state_year", all=TRUE)
Database$intrastate.dummy[is.na(Database$intrastate.dummy)] <- 0

# the number of intrastate 

UCDP_intr_select_3 <- summarize(group_by(UCDP_intr_select, state_year), 
                                intrastate.number = length (state_year))

Database <- merge(Database, UCDP_intr_select_3 ,by.x="state_year",by.y="state_year", all=TRUE)

Database$intrastate.number[is.na(Database$intrastate.number)] <- 0

# version 2 --- conflict (did not apply into analysis as dependent variable because of its disadvantages)
UCDP_intr <- read.csv("ucdp-prio-acd-201.csv")
UCDP_intr_select_4 <- dplyr::select (UCDP_intr, conflict_id, location, side_a_id, incompatibility, year, type_of_conflict, cumulative_intensity, gwno_loc, region)
UCDP_intr_select_4 <- UCDP_intr_select_4 [UCDP_intr_select_4$year > 2009 & UCDP_intr_select_4$year < 2019,]

iso3c <- read.csv ("gwstates.csv")
UCDP_intr_select_4 <- merge (UCDP_intr_select_4, iso3c, by.x = "gwno_loc", by.y = "gwcode")
UCDP_intr_select_4$state_year <- paste (UCDP_intr_select_4$iso3c, UCDP_intr_select_4$year, sep = "+")

UCDP_intr_select_5 <- summarize(group_by(UCDP_intr_select_4, state_year), 
                                conflict.dummy = length (state_year))
UCDP_intr_select_5$conflict.dummy [UCDP_intr_select_5$conflict.dummy > 0] <- 1 

Database <- merge(Database, UCDP_intr_select_5 ,by.x="state_year",by.y="state_year", all=TRUE)
Database$conflict.dummy[is.na(Database$conflict.dummy)] <- 0


## control variables
QOD <- read.csv ("time.series.csv")
state.code <- read.csv ("gwstates.csv")
QOD <- merge (QOD, state.code, by.x = "ccodecow", by.y = "gwcode", all =TRUE)
QOD_new <- dplyr::select (QOD, cname, year, ccodealp, iso3c, ffp_ued, wdi_gdpcapcon2010, wbgi_rle, p_polity2, fh_fog, wdi_pop)
QOD_new$state_year <- paste (QOD_new$iso3c, QOD_new$year, sep = "+")
Database$wdi_gdpcapcon2010 <- as.numeric(Database$wdi_gdpcapcon2010)
Database <- merge(Database, QOD_new ,by.x="state_year",by.y="state_year", all=TRUE)
nrow(na.omit(Database))
names(Database)[names(Database) == "ffp_ued"] <- "economic inequality"
names(Database)[names(Database) == "wbgi_rle"] <- "rule of law (estimate)"
names(Database)[names(Database) == "wdi_gdpcapcon2010"] <- "GDP per capita (2010 US dollar)"
names(Database)[names(Database) == "polity2"] <- "p_polity2"
names(Database)[names(Database) == "wdi_pop"] <- "population"
names(Database)[names(Database) == "fh_fog"] <- "government functioning"

Database$p_polity2_square <- Database$p_polity2 * Database$p_polity2

## Manage data - ECONOMIC

# add a GDP quantile

deciles <- quantile(Database$`GDP per capita (2010 US dollar)`, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
Database$GDP_quan <- cut(Database$`GDP per capita (2010 US dollar)`, 
                               breaks = deciles, include.lowest = TRUE)
table (Database$GDP_quan)
boxplot(Database$all.number ~ Database$GDP_quan)


## add POLITY group
?deciles

Database$DEM_quan <- cut(Database$p_polity2,
                         breaks = c(-10, -5, 5, 10), include.lowest = TRUE)
table (Database$DEM_quan)



### final 
Database <- Database[Database$year1 != 2019, ]
setwd ("/Users/zhaowangyin/Desktop/Research_displaced_2/Analysis Data")
write.csv(Database, "Database.csv")



