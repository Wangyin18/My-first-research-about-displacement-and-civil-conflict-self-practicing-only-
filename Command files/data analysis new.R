### Data analysis 
setwd ("/Users/zhaowangyin/Desktop/Research_displaced_2/Analysis Data")
Database <- read.csv ("Database.csv")



names(Database)[names(Database) == "GDP.per.capita..2010.US.dollar."] <- "GDP per capita (2010 US dollar)"
names(Database)[names(Database) == "economic.inequality"] <- "economic inequality"
names(Database)[names(Database) == "rule.of.law..estimate."] <- "rule of law (estimate)"
names(Database)[names(Database) == "polity2"] <- "p_polity2"
names(Database)[names(Database) == "wdi_pop"] <- "population"
names(Database)[names(Database) == "government.functioning"] <- "government functioning"

library (stargazer)

### DATA visualation 


### here I add code in QOD into database - UCDP

conflict_map <- dplyr::select (Database, cname, ccodealp, all.number, Country.Code)

setwd ("/Users/zhaowangyin/Desktop/Research_displaced/Original Data/ne_10m_admin_0_countries")
library (sf)
library (tmap)
world_map <- st_read("ne_10m_admin_0_countries.shp")
qtm (world_map)
conflict_map1 <- summarize(group_by(conflict_map, ccodealp), conflict_sum = sum(all.number))
world_map <- merge(world_map, conflict_map1, by.x = "ADM0_A3", by.y = "ccodealp", all = TRUE)
hist (Database_V$conflict_sum)
a <- tm_shape (world_map) + tm_polygons("conflict_sum", title = "Conflict Distribution", palette = "Greens", style="fixed", breaks = c(0, 50, 1000, 5000, 10000, 100000)) + 
  tm_layout(title = "Conflict Count Distribution from 2010-2018", title.size = 1.2,
            title.position = c(0.4, 0.22)) + tm_legend(position = c(0.04, 0.1), title.size = 1) 
a                                                                                                                                      
st_write (world_map, "world_map.shp")
tmap_save (a)


### CONFLICT TREND
library (patchwork)
conflict_per_year <- summarize(group_by(Database, year), 
                            conflict.number = sum(all.number))
intra_conflict_per_year <- summarize (group_by(Database, year), conflict.nnumber = sum (intrastate.number))

p1 <- ggplot(data = conflict_per_year, aes(x = year, y = conflict.number)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("year") + ylab("all event on organized violence") + theme_bw() 

p2 <- ggplot (data = intra_conflict_per_year, aes (x = year, y = conflict.nnumber)) + geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("year") + ylab("intrastate conflict number") + theme_bw() 

conflict_per_year$intraconflict <- intra_conflict_per_year$conflict.nnumber

p1 + p2

## another way to do it  
par(mar=c(5,5,4,5)+0.1)
bar <- barplot(conflict_per_year$conflict.number,ylim = c(0,25000),xlab = "year", ylab="Events on organized violence",col="grey",col.axis="black",col.lab="black")
mtext(conflict_per_year$year,side=1,line=1,at=bar,col="black")
mtext("Year",side=1,line=3,col="black")
par(new=T)
plot(bar,conflict_per_year$intraconflict,axes=F,ylim=c(0,60),xlab="",ylab="",col="blue",type="b", lwd = 2)
axis(4,col="red",col.ticks="black",col.axis="black")
mtext("intrastate conflict number ",side=4,line=3,col="black")



###################################################################################
### Let's try NEGATIVE BINOMIAL REGRESSION
install.packages("MASS")
library (MASS)
### for control variable
gmodel_all_c <- glm.nb (Database$all.number  ~ Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2 + Database$p_polity2_square+ Database$`government functioning` + log(Database$population))
gmodel_intr_c <- glm (Database$intrastate.dummy  ~ Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2+ Database$p_polity2_square + Database$`government functioning` + log(Database$population), family = binomial(link = "logit"))
gmodel_intr.n_c <- glm.nb (Database$intrastate.number  ~ Database$`economic inequality` +  log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2 + Database$p_polity2_square+ Database$`government functioning` + log(Database$population))


### displacement number
gmodel_all_d <- glm.nb (Database$all.number ~ log (Database$number))
gmodel_intr_d <- glm (Database$intrastate.dummy ~ log (Database$number),family = binomial(link = "logit"))
gmodel_intr_number_d <- glm.nb (Database$intrastate.number ~ log (Database$number))


### displacement

gmodel_all <- glm.nb (Database$all.number ~ log (Database$number) + Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2 + Database$p_polity2_square+ Database$`government functioning` + log(Database$population) )
gmodel_intr <- glm (Database$intrastate.dummy ~ log (Database$number) +Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2+ Database$p_polity2_square + Database$`government functioning` + log(Database$population),family = binomial(link = "logit") )
gmodel_intr_number <- glm.nb (Database$intrastate.number ~ log (Database$number) +Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2 + Database$p_polity2_square+ Database$`government functioning` + log(Database$population))


stargazer(gmodel_all_c, gmodel_intr_c, gmodel_intr.n_c,gmodel_all_d, gmodel_intr_d, gmodel_intr_number_d, gmodel_all, gmodel_intr, gmodel_intr_number, type = "text" )


### state fixed effect
Database$state_fixed <- as.factor (Database$Country.Code)

model_all_s <- glm.nb (Database$all.number ~ log (Database$number) + Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2+ Database$p_polity2_square + Database$`government functioning` + log(Database$population) + Database$state_fixed )
model_intr_s <- glm (Database$intrastate.dummy ~ log (Database$number) + Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2 + Database$p_polity2_square+ Database$`government functioning` + log(Database$population)+ Database$state_fixed  )
model_intr_number_s <- glm.nb (Database$intrastate.number ~ log (Database$number) +Database$`economic inequality` + log (Database$`GDP per capita (2010 US dollar)`) + Database$p_polity2+ Database$p_polity2_square + Database$`government functioning` + log(Database$population) + Database$state_fixed)

stargazer(model_dummy, type = "text")
stargazer(model_dummy_s, model_all_s,  model_intr_s, model_intr_number_s,type = "text")






#### Special GDP per capita quartile 

Database_gdp <- Database [Database$GDP_quan != "[211,1.63e+03]", ]
Database_gdp1 <- Database [Database$GDP_quan != "(1.51e+04,1.94e+05]", ]

gmodel_all_gdp <- glm.nb (Database_gdp$all.number ~ log (Database_gdp$number) + Database_gdp$`economic inequality` + log (Database_gdp$`GDP per capita (2010 US dollar)`) + Database_gdp$p_polity2 + Database_gdp$p_polity2_square+ Database_gdp$`government functioning` + log(Database_gdp$population) )
gmodel_intr_gdp <- glm (Database_gdp$intrastate.dummy ~ log (Database_gdp$number) +Database_gdp$`economic inequality` + log (Database_gdp$`GDP per capita (2010 US dollar)`) + Database_gdp$p_polity2+ Database_gdp$p_polity2_square + Database_gdp$`government functioning` + log(Database_gdp$population) )
gmodel_intr_number_gdp <- glm.nb (Database_gdp$intrastate.number ~ log (Database_gdp$number) +Database_gdp$`economic inequality` + log (Database_gdp$`GDP per capita (2010 US dollar)`) + Database_gdp$p_polity2 + Database_gdp$p_polity2_square+ Database_gdp$`government functioning` + log(Database_gdp$population) )

gmodel_all_gdp1 <- glm.nb (Database_gdp1$all.number ~ log (Database_gdp1$number) + Database_gdp1$`economic inequality` + log (Database_gdp1$`GDP per capita (2010 US dollar)`) + Database_gdp1$p_polity2 + Database_gdp1$p_polity2_square+ Database_gdp1$`government functioning` + log(Database_gdp1$population) )
gmodel_intr_gdp1 <- glm (Database_gdp1$intrastate.dummy ~ log (Database_gdp1$number) +Database_gdp1$`economic inequality` + log (Database_gdp1$`GDP per capita (2010 US dollar)`) + Database_gdp1$p_polity2+ Database_gdp1$p_polity2_square + Database_gdp1$`government functioning` + log(Database_gdp1$population) )
gmodel_intr_number_gdp1 <- glm.nb (Database_gdp1$intrastate.number ~ log (Database_gdp1$number) +Database_gdp1$`economic inequality` + log (Database_gdp1$`GDP per capita (2010 US dollar)`) + Database_gdp1$p_polity2 + Database_gdp1$p_polity2_square+ Database_gdp1$`government functioning` + log(Database_gdp1$population) )

stargazer(gmodel_all_gdp, gmodel_intr_gdp, gmodel_intr_number_gdp, type = "text")
stargazer(gmodel_all_gdp1, gmodel_intr_gdp1, gmodel_intr_number_gdp1, type = "text")


#### special Polity2
Database_de <- Database [Database$DEM_quan == "(-5,5]", ]
gmodel_all_de <- glm.nb (Database_de$all.number ~ log (Database_de$number) + Database_de$`economic inequality` + log (Database_de$`GDP per capita (2010 US dollar)`) + Database_de$p_polity2 + Database_de$p_polity2_square+ Database_de$`government functioning` + log(Database_de$population) )
gmodel_intr_de <- glm (Database_de$intrastate.dummy ~ log (Database_de$number) +Database_de$`economic inequality` + log (Database_de$`GDP per capita (2010 US dollar)`) + Database_de$p_polity2+ Database_de$p_polity2_square + Database_de$`government functioning` + log(Database_de$population) )
gmodel_intr_number_de <- glm.nb (Database_de$intrastate.number ~ log (Database_de$number) +Database_de$`economic inequality` + log (Database_de$`GDP per capita (2010 US dollar)`) + Database_de$p_polity2 + Database_de$p_polity2_square+ Database_de$`government functioning` + log(Database_de$population) )
stargazer( gmodel_all_de, gmodel_intr_de, gmodel_intr_number_de, type = "text")

Database_de1 <- Database [Database$DEM_quan == "[-10,-5]", ]
gmodel_all_de1 <- glm.nb (all.number ~ log (number) + `economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de1 )
gmodel_intr_de1 <- glm (intrastate.dummy ~ log (number) + `economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de1 )
gmodel_intr_number_de1 <- glm.nb (intrastate.number ~ log (number) +`economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de1 )
stargazer( gmodel_intr_de1, gmodel_intr_number_de1, type = "text")


Database_de2 <- Database [Database$DEM_quan == "(5,10]", ]
gmodel_all_de2 <- glm.nb (all.number ~ log (number) + `economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de2 )
gmodel_intr_de2 <- glm (intrastate.dummy ~ log (number) + `economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de2 )
gmodel_intr_number_de2 <- glm.nb (intrastate.number ~ log (number) +`economic inequality` + log (`GDP per capita (2010 US dollar)`) + p_polity2 + p_polity2_square + `government functioning` + log(population), data = Database_de2 )
stargazer(gmodel_all_de2, gmodel_intr_de2, gmodel_intr_number_de2, type = "text")



