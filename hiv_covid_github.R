#################
# HIV/COVID-19 spatial maps
# Citation: Goldstein ND, Webster JL, Robinson LF, Welles SL. Use of Neighborhood Infection Incidence to Identify Disparities of COVID-19 and HIV Occurrence in Philadelphia, Pennsylvania. Manuscript in preparation.
# 7/29/20 -- Neal Goldstein
#################


### FUNCTIONS ###

library("tidycensus") #retrieve ACS data, note if error installing on MacOS see: https://github.com/r-quantities/units/issues/1
library("rgdal") #read shapefile
library("psych") #PCA


### READ DATA ###

#Philly data retreived Jul 29 2020 from: https://www.opendataphilly.org/dataset/covid-cases
covid_zip = read.csv("covid_cases_by_zip.csv", as.is=T, stringsAsFactors=F)

#HIV data retrieved Jul 29 2020 from: https://aidsvu.org/local-data/united-states/northeast/pennsylvania/philadelphia/
hiv_zip = read.csv("hiv_zip_philly.csv", as.is=T, stringsAsFactors=F)

#Overdose data: https://www.phila.gov/media/20200511105852/CHART-v5e4.pdf
overdose_zip = read.csv("overdose_zip_philly.csv", as.is=T, stringsAsFactors=F, na.strings=c(""," "))

#zctas for crosswalk: from UDS mapper https://www.udsmapper.org/zcta-crosswalk.cfm
zcta = read.csv("zip_to_zcta_2019.csv", as.is=T, stringsAsFactors=F)

#retrieve census variables of interest: using tidycensus but could manually obtain from FactFinder
#2018 is most recent year data are available per package
census_api_key("paste api key here")
population = get_acs(geography="zcta", table="B01003", year=2018, output="wide")
income = get_acs(geography="zcta", table="B19013", year=2018, output="wide")
occupation = get_acs(geography="zcta", table="S2401", year=2018, output="wide")
race = get_acs(geography="zcta", table="B02001", year=2018, output="wide")
ethnicity = get_acs(geography="zcta", table="B03003", year=2018, output="wide")
household = get_acs(geography="zcta", table="B11009", year=2018, output="wide")
age = get_acs(geography="zcta", table="S0101", year=2018, output="wide")

#census-based deprivation index: details provided here https://towardsdatascience.com/a-census-based-deprivation-index-using-r-7aa738da697c, https://www.ncbi.nlm.nih.gov/pubmed/17031568
deprivation = get_acs(geography="zcta", variables=c("B17001_002", "B17001_001", "B06009_002" , "B06009_001","B09008_011","B09008_001","B08124_002", "B08124_001", "B25014_005","B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013","B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008","C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004","B19001_005", "B19001_006", "B19001_001"), output="wide", year=2018)

#US ZCTA shapefile national data from Census: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=ZIP+Code+Tabulation+Areas
us_zcta = readOGR("tl_2019_us_zcta510/", "tl_2019_us_zcta510")

#US state shapefile national data from Census: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=States+%28and+equivalent%29
us_state = readOGR("tl_2019_us_state/", "tl_2019_us_state")


### CLEAN and JOIN DATA ###

#crosswalk Philly zip to zctas
covid_zip$zcta = NA
for (i in 1:nrow(covid_zip)) {
  z = zcta$ZCTA[which(zcta$ZIP_CODE==as.numeric(covid_zip$zip_code[i]))]
  covid_zip$zcta[i] = ifelse(length(z)>0, z, NA)
}
rm(i,z,zcta)

#philly data (add 19112 that was suppressed in data pulls)
philly_data = data.frame("Positive_tests"=NA, "Total_tests"=NA, "Negative_tests"=NA, "HIV"=NA, "Population"=NA, "Overdoses"=NA, "ZCTA"=c(unique(covid_zip$zcta),"19112"), stringsAsFactors=F)

for (i in 1:nrow(philly_data)) {
  if (sum(covid_zip$zcta==philly_data$ZCTA[i])>0) {
    philly_data$Positive_tests[i] = sum(covid_zip$count[covid_zip$zcta==philly_data$ZCTA[i] & covid_zip$covid_status=="POS"], na.rm=T)
    philly_data$Negative_tests[i] = sum(covid_zip$count[covid_zip$zcta==philly_data$ZCTA[i] & covid_zip$covid_status=="NEG"], na.rm=T)
  }
  
  if (sum(hiv_zip$zcta==philly_data$ZCTA[i])>0) {
    philly_data$HIV[i] = hiv_zip$hiv_per_100k[hiv_zip$zcta==philly_data$ZCTA[i]]
  }
  
  if (sum(overdose_zip$zcta==philly_data$ZCTA[i])>0) {
    philly_data$Overdoses[i] = overdose_zip$deaths[overdose_zip$zcta==philly_data$ZCTA[i]]
  }
}
rm(i)

#high risk occupation %s: management/business/science/arts:healthcare, service, natural resources/construction/maintenance, production/transport/moving
occupation$Occupation = (occupation$S2401_C01_015E + occupation$S2401_C01_018E + occupation$S2401_C01_029E + occupation$S2401_C01_033E) / occupation$S2401_C01_001E

#black %
race$Black = round(race$B02001_003E / race$B02001_001E * 100, 1)

#Hispanic %
ethnicity$Hispanic = round(ethnicity$B03003_003E / ethnicity$B03003_001E * 100, 1)

#male partenered household % (married and cohabiting)
household$Male_partner_household = round(household$B11009_003E / household$B11009_001E * 100, 1)

#overwrite GEOID with ZCTA
population$GEOID = gsub("ZCTA5 ", "", population$NAME)
income$GEOID = gsub("ZCTA5 ", "", income$NAME)
occupation$GEOID = gsub("ZCTA5 ", "", occupation$NAME)
race$GEOID = gsub("ZCTA5 ", "", race$NAME)
ethnicity$GEOID = gsub("ZCTA5 ", "", ethnicity$NAME)
household$GEOID = gsub("ZCTA5 ", "", household$NAME)
age$GEOID = gsub("ZCTA5 ", "", age$NAME)
deprivation$GEOID = gsub("ZCTA5 ", "", deprivation$NAME)

#join population, household income to Philly zcta
philly_data = merge(philly_data,population[,c("GEOID","B01003_001E")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,income[,c("GEOID","B19013_001E")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,occupation[,c("GEOID","Occupation")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,race[,c("GEOID","Black")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,ethnicity[,c("GEOID","Hispanic")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,household[,c("GEOID","Male_partner_household")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,age[,c("GEOID","S0101_C01_032E")],by.x="ZCTA",by.y="GEOID")

#recode and clean
philly_data$Total_tests = philly_data$Positive_tests + philly_data$Negative_tests
philly_data$Population = philly_data$B01003_001E
philly_data$Income = philly_data$B19013_001E
philly_data$Age = philly_data$S0101_C01_032E
philly_data$NAME = NULL
philly_data$B01003_001E = NULL
philly_data$B19013_001E = NULL
philly_data$S0101_C01_032E = NULL

rm(covid_zip, hiv_zip, population, income, occupation, race, ethnicity, household, age, overdose_zip)

#create deprivation index: https://towardsdatascience.com/a-census-based-deprivation-index-using-r-7aa738da697c, https://www.ncbi.nlm.nih.gov/pubmed/17031568
deprivation$pct_poverty = deprivation$B17001_002E / deprivation$B17001_001E
deprivation$pct_noHS = deprivation$B06009_002E / deprivation$B06009_001E
deprivation$pct_FHH = deprivation$B09008_011E / deprivation$B09008_001E
deprivation$pct_mgmt = deprivation$B08124_002E / deprivation$B08124_001E 
deprivation$pct_crowd = (deprivation$B25014_005E + deprivation$B25014_006E + deprivation$B25014_007E + deprivation$B25014_011E + deprivation$B25014_012E + deprivation$B25014_013E) / deprivation$B25014_001E
deprivation$pct_pubassist = deprivation$B19058_002E / deprivation$B19058_001E
deprivation$pct_unempl = (deprivation$C23002C_021E + deprivation$C23002D_008E) / (deprivation$C23002C_017E + deprivation$C23002D_003E)
deprivation$pct_under30K = ((deprivation$B19001_002E + deprivation$B19001_003E + deprivation$B19001_004E + deprivation$B19001_005E + deprivation$B19001_006E) / deprivation$B19001_001E)
deprivation_matrix = as.matrix(deprivation[, c("pct_poverty","pct_noHS","pct_FHH","pct_mgmt","pct_crowd","pct_pubassist", "pct_unempl","pct_under30K")])
deprivation_matrix[is.nan(deprivation_matrix)] = 0
deprivation$census_ADI = principal(deprivation_matrix,nfactors = 1)$scores 

philly_data = merge(philly_data, deprivation[,c("GEOID","census_ADI")], by.x="ZCTA", by.y="GEOID", all.x=T, all.y=F)

rm(deprivation, deprivation_matrix)

#subset ZCTA shapefile based on philly
philly_sf = us_zcta[us_zcta$ZCTA5CE10 %in% unique(philly_data$ZCTA), ]
rm(us_zcta)

#subset state shapefile based on mid-atlantic
midatlantic_sf = us_state[us_state$STUSPS %in% c("PA","DE","MD","NJ","WV","VA","NY","CT","RI","MA","VT","NH"), ]
rm(us_state)


### SAVE DATA ###

save.image("philly_spatial.RData")
write.csv(philly_data, "philly_data.csv", na="", row.names=F)


### FUNCTIONS ###

library("psych") #describe, describeBy
library("gmodels") #CrossTable
library("sf") #spatial data
library("RColorBrewer") #color palette
library("raster") #area function
library("spdep") #satial dependency
library("maptools") #union function
library("spatialreg") #for spatial regressions

library(ggpubr)
library(dplyr)
library(gridExtra)


### LOAD DATA ###

load("philly_spatial.RData")


### CODE VARIABLES ###

#create a COVID per capita
philly_data$COVID = philly_data$Positive_tests / philly_data$Population * 100000

#create Z scores for each infection
philly_data$COVID_z = scale(philly_data$COVID)
philly_data$HIV_z = scale(philly_data$HIV)

#create a difference in the Z scores (COVID - HIV)
philly_data$Diff_z = philly_data$COVID_z - philly_data$HIV_z

# convert # of overdose deaths variable to factor
philly_data$factor_Overdoses = as.numeric(factor(philly_data$Overdoses, order = TRUE, levels = c("0","1-6","7-24","25-49","50-99","100+")))

#exclude suppressed data areas
philly_data$COVID_suppressed = ifelse(philly_data$ZCTA=="19109" | philly_data$ZCTA=="19112" | philly_data$ZCTA=="19113", 1, 0)
philly_data$HIV_suppressed = ifelse(philly_data$ZCTA=="19109" | philly_data$ZCTA=="19112" | philly_data$ZCTA=="19113" | philly_data$ZCTA=="19118" | philly_data$ZCTA=="19127" | philly_data$ZCTA=="19137", 1, 0)

#create a center city indicator: https://en.wikipedia.org/wiki/Center_City,_Philadelphia
philly_data$Center_city = ifelse(philly_data$ZCTA=="19102" | philly_data$ZCTA=="19103" | philly_data$ZCTA=="19106" | philly_data$ZCTA=="19107" | philly_data$ZCTA=="19146" | philly_data$ZCTA=="19147", 1, 0)


### DESCRIPTIVES ###

sum(philly_data$Population[philly_data$COVID_suppressed==0])
describe(philly_data$Population[philly_data$COVID_suppressed==0])
describe(philly_data$COVID[philly_data$COVID_suppressed==0])
sum(philly_data$COVID[philly_data$COVID_suppressed==0])
describe(philly_data$HIV[philly_data$HIV_suppressed==0])
sum(philly_data$HIV[philly_data$HIV_suppressed==0])
describe(philly_data$Diff_z)

#center city has fewer cases of COVID, more cases of HIV
describeBy(philly_data$COVID[philly_data$COVID_suppressed==0], philly_data$Center_city[philly_data$COVID_suppressed==0])
wilcox.test(philly_data$COVID[philly_data$COVID_suppressed==0] ~ philly_data$Center_city[philly_data$COVID_suppressed==0])

describeBy(philly_data$HIV[philly_data$HIV_suppressed==0], philly_data$Center_city[philly_data$HIV_suppressed==0])
wilcox.test(philly_data$HIV[philly_data$HIV_suppressed==0] ~ philly_data$Center_city[philly_data$HIV_suppressed==0])

describeBy(philly_data$Diff_z, philly_data$Center_city)
wilcox.test(philly_data$Diff_z ~ philly_data$Center_city)

#center city less deprivation, greater population density, greater household income, less "high risk" occupations
describeBy(philly_data$census_ADI, philly_data$Center_city)
wilcox.test(philly_data$census_ADI ~ philly_data$Center_city)

describeBy(philly_sf_joined$Density, philly_sf_joined$Center_city)
wilcox.test(philly_sf_joined$Density ~ philly_sf_joined$Center_city)

describeBy(philly_data$Income, philly_data$Center_city)
wilcox.test(philly_data$Income ~ philly_data$Center_city)

describeBy(philly_data$Occupation, philly_data$Center_city)
wilcox.test(philly_data$Occupation ~ philly_data$Center_city)


### SET MAPPING PARAMETERS ###

#set CRS; https://epsg.io/3652
philly_sf_proj = st_transform(st_as_sf(philly_sf), crs=3652)

#merge case data with shapefile
philly_sf_joined = merge(x=philly_sf_proj, y=philly_data, by.x="ZCTA5CE10", by.y="ZCTA", all.x=T, duplicateGeoms=T)

#create a population density variable
philly_sf_joined$Density = philly_sf_joined$Population / area(philly_sf) * 2590000

#center city Philadelphia
cc = subset(philly_sf_joined,Center_city==1 | ZCTA5CE10=="19109")
cc = unionSpatialPolygons(as_Spatial(cc), rep(T,nrow(cc)))


### REFERENCE MAP ###

## Inset map
par(mar=rep(0.1,4))
plot(midatlantic_sf, col="#EEEEEE", border="#999999")
points(y=40,x=-75.3,pch=8,col="black",lwd=4,cex=3)

## Reference map
plot(philly_sf_joined$geometry, border="#595959", lwd=0.7, cex.main=2)
text(t(sapply(slot(as_Spatial(philly_sf_joined), "polygons"), function(i) slot(i, "labpt"))), cex=0.6, labels=philly_sf_joined$ZCTA5CE10)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)


### RISK FACTOR MAPS ###

## A) high-risk occupation
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Occupation, breaks=c(quantile(philly_sf_joined$Occupation, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nA)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## B) median household income
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Income, breaks=c(quantile(philly_sf_joined$Income, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nB)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## C) population density
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Density, breaks=c(quantile(philly_sf_joined$Density, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nC)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## D) % Black
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Black, breaks=c(quantile(philly_sf_joined$Black, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nD)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## E) % Hispanic
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Hispanic, breaks=c(quantile(philly_sf_joined$Hispanic, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nE)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## F) % male-partner hh
#choropleth shading
choropleth_col = brewer.pal(4, "Greys")[as.numeric(cut(philly_sf_joined$Male_partner_household, breaks=c(unique(quantile(philly_sf_joined$Male_partner_household, probs=seq(0, 1, by = 0.25), na.rm=T))), include.lowest=TRUE))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nF)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## G) overdose deaths
#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(philly_sf_joined$factor_Overdoses)]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nG)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## H) age
# convert age to factor
philly_sf_joined$factor_Age <- factor(ifelse(philly_sf_joined$Age < 25, "< 25",
                                             ifelse(philly_sf_joined$Age < 30, "25-29",
                                                    ifelse(philly_sf_joined$Age < 35, "30-34",
                                                           ifelse(philly_sf_joined$Age < 40, "35-39", "40+")))))

#choropleth shading
choropleth_col = brewer.pal(8, "Greys")[as.numeric(philly_sf_joined$factor_Age)]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nH)", cex.main=2)
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)


### Z-SCORE MAPS ###

## A) COVID-19
#choropleth shading
choropleth_col = rep("#FFFFFF",nrow(philly_sf_joined))
choropleth_col[which(philly_sf_joined$COVID_z<=0)] = rev(brewer.pal(4, "Blues"))[na.omit(as.numeric(cut(philly_sf_joined$COVID_z[philly_sf_joined$COVID_z<=0], breaks=c(quantile(philly_sf_joined$COVID_z[philly_sf_joined$COVID_z<=0], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]
choropleth_col[which(philly_sf_joined$COVID_z>0)] = brewer.pal(4, "Reds")[na.omit(as.numeric(cut(philly_sf_joined$COVID_z[philly_sf_joined$COVID_z>0], breaks=c(quantile(philly_sf_joined$COVID_z[philly_sf_joined$COVID_z>0], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nA)", cex.main=2)
text(t(sapply(slot(as_Spatial(philly_sf_joined), "polygons"), function(i) slot(i, "labpt"))), cex=0.6, labels=round(philly_sf_joined$COVID_z,2))
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

## B) HIV Z
#choropleth shading
choropleth_col = rep("#FFFFFF",nrow(philly_sf_joined))
choropleth_col[which(philly_sf_joined$HIV_z<=0)] = rev(brewer.pal(4, "Blues"))[na.omit(as.numeric(cut(philly_sf_joined$HIV_z[philly_sf_joined$HIV_z<=0], breaks=c(quantile(philly_sf_joined$HIV_z[philly_sf_joined$HIV_z<=0], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]
choropleth_col[which(philly_sf_joined$HIV_z>0)] = brewer.pal(4, "Reds")[na.omit(as.numeric(cut(philly_sf_joined$HIV_z[philly_sf_joined$HIV_z>0], breaks=c(quantile(philly_sf_joined$HIV_z[philly_sf_joined$HIV_z>0], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]
#draw choropleth map
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nB)", cex.main=2)
text(t(sapply(slot(as_Spatial(philly_sf_joined), "polygons"), function(i) slot(i, "labpt"))), cex=0.6, labels=round(philly_sf_joined$HIV_z,2))
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)

### Differences in z-scores map - 9 categories ###
# similar: HIV above av, CoV above av; HIV av, CoV av; HIV below av, CoV below av (grays)
# dissimilar, HIV high: HIV above av, CoV below av; HIV above av, CoV av; HIV av, CoV below av (oranges)
# dissimilar, CoV high: CoV above av, HIV below av; CoV above av, HIV av; CoV av, HIV below av (greens)

# creating categories above, normal, below average categories
# "normal" is 0.5 above and below the average, z-score=0
philly_sf_joined$COVID_z_cat = ifelse(philly_sf_joined$COVID_z < -0.5, "Below", ifelse(philly_sf_joined$COVID_z < 0.5, "Average", "Above"))
philly_sf_joined$HIV_z_cat = ifelse(philly_sf_joined$HIV_z < -0.5, "Below", ifelse(philly_sf_joined$HIV_z < 0.5, "Average", "Above"))

# choropleth shading
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Average" & philly_sf_joined$COVID_z_cat == "Average")] = "#f8f8f8"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Above" & philly_sf_joined$COVID_z_cat == "Above")] = "#b6b6b6"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Below" & philly_sf_joined$COVID_z_cat == "Below")] = "#d8d8d8"

choropleth_col[which(philly_sf_joined$HIV_z_cat == "Above" & philly_sf_joined$COVID_z_cat == "Below")] = "#c66d10"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Above" & philly_sf_joined$COVID_z_cat == "Average")] = "#ea956d"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Average" & philly_sf_joined$COVID_z_cat == "Below")] = "#ebceb2"

choropleth_col[which(philly_sf_joined$HIV_z_cat == "Below" & philly_sf_joined$COVID_z_cat == "Above")] = "#336952"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Average" & philly_sf_joined$COVID_z_cat == "Above")] = "#66937e"
choropleth_col[which(philly_sf_joined$HIV_z_cat == "Below" & philly_sf_joined$COVID_z_cat == "Average")] = "#a9cdb4"

# draw choropleth map
par(xpd=TRUE)
plot(philly_sf_joined$geometry,col=choropleth_col, border="#595959", lwd=0.7, main="\nC)", cex.main=2)
text(t(sapply(slot(as_Spatial(philly_sf_joined), "polygons"), function(i) slot(i, "labpt"))), cex=0.6, labels=round(philly_sf_joined$Diff_z,2))
# legend("bottomright", 
#        legend = c("COVID average, HIV average", "COVID above average, HIV above average","COVID below average, HIV above average",
#                   "HIV above average, COVID below average", "HIV above average, COVID average","HIV average, COVID below average",
#                   "COVID above average, HIV below average", "COVID above average, HIV average","COVID average, HIV below average"), 
#        col = c("#fefefe","#c2c2c2","#ededed",
#                "#c26e00","#da8f49","#ecb37d",
#                "#3e9168","#62ae8b","#96c9a8"), 
#        pch = 15, 
#        bty ="n",
#        pt.cex = 2,
#        cex = 1,
#        text.col = "black")
#add center city demarcation
plot(cc, border="black", lwd=2, add=T)


### CORRELATION MATRIX ###

## COVID ## 
# household income, high-risk occupations, pop. density, % Black, % Hispanic

# income
COVp1 <-  ggscatter(philly_sf_joined, x = "Income", y = "COVID", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    main= "Median household income", xlab = "Income ($)", ylab = "COVID-19 cases")

# high risk occupations
COVp2 <-  ggscatter(philly_sf_joined, x = "Occupation", y = "COVID", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    main= "Percent high-risk occupatins", xlab = "%", ylab = "COVID-19 cases")

# pop. density
COVp3 <-  ggscatter(philly_sf_joined, x = "Density", y = "COVID", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    main= "Population density", xlab = "Population per 100,000", ylab = "COVID-19 cases")

# % Black
COVp4 <-  ggscatter(philly_sf_joined, x = "Black", y = "COVID", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    main= "Percent Black or African American", xlab = "%", ylab = "COVID-19 cases")

# % Hispanic
COVp5 <-  ggscatter(philly_sf_joined, x = "Hispanic", y = "COVID", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    main= "Percent Hispanic or Latino", xlab = "%", ylab = "COVID-19 cases")

# Age
COVp6 <-  ggscatter(philly_sf_joined, x = "Age", y = "COVID",
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    main= "Median Age", xlab = "Years", ylab = "COVID-19 cases per 100,000")

# COVID correlation plots combined
COVpAll <- grid.arrange(COVp1, COVp2, COVp3, COVp4, COVp5, COVp6, nrow = 1)

## HIV ##
# median household income, % male-partner household, overdose deaths, % Black, % Hispanic

# household income
HIVp1 <-   ggscatter(philly_sf_joined, x = "Income", y = "HIV", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     main= "Median household income", xlab = "Income ($)", ylab = "HIV cases")

# % male-partner household
HIVp2 <-   ggscatter(philly_sf_joined, x = "Male_partner_household", y = "HIV",
                     add = "reg.line", conf.int = TRUE,
                     cor.coef = TRUE, cor.method = "spearman",
                     main= "Percent male-partner households", xlab = "%", ylab = "HIV cases")

# overdose deaths
HIVp3 <-   ggscatter(philly_sf_joined, x = "factor_Overdoses", y = "HIV",
                     add = "reg.line", conf.int = TRUE,
                     cor.coef = TRUE, cor.method = "kendall",
                     main= "Overdose deaths in 2019", xlab = "Number of deaths", ylab = "HIV cases")

# % Black
HIVp4 <-   ggscatter(philly_sf_joined, x = "Black", y = "HIV", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "spearman",
                     main= "Percent Black or African American", xlab = "%", ylab = "HIV cases")

# % Hispanic
HIVp5 <-   ggscatter(philly_sf_joined, x = "Hispanic", y = "HIV", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "spearman",
                     main= "Percent Hispanic or Latino", xlab = "%", ylab = "HIV cases")

HIVp6 <-   ggscatter(philly_sf_joined, x = "Age", y = "HIV",
                     add = "reg.line", conf.int = TRUE,
                     cor.coef = TRUE, cor.method = "pearson",
                     main= "Median Age", xlab = "Years", ylab = "HIV cases per 100,000")

# HIV correlation plots combined
HIVpAll <- grid.arrange(HIVp1, HIVp2, HIVp3, HIVp4, HIVp5, HIVp6, nrow = 1)

## Risk Factors ##
# income x pop. density
rf1 <-   ggscatter(philly_sf_joined, x = "Income", y = "Density", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   main= "Median household income & population density", xlab = "Income ($)", ylab = "Population per 100,000")

# income x high-risk occupation
rf2 <-   ggscatter(philly_sf_joined, x = "Income", y = "Occupation", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "spearman",
                   main= "Median household income & percent high-risk occupation", xlab = "Income ($)", ylab = "%")

# high-risk occupation x pop. density
rf3 <-   ggscatter(philly_sf_joined, x = "Occupation", y = "Density", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "spearman",
                   main= "Percent high-risk occupation & population density", xlab = "%", ylab = "Population per 100,000")

# Risk factors correlation plots combined
rfAll <- grid.arrange(rf1, rf2, rf3, nrow = 1)

corrAll <- grid.arrange(COVp1, COVp2, COVp3, COVp4, COVp5, COVp6,
                        HIVp1, HIVp2, HIVp3, HIVp4, HIVp5, HIVp6,
                        rf1, rf2, rf3, nrow = 5)


### LINEAR MODELS ###

# scale percentages to 10%
philly_sf_joined$Black_10 <- philly_sf_joined$Black/10
philly_sf_joined$Hispanic_10 <- philly_sf_joined$Hispanic/10

# scale income to $10,000
philly_sf_joined$Income_10k <- philly_sf_joined$Income/10000

# scale density to 10,000
philly_sf_joined$Density_10 <- philly_sf_joined$Density/10000

# scale occupation (proportion to percentage)
philly_sf_joined$Occupation_100 <- philly_sf_joined$Occupation*100

## COVID-19 cases linear regression analysis
# outcome: COVID-19 cases per capita
# covariates: % high-risk occupation, income scaled, pop. density scaled, % Black scaled, % Hispanic scaled
covid_reg_scale <- lm(COVID ~ Occupation_100 + Income_10k + Density_10 + Black + Hispanic + Age, data=philly_sf_joined, weights=philly_sf_joined$Population)
summary(covid_reg_scale)
confint(covid_reg_scale)

## HIV linear regression analysis
# outcome: HIV incidence
# covariates: income scaled, % male-partner household, number of overdose deaths, % Black, % Hispanic, age
HIV_reg_scale <- lm(HIV ~ Income_10k + Male_partner_household + factor_Overdoses + Black + Hispanic + Age, data=philly_sf_joined, weights=philly_sf_joined$Population)
summary(HIV_reg_scale)
confint(HIV_reg_scale)

## COVID | HIV linear regression analysis
# outcome: COVID-19 cases per capita
# covariates: HIV, % Black
diff_reg_scale <- lm(COVID ~ HIV + Black, data=philly_sf_joined, weights=philly_sf_joined$Population)
summary(diff_reg_scale)
confint(diff_reg_scale)


### SPATIAL ANALYSIS ###

# #define neighbors using Queen's contiguity (spdep package)
# nb_list = poly2nb(philly_sf_joined)
# 
# #define weighting scheme
# wt_list = nb2listw(nb_list) #using row standardization
# 
# #diagnose spatial dependence using global Moran's I
# moran.mc(philly_sf_joined$Ratio, wt_list, na.action=na.omit, nsim=1000)
# 
# #OLS regression
# model_ols = lm(Ratio ~ scale(Density) + census_ADI, data=philly_sf_joined)
# summary(model_ols)
# 
# #check for spatial autocorrelation in residuals
# lm.morantest(model_ols,wt_list)
# 
# #LaGrange Multiplier tests to inform the spatial regression approach
# lm.LMtests(model_ols, wt_list, test="all") #suggests a spatial lag model is appropriate
# 
# #compare a spatial lag Y versus spatial mixed model
# model_lag = lagsarlm(Ratio ~ scale(Density) + census_ADI, data=philly_sf_joined, wt_list)
# model_mixed = lagsarlm(Ratio ~ scale(Density) + census_ADI, data=philly_sf_joined, wt_list, type="mixed")
# 
# summary(model_ols); AIC(model_ols)
# summary(model_lag) #spatial lag Y model appears to fit the data the best
# summary(model_mixed)
# 
# #obtain estimates
# summary(impacts(model_lag,tr=trW(as(wt_list, "CsparseMatrix"), type="mult"), R=1000), zstats=T)
