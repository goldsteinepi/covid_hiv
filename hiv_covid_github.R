#################
# HIV/COVID-19 spatial maps
# Citation: Welles SL, Goldstein ND. An ecological exploration of the cooccurrence of HIV and COVID-19 infection in Philadelphia, Pennsylvania. Manuscript in preparation.
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

#zctas for crosswalk: from UDS mapper https://www.udsmapper.org/zcta-crosswalk.cfm
zcta = read.csv("zip_to_zcta_2019.csv", as.is=T, stringsAsFactors=F)

#retrieve census variables of interest: using tidycensus but could manually obtain from FactFinder
#2018 is most recent year data are available per package
census_api_key("paste api key here")
population = get_acs(geography="zcta", table="B01003", year=2018, output="wide")
income = get_acs(geography="zcta", table="B19013", year=2018, output="wide")
occupation = get_acs(geography="zcta", table="S2401", year=2018, output="wide")

#census-based deprivation index: details provided here https://towardsdatascience.com/a-census-based-deprivation-index-using-r-7aa738da697c, https://www.ncbi.nlm.nih.gov/pubmed/17031568
deprivation = get_acs(geography="zcta", variables=c("B17001_002", "B17001_001", "B06009_002" , "B06009_001","B09008_011","B09008_001","B08124_002", "B08124_001", "B25014_005","B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013","B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008","C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004","B19001_005", "B19001_006", "B19001_001"), output="wide", year=2018)

#ZCTA shapefile national data from Census: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=ZIP+Code+Tabulation+Areas
us_zcta = readOGR("tl_2019_us_zcta510/", "tl_2019_us_zcta510")


### CLEAN and JOIN DATA ###

#crosswalk Philly zip to zctas
covid_zip$zcta = NA
for (i in 1:nrow(covid_zip)) {
  z = zcta$ZCTA[which(zcta$ZIP_CODE==as.numeric(covid_zip$zip_code[i]))]
  covid_zip$zcta[i] = ifelse(length(z)>0, z, NA)
}
rm(i,z,zcta)

philly_data = data.frame("Positive_tests"=NA, "Total_tests"=NA, "Negative_tests"=NA, "HIV"=NA, "Population"=NA, "ZCTA"=unique(covid_zip$zcta), stringsAsFactors=F)

for (i in 1:nrow(philly_data)) {
  philly_data$Positive_tests[i] = sum(covid_zip$count[covid_zip$zcta==philly_data$ZCTA[i] & covid_zip$covid_status=="POS"], na.rm=T)
  philly_data$Negative_tests[i] = sum(covid_zip$count[covid_zip$zcta==philly_data$ZCTA[i] & covid_zip$covid_status=="NEG"], na.rm=T)
  philly_data$HIV[i] = hiv_zip$hiv_per_100k[hiv_zip$zcta==philly_data$ZCTA[i]]
}
rm(i)

#high risk occupation %s: management/business/science/arts:healthcare, service, natural resources/construction/maintenance, production/transport/moving
occupation$Occupation = (occupation$S2401_C01_015E + occupation$S2401_C01_018E + occupation$S2401_C01_029E + occupation$S2401_C01_033E) / occupation$S2401_C01_001E

#join population, household income to Philly zcta
philly_data = merge(philly_data,population[,c("GEOID","B01003_001E")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,income[,c("GEOID","B19013_001E")],by.x="ZCTA",by.y="GEOID")
philly_data = merge(philly_data,occupation[,c("GEOID","Occupation")],by.x="ZCTA",by.y="GEOID")

#recode and clean
philly_data$Total_tests = philly_data$Positive_tests + philly_data$Negative_tests
philly_data$Population = philly_data$B01003_001E
philly_data$Income = philly_data$B19013_001E
philly_data$NAME = NULL
philly_data$B01003_001E = NULL
philly_data$B19013_001E = NULL

rm(covid_zip, hiv_zip, population, income, occupation)

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
deprivation$census_NDI = principal(deprivation_matrix,nfactors = 1)$scores 

philly_data = merge(philly_data, deprivation[,c("GEOID","census_NDI")], by.x="ZCTA", by.y="GEOID", all.x=T, all.y=F)

rm(deprivation, deprivation_matrix)

#subset shapefile based on philly
philly_sf = us_zcta[us_zcta$ZCTA5CE10 %in% unique(philly_data$ZCTA), ]
rm(us_zcta)


### SAVE DATA ###

save.image("philly_spatial.RData")


### FUNCTIONS ###

library("psych") #describe, describeBy
library("gmodels") #CrossTable
library("sf") #spatial data
library("RColorBrewer") #color palette
library("raster") #area function
library("spdep") #satial dependency
library("maptools") #union function
library("spatialreg") #for spatial regressions


### LOAD DATA ###

load("philly_spatial.RData")

#create a COVID per capita
philly_data$COVID = philly_data$Positive_tests / philly_data$Population * 100000

#create a per 100k ratio
philly_data$Ratio = philly_data$COVID / philly_data$HIV

#exclude suppressed data areas
philly_data = philly_data[philly_data$ZCTA!="19109" & philly_data$ZCTA!="19113", ]

#create a center city indicator: https://en.wikipedia.org/wiki/Center_City,_Philadelphia
philly_data$Center_city = ifelse(philly_data$ZCTA=="19102" | philly_data$ZCTA=="19103" | philly_data$ZCTA=="19106" | philly_data$ZCTA=="19107" | philly_data$ZCTA=="19146" | philly_data$ZCTA=="19147", 1, 0)


### DESCRIPTIVES ###

describe(philly_data$COVID)
describe(philly_data$HIV)
describe(philly_data$Ratio)

#center city has fewer cases of COVID, more cases of HIV
describeBy(philly_data$COVID, philly_data$Center_city)
wilcox.test(philly_data$COVID ~ philly_data$Center_city)

describeBy(philly_data$HIV, philly_data$Center_city)
wilcox.test(philly_data$HIV ~ philly_data$Center_city)

describeBy(philly_data$Ratio, philly_data$Center_city)
wilcox.test(philly_data$Ratio ~ philly_data$Center_city)

#center city less deprivation, greater population density, greater household income, less "high risk" occupations
describeBy(philly_data$census_NDI, philly_data$Center_city)
wilcox.test(philly_data$census_NDI ~ philly_data$Center_city)

describeBy(philly_sf_joined$Density, philly_sf_joined$Center_city)
wilcox.test(philly_sf_joined$Density ~ philly_sf_joined$Center_city)

describeBy(philly_data$Income, philly_data$Center_city)
wilcox.test(philly_data$Income ~ philly_data$Center_city)

describeBy(philly_data$Occupation, philly_data$Center_city)
wilcox.test(philly_data$Occupation ~ philly_data$Center_city)


### EXPLORATORY MAPS ###

#set CRS; https://epsg.io/3652
philly_sf_proj = st_transform(st_as_sf(philly_sf), crs=3652)

#merge case data with shapefile
philly_sf_joined = merge(x=philly_sf_proj, y=philly_data, by.x="ZCTA5CE10", by.y="ZCTA", all.x=T, duplicateGeoms=T)

#create a population density variable
philly_sf_joined$Density = philly_sf_joined$Population / area(philly_sf) 

#choropleth shading for ratio
choropleth_col = rep("#FFFFFF",nrow(philly_sf_joined))
choropleth_col[which(philly_sf_joined$Ratio<=1)] = rev(brewer.pal(4, "Reds"))[na.omit(as.numeric(cut(philly_sf_joined$Ratio[philly_sf_joined$Ratio<=1], breaks=c(quantile(philly_sf_joined$Ratio[philly_sf_joined$Ratio<=1], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]
choropleth_col[which(philly_sf_joined$Ratio>1)] = brewer.pal(4, "Blues")[na.omit(as.numeric(cut(philly_sf_joined$Ratio[philly_sf_joined$Ratio>1], breaks=c(quantile(philly_sf_joined$Ratio[philly_sf_joined$Ratio>1], probs = seq(0, 1, by = 0.25), na.rm=T)), include.lowest=TRUE)))]

#draw choropleth ratio map: export as 1200 width
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="darkgray", main="\nA)", cex.main=2)
text(t(sapply(slot(as_Spatial(philly_sf_joined), "polygons"), function(i) slot(i, "labpt"))), cex=0.6, labels=round(philly_sf_joined$Ratio,2))

#add center city demarcation
cc = subset(philly_sf_joined,Center_city==1 | ZCTA5CE10=="19109")
cc = unionSpatialPolygons(as_Spatial(cc), rep(T,nrow(cc)))
plot(cc, border="black", lwd=3, add=T)

#choropleth shading for density
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Density, breaks=c(quantile(philly_sf_joined$Density, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]

#draw choropleth density map: export as 1200 width
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="darkgray", main="\nB)", cex.main=2)
plot(cc, border="black", lwd=3, add=T)

#choropleth shading for deprivation
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$census_NDI, breaks=c(quantile(philly_sf_joined$census_NDI, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]

#draw choropleth deprivation map: export as 1200 width
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="darkgray", main="\nC)", cex.main=2)
plot(cc, border="black", lwd=3, add=T)

#choropleth shading for household income
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Income, breaks=c(quantile(philly_sf_joined$Income, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]

#draw choropleth income map: export as 1200 width
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="darkgray", main="\nC)", cex.main=2)
plot(cc, border="black", lwd=3, add=T)

#choropleth shading for occupation
choropleth_col = brewer.pal(8, "Greys")[as.numeric(cut(philly_sf_joined$Occupation, breaks=c(quantile(philly_sf_joined$Occupation, probs=seq(0, 1, by = 0.20), na.rm=T)), include.lowest=TRUE))]

#draw choropleth occupation map: export as 1200 width
par(mar=rep(0.1,4))
plot(philly_sf_joined$geometry,col=choropleth_col, border="darkgray", main="\nC)", cex.main=2)
plot(cc, border="black", lwd=3, add=T)


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
# model_ols = lm(Ratio ~ scale(Density) + census_NDI, data=philly_sf_joined)
# summary(model_ols)
# 
# #check for spatial autocorrelation in residuals
# lm.morantest(model_ols,wt_list)
# 
# #LaGrange Multiplier tests to inform the spatial regression approach
# lm.LMtests(model_ols, wt_list, test="all") #suggests a spatial lag model is appropriate
# 
# #compare a spatial lag Y versus spatial mixed model
# model_lag = lagsarlm(Ratio ~ scale(Density) + census_NDI, data=philly_sf_joined, wt_list)
# model_mixed = lagsarlm(Ratio ~ scale(Density) + census_NDI, data=philly_sf_joined, wt_list, type="mixed")
# 
# summary(model_ols); AIC(model_ols)
# summary(model_lag) #spatial lag Y model appears to fit the data the best
# summary(model_mixed)
# 
# #obtain estimates
# summary(impacts(model_lag,tr=trW(as(wt_list, "CsparseMatrix"), type="mult"), R=1000), zstats=T)

