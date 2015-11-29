# load relevant libraries
library(ggplot2)
library(rgdal)
library(rgeos)
library(dplyr)
library(viridis)

# read in data, clean some column names
sf <- read.csv("sanfrancisco_incidents_summer_2014.csv")
sf$long <- sf$X
sf$lat <- sf$Y

# create a map from specific categories to 9 broad categories
cats <- list("ARSON" = "violent", 
"ASSAULT" = "violent", 
"BRIBERY" = "theft", 
"BURGLARY" = "theft", 
"DISORDERLY CONDUCT"  = "other", 
"DRIVING UNDER THE INFLUENCE"  = "substances", 
"DRUG/NARCOTIC"  = "substances", 
"DRUNKENNESS"  = "substances", 
"EMBEZZLEMENT"   = "theft", 
"EXTORTION"  = "theft", 
"FAMILY OFFENSES"  = "other", 
"FORGERY/COUNTERFEITING"  = "other", 
"FRAUD"  = "other", 
"GAMBLING"  = "other", 
"KIDNAPPING"  = "violent", 
"LARCENY/THEFT"  = "theft", 
"LIQUOR LAWS"  = "substances", 
"LOITERING"  = "other", 
"MISSING PERSON" = "missing person", 
"NON-CRIMINAL"  = "noncriminal", 
"OTHER OFFENSES"  = "other", 
"PORNOGRAPHY/OBSCENE MAT"  = "other", 
"PROSTITUTION"  = "other",
"ROBBERY"  = "theft", 
"RUNAWAY"  = "other", 
"SECONDARY CODES"  = "other", 
"STOLEN PROPERTY"  = "theft", 
"SUICIDE"  = "other", 
"SUSPICIOUS OCC"  = "suspicious occurance", 
"TRESPASS"  = "other", 
"VANDALISM"  = "other", 
"VEHICLE THEFT"  = "theft", 
"WARRANTS" = "warrants", 
"WEAPON LAWS" = "weapons")

sf$broadCategories <- as.factor(unlist(cats[sf$Category]))
  
# read in zip code boundary information from https://data.sfgov.org/Geographic-Locations-and-Boundaries/San-Francisco-ZIP-Codes-Zipped-Shapefile-Format-/9q84-kc2y
sfzips <- readOGR("sfzipcodes/","sfzipcodes") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
# sfneighborhoods <- readOGR("planning_neighborhoods/","planning_neighborhoods") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

# create a spatial frame from the crime data
sfCrimes <- sf
coordinates(sfCrimes) <- c("long", "lat")

# match the coordinate systems
proj4string(sfCrimes) <- proj4string(sfzips)

# generate a vector for which zip each crime happend in.
zipIn <- over(sfCrimes, as(sfzips, "SpatialPolygons"))

# assign each crime to a zip code based on lattitude and longitude
zipInfoForCrimes <- over(sfCrimes, sfzips)
# join zips to the crimes
sf <- cbind(sf, zipInfoForCrimes)
# remove the crimes that are not within a zipcode (in this set there are about 84 of those, most are just beyond the city boundary)
sf <- subset(sf, !is.na(ZIP_CODE))

# convert spatial data to a dataframe for easier plotting
zipData <- fortify(sfzips)
sfzips@data$id <- rownames(sfzips@data)
zipData <- merge(zipData, sfzips@data, by="id")

# create centroid labels
zipLabels <- zipData %>% group_by(ZIP_CODE) %>% summarise(long = mean(range(long)), lat = mean(range(lat)))

# Key
ggplot() + 
  geom_polygon(data = zipData, aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  geom_text(data = zipLabels, aes(x = long, y = lat, label = ZIP_CODE), size =4) +
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Zip codes in San Francisco")


# Where is the crime?
crimeCounts <- sf %>% group_by(ZIP_CODE) %>% summarize(count = length(IncidntNum))
crimeCounts$id <- crimeCounts$zip

mergedZip <- merge(zipData, crimeCounts, by="ZIP_CODE")

ggplot() + 
  geom_polygon(data = zipData, aes(x = long, y = lat, group = group), fill="grey", color = "white")  +
  geom_point(data = sf, aes(x = long, y = lat), alpha = 0.1) +
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Crime reports by zip code")


ggplot() + 
  geom_polygon(data = mergedZip, aes(x = long, y = lat, group = group, fill=count), color = "white") + 
  scale_fill_viridis() + 
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Crime reports by zip code")


# crimes per capita
pop <- read.csv("DEC_10_SF1_P1_with_ann.csv")
pop$ZIP_CODE <- pop$Id2
pop$population <- pop$Total
pop <- select(pop, ZIP_CODE, population)

crimeCounts <- sf %>% group_by(ZIP_CODE) %>% summarize(count = length(IncidntNum))
crimeCounts$id <- crimeCounts$ZIP_CODE
crimeCounts <- merge(crimeCounts, pop)
crimeCounts$crimePerCapita <- crimeCounts$count/crimeCounts$pop
  
mergedZip <- merge(zipData, crimeCounts, by="ZIP_CODE")

ggplot() + 
  geom_polygon(data = mergedZip, aes(x = long, y = lat, group = group, fill=population), color = "white") + 
  scale_fill_viridis() + 
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Population by zip code")

ggplot() + 
  geom_polygon(data = mergedZip, aes(x = long, y = lat, group = group, fill=crimePerCapita), color = "white") + 
  scale_fill_viridis() + 
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Per capita crime reports")

#### What kinds of crime
ggplot() + 
  geom_polygon(data = zipData, aes(x = long, y = lat, group = group), fill="grey", color = "white")  +
  geom_point(data = sf, aes(x = long, y = lat), alpha = 0.1) +
  facet_wrap(~broadCategories) + 
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Crime reports by zip code")

# which zips contribute the most crime?
crimeCounts <- sf %>% group_by(ZIP_CODE, broadCategories) %>% summarize(count = length(IncidntNum))
crimesInZipz <- crimeCounts %>% group_by(ZIP_CODE) %>% summarise(totInZip = sum(count)) %>% mutate(percZip = totInZip / sum(totInZip), jn = 1)
crimesInCats <- crimeCounts %>% group_by(broadCategories) %>% summarise(totInCat = sum(count)) %>% mutate(percCat = totInCat / sum(totInCat), jn = 1)

proportions <- select(plyr::join(crimesInZipz, crimesInCats, type="full"), ZIP_CODE, broadCategories, percZip, percCat)
proportions$expectedN <- nrow(sf)*proportions$percZip*proportions$percCat

crimeCountsWithExpected <- merge(crimeCounts, proportions, all = TRUE)
crimeCountsWithExpected$count <- ifelse(is.na(crimeCountsWithExpected$count), 0, crimeCountsWithExpected$count)

crimeCountsWithExpected$diff <- crimeCountsWithExpected$count - crimeCountsWithExpected$expectedN

crimeCountsWithExpected$id <- crimeCountsWithExpected$zip

mergedZip <- merge(zipData, crimeCountsWithExpected, by="ZIP_CODE")

ggplot() + 
  geom_polygon(data = mergedZip, aes(x = long, y = lat, group = group, fill=diff), color = "white") + 
  facet_wrap(~broadCategories) + 
  scale_fill_viridis("difference") + 
  coord_fixed() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title="Crime reports by zip code")


