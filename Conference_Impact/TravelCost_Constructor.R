##### TravelCost_Constructor -- last modified 6.26.19
#   Functions to calculate travel costs between any MSA and any destination point. 
#
#     NOTES: 
#
#     OUTPUT PART 1 (Adoption and Use Graphts): 
# 
#     MAJOR EDITS:  
#         - 7.5.19: Change therapist location to weighted average of all patient MSAs, rather than just modes.
#         - 7.5.19: Make travel cost a % of salary, and calculate opportunity cost differently based on provider type.
#         - TBA:Add in API calculated driving times, public transportation, etc. 
################################################################################


##### User Input
want_graph <- F # Do you want to save graphs?
conf_type <- 2 # Key: 1 = FBT Conferences, 2 = Pharma Conferences
threshold <- c(0.01, 0.02, 0.05, 0.10, 0.15, 0.85, 0.9, 0.95, 0.98, 0.99) # (To get multiple thresholds, run this option)
# threshold <- 0.05 # (to get about 100 therapists in each conference)
####################


# Packages
library(readxl)
library(xlsx)
library(tidyverse)
library(tidylog)
library(zoo) # Useful for date formats
library(reshape2)
library(here)
library(maps)
library(sf)
library(tigris)
library(geosphere)
library(igraph)
library(fastDummies)
# sf Data Options
options(tigris_class = "sf")
options(tigris_use_cache = T)


### Map Data Setup
msa <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_cbsa") # MSAs
mdiv <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_metdiv") # Metro divisions
msa <- msa[, c("GEOID", "NAME", "NAMELSAD", "LSAD", "geometry")]
mdiv <- mdiv[, c("CBSAFP", "METDIVFP", "NAME", "NAMELSAD", "LSAD", "geometry")] %>% rename(GEOID = METDIVFP)
msa$CBSAFP <- msa$GEOID
mymap <- rbind(msa, mdiv)
rm(msa, mdiv)
mymap$state <- gsub('.*, ', '', mymap$NAME)
mymap <- mymap[!grepl("AK", mymap$state) & !grepl("HI", mymap$state) & !grepl("PR", mymap$state),]
# State map (to superimpose)
st <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_state") # State Boundaries
st <- st[(st$STUSPS != "AK" & st$STUSPS != "HI" & st$STATEFP < 60), ] # Only continental US
# Latitude and longitude for each region
mymap$centroids <- st_transform(mymap, 29101) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>% st_geometry()
mymap$longitude <- st_coordinates(mymap$centroids)[,1]
mymap$latitude <- st_coordinates(mymap$centroids)[,2]
#####


### Therapist Location Setup
#   List of all therapist locations in my sample -- each therapist is a geometric average of their clients (who are all assigned the centerpoint of their MSA)
load(here("2_Data", "Specs_TherapyTreatments.Rda"))

locdata <- mydata[which(mydata$MSA != 0 & !is.na(mydata$MSA)), ]
tomerge <- mymap[, c("GEOID", "longitude", "latitude")]
st_geometry(tomerge) <- NULL
colnames(tomerge) <- c("MSA", "avg_long", "avg_lat")
tomerge$MSA <- as.numeric(tomerge$MSA)
locdata <- left_join(locdata, tomerge, by = 'MSA')
rm(tomerge)

# Merging in the lat/long from missing MSAs
extra_MSAs <- read_excel(here("2_Data/MSA_ShapeFile", "MSA_Dictionary.xlsx"), sheet = 2) %>% 
  select(MSA_old, latitude, longitude) %>% drop_na(latitude)
locdata <- merge(locdata, extra_MSAs, all.x = T, by.x = 'MSA', by.y = 'MSA_old') %>% rename(tomerge_lat1 = latitude, tomerge_long1 = longitude)
locdata$avg_lat <- ifelse(is.na(locdata$avg_lat), locdata$tomerge_lat1, locdata$avg_lat)
locdata$avg_long <- ifelse(is.na(locdata$avg_long), locdata$tomerge_long1, locdata$avg_long)
rm(extra_MSAs)

# Removing non-continental coordinates
locdata <- locdata[!is.na(locdata$avg_lat), ]

# Keeping a list of MSAs and their centers
myMSAs <- locdata %>% group_by(MSA) %>% summarize(avg_lat = mean(avg_lat), avg_long = mean(avg_long))

# Collapsing to patient-provider-month level
spec_locs <- locdata %>% group_by(datemon, PROVID, ENROLID) %>% summarize(avg_long = mean(avg_long), 
                                                                          avg_lat = mean(avg_lat))

# Geometric midpoints for each provider-month
spec_locs <- spec_locs %>% ungroup() %>% mutate(lat = avg_lat * pi / 180, 
                                           lon = avg_long * pi / 180, 
                                           x = cos(lat) * cos(lon), 
                                           y = cos(lat) * sin(lon), 
                                           z = sin(lat)) %>%
  group_by(datemon, PROVID) %>% summarize(x = mean(x), y = mean(y), z = mean(z)) %>% mutate(mean_long = atan2(y, x) * 180 / pi, 
                                                                                            hyp = sqrt(x * x + y * y),
                                                                                            mean_lat = atan2(z, hyp) * 180 / pi) %>%
  select(datemon, PROVID, mean_long, mean_lat)
locdata <- left_join(locdata, spec_locs, by = c("PROVID", "datemon"))

# Drop if average distance between patient and therapist is over 100 miles
mydata <- locdata %>% group_by(MSA, PROVID, datemon) %>% mutate(dist = distGeo(c(first(mean_long), first(mean_lat)), c(first(avg_long), first(avg_lat))) * 0.000621371) %>%
  group_by(PROVID, datemon) %>% mutate(drop = ifelse(mean(dist) >= 100, 1, 0)) %>% 
  filter(drop == 0) %>% select(-c(drop, avg_long, avg_lat)) %>% rename(avg_long = mean_long, avg_lat = mean_lat)

rm(spec_locs, locdata)
#####


# Map 1: Therapist locations 
if (want_graph) {
  graphdata <- mydata %>% group_by(avg_lat, avg_long) %>% summarize(count = n_distinct(PROVID))
  # could filter to a certain quarter to avoid dot overload
  
  locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = graphdata, aes(x=avg_long, y = avg_lat, size=count), color = 'red') +
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Location of Specialists') + scale_size(guide = F) + labs(x = "", y = "") + 
    theme(plot.title = element_text(size = 16, face = "bold"))
  
  ggsave(locgraph, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Spec_Locations_2.png"), width = 18, height = 9)
}
rm(spec_locs)
#####


##### PART 1: AIR TRAVEL COSTS

# Air data setup
if (file.exists(here("2_Data/Conference_Impacts", "Airports.Rda"))) {
  load(here("2_Data/Conference_Impacts", "Airports.Rda"))
} else {
  # A. Map of airports
  airports <- read_excel(here("2_Data/Conference_Impacts", "AirFare_Data.xlsx"))
  
  # Making sure all airports are geo-located
  airports <- airports %>% group_by(city1) %>% mutate(Geocoded_City1 = first(Geocoded_City1))
  airports <- airports %>% group_by(city2) %>% mutate(Geocoded_City2 = first(Geocoded_City2))
  
  # Cleaning out the "(Metropolitan Area)" text to extract coordinates
  airports$Geocoded_City1 <- gsub("(Metropolitan Area)", "", airports$Geocoded_City1, fixed = T)
  airports$Geocoded_City2 <- gsub("(Metropolitan Area)", "", airports$Geocoded_City2, fixed = T)
  
  airports$origin_point <- gsub("[\\(\\)]", "", regmatches(airports$Geocoded_City1, gregexpr("\\(.*?\\)", airports$Geocoded_City1)))
  airports$destination_point <- gsub("[\\(\\)]", "", regmatches(airports$Geocoded_City2, gregexpr("\\(.*?\\)", airports$Geocoded_City2)))
  
  # Some manual adjustments
  # sort(unique(airports[which(airports$origin_point == ""), ]$Geocoded_City1))
  airports[which(airports$Geocoded_City1 == "Ithaca/Cortland, NY"), ]$origin_point <- "42.4910851,-76.4606914"
  airports[which(airports$Geocoded_City2 == "Ithaca/Cortland, NY"), ]$destination_point <- "42.4910851,-76.4606914"
  airports[which(airports$Geocoded_City1 == "Quad Cities, IL "), ]$origin_point <- "41.4496381,-90.5104646"
  airports[which(airports$Geocoded_City2 == "Quad Cities, IL "), ]$destination_point <- "41.4496381,-90.5104646"
  
  # Some of the airports are coded as the same point in the data file -- this fixes (hopefully all of) those mistakes
  airports <- airports %>% rename(ORIGIN = `Airport 1`, DEST = `Airport 2`)
  airports[which(airports$ORIGIN == "SFB"), ]$origin_point <- "28.7793953,-81.2378799"
  airports[which(airports$DEST == "SFB"), ]$destination_point <- "28.7793953,-81.2378799"
  airports[which(airports$ORIGIN == "MIA"), ]$origin_point <- "25.7958771,-80.2892449"
  airports[which(airports$DEST == "MIA"), ]$destination_point <- "25.7958771,-80.2892449"
  airports[which(airports$ORIGIN == "CAE"), ]$origin_point <- "33.9419214,-81.1241955"
  airports[which(airports$DEST == "CAE"), ]$destination_point <- "33.9419214,-81.1241955"
  airports[which(airports$ORIGIN == "PWM"), ]$origin_point <- "43.8277488,-70.216715"
  airports[which(airports$DEST == "PWM"), ]$destination_point <- "43.8277488,-70.216715"
  
  
  # All airports and their coordinates
  all_airports <- data.frame(unique(union(airports$origin_point, airports$destination_point)))
  all_airports$lat <- as.numeric(gsub(",.*","",all_airports$unique.union.airports.origin_point..airports.destination_point..))
  all_airports$long <- as.numeric(gsub("^.*\\,","",all_airports$unique.union.airports.origin_point..airports.destination_point..))
  all_airports <- all_airports %>% select(long, lat) %>% filter(long >= -140) %>% distinct()
  
  # Other merging info
  airports$quarter <- as.yearqtr(paste(airports$Year, airports$quarter, sep="-0"), format = "%Y-%q")
  airports$city1 <- gsub("(Metropolitan Area)", "", airports$city1, fixed = T)
  airports$city2 <- gsub("(Metropolitan Area)", "", airports$city2, fixed = T)
  
  # B. Merging in flight times
  vars_to_keep <- c("YEAR", "FL_DATE", "ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "ACTUAL_ELAPSED_TIME")
  for (i in 2007:2017) {
    for (j in 1:4) {
      filenam <- paste("C:\\Users\\alcobe\\Downloads\\Artisanal Innovation\\2_Data\\Conference_Impacts\\", i, 'Q', j, '_ONTIME_REPORTING.csv', sep="")
      if (i == 2007 & j == 1) {
        airtime <- read.csv(filenam)
        airtime <- airtime %>% select(vars_to_keep)
      } else {
        tomerge <- read.csv(filenam)
        tomerge <- tomerge %>% select(vars_to_keep)
        airtime <- rbind(airtime, tomerge)
      }
    }
  }
  rm(tomerge)
  
  # Quarterly averages for each flight pattern
  airtime$quarter <- as.yearqtr(as.Date(airtime$FL_DATE, format = "%Y-%m-%d"))
  airtime <- airtime %>% group_by(quarter, ORIGIN, DEST) %>% summarise(traveltime = mean(ACTUAL_ELAPSED_TIME, na.rm = T))
  airports <- left_join(airports, airtime, by = c("quarter", "ORIGIN", "DEST"))
  # Add in switched origin/destination columns if necessary
  airtime2 <- airtime %>% rename(ORIGIN = DEST, DEST = ORIGIN, tomerge = traveltime) %>% select(ORIGIN, everything())
  airports <- left_join(airports, airtime2, by = c("quarter", "ORIGIN", "DEST"))
  airports$traveltime <- ifelse(is.na(airports$traveltime), airports$tomerge, airports$traveltime)
  airports <- airports %>% select(-tomerge)
  
  # If there isn't data on flight time between airports, infer it from average time by distance in that quarter
  for (i in 2007:2017) {
    for (j in 1:4) {
      q <- as.yearqtr(paste(i, " Q", j, sep = ""))
      time_per_mile <- as.numeric(airports %>% filter(quarter == q) %>% drop_na(traveltime) %>% 
                                    mutate(test = traveltime / nsmiles) %>% ungroup %>% summarize(mean = mean(test)))
      airports$traveltime <- ifelse(is.na(airports$traveltime) & airports$quarter == q, time_per_mile * airports$nsmiles, airports$traveltime)
    }
  }
  airports <- airports[which(airports$Year < 2018), ] # Drop last year 
  # plot(density(airports$traveltime/60)) # Graph of travel times in hours. 
  
  rm(airtime, airtime2)
}


# Map of airports, if wanted
if (want_graph) {
  locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = all_airports, aes(x=long, y = lat), color = 'blue') +
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Location of Airports') + labs(x = "", y = "") + 
    theme(plot.title = element_text(size = 16, face = "bold"))
  
  ggsave(locgraph, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Airport_Locations.png"), width = 18, height = 9)
}


### Function for travel cost between origin and destination
# Give me an origin (specialist location), destination (conference), and date. 
# Steps: 1. Connect origin to closest airports (by driving time)
#        2. Connect destination to closest airports (by driving time)
#        3. Constructs network where each edge is weighted by travel cost (opportunity cost of time included)
#        4. Identifies shortest path from origin to destination
#        5. Returns travel route, time, and cost.

find_my_path <- function(origin, destination, date, map_path = F, # User-specifies: note that date must be in quarter
                         n_airports = 5, drivespeed = 30, gascost = 2.79, mpg = 30, salary_hr = 40.87, extra_air_time = 180) # Defaults
  # map_path:  Would you like a map of the person's travel?
  # n_airports: How many closest airports to consider? (default to 5)
  # drivespeed: Average driving speed between origin/destination and airport (default to 30)
  # gascost: Price of gas (default to $2.79 in 2018 dollars)
  # mpg: MPG of a car (default to 30mpg)
  # salary_hr: Hourly salary for opportunity cost of time (default to $85k/yr --> $40.87/hr in 2018 USD)
  # extra_air_time: Extra time to take into account for air travel (default to 3 hours)
  
  {
  # 0. All airports and their coordinates available at the current time
  network <- airports[which(airports$quarter == as.yearqtr(date)),c("fare", "origin_point", "destination_point", "traveltime")]
  all_airports <- data.frame(unique(union(network$origin_point, network$destination_point)))
  colnames(all_airports) <- "points"
  all_airports$lat <- as.numeric(gsub(",.*","",all_airports$points))
  all_airports$long <- as.numeric(gsub("^.*\\,","",all_airports$points))
  all_airports <- all_airports %>% select(long, lat) %>% filter(long >= -140) %>% distinct()
    # It's okay if this doesn't remove any rows -- just double checking. 
  
  # 1. Find distances from origin to all airports
  nodes <- rbind(origin, destination)
  distances <- data.frame(distm(all_airports, nodes)*0.000621371)  # Distances are in miles
  names(distances) <- c("origin", "destination")
  
  # 2. Keep only the closest n airports for each origin/destination
  origin_airport_locs <- data.frame(slice(all_airports, order(distances$origin)[1:n_airports]),
                                    dist = distances[order(distances$origin)[1:n_airports],]$origin)
  dest_airport_locs <- data.frame(slice(all_airports, order(distances$destination)[1:n_airports]), 
                                  dist = distances[order(distances$destination)[1:n_airports], ]$destination)
  # Adding in travel time between locations -- assume average speed of 30mph
  origin_airport_locs$traveltime <- (origin_airport_locs$dist / drivespeed) * 60 # Keep travel time in minutes
  dest_airport_locs$traveltime <- (dest_airport_locs$dist / drivespeed) * 60 # Keep travel time in minutes
  # Adding in gas cost -- based on mpg and gascost
  origin_airport_locs$fare <- (origin_airport_locs$dist / mpg) * gascost
  dest_airport_locs$fare <- (dest_airport_locs$dist / mpg) * gascost
  
  # 3. Add connections between origin/destination + closest airports to network
  origin_airport_locs$origin_point <- paste(origin[2],origin[1],sep=", ")
  origin_airport_locs$destination_point <- paste(origin_airport_locs$lat, origin_airport_locs$long, sep=", ")
  origin_airport_locs <- origin_airport_locs %>% select(fare, origin_point, destination_point, traveltime)
  dest_airport_locs$origin_point <- paste(destination[2],destination[1],sep=", ")
  dest_airport_locs$destination_point <- paste(dest_airport_locs$lat, dest_airport_locs$long, sep=", ")
  dest_airport_locs <- dest_airport_locs %>% select(fare, origin_point, destination_point, traveltime)
  network <- rbind(network, origin_airport_locs, dest_airport_locs)
  
  # 3.5 Connect origin + destination points with driving time
  drivedist <- distGeo(origin, destination)*0.000621371
  tomerge <- data.frame(origin_point = paste(origin[2],origin[1],sep=", "), 
                        destination_point = paste(destination[2],destination[1],sep=", "), 
                        fare = drivedist/mpg * gascost, 
                        traveltime = drivedist/(2*drivespeed) * 60) # For now assume driving to airport doubles average speed
  network <- rbind(network, tomerge)
  
  # 4. Measure of travel cost -- fare + opportunity cost of time (including extra time in airport)
  network$travelcost <- network$fare + (network$traveltime + extra_air_time)/60 * salary_hr
  
  # 5. Identify shortest path (in travelcost terms) from origin to destination
  levels <- with(network, unique(c(origin_point, destination_point)))
  network$origin_point <- as.numeric(factor(network$origin_point, levels = levels))
  network$destination_point <- as.numeric(factor(network$destination_point, levels = levels))
  network <- network %>% select(origin_point, destination_point, travelcost)
  
  mynet <- graph_from_data_frame(d=network, vertices=1:length(levels), directed = F)
  # plot(mynet) # Plots the network (but not on a map)
  
  mypath <- shortest_paths(mynet, 
                           from = which(levels == paste(origin[2],origin[1],sep=", ")), 
                           to = which(levels == paste(destination[2],destination[1],sep=", ")), 
                           weights = network$travelcost, output = 'vpath')
  
  # 6. If desired, map path
  path <- levels[V(mynet)[mypath$vpath[[1]]]]
  if (map_path) {
    graphdata <- data.frame(path)
    graphdata$lat <- as.numeric(gsub(",.*","",graphdata$path))
    graphdata$long <- as.numeric(gsub("^.*\\,","",graphdata$path))
    graphdata$color <- 1 # Coloring origin and destination separately from airports.
    graphdata[1, ]$color <- 0
    graphdata[nrow(graphdata), ]$color <- 2
    if (nrow(graphdata) == 2) {
      graphdata$color <- factor(graphdata$color, labels = c("Origin", "Destination"))
    } else {
      graphdata$color <- factor(graphdata$color, labels = c("Origin", "Airports", "Destination"))
    }

    locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = graphdata, aes(x=long, y = lat, color = color), size = 2.5) + 
      geom_path(data = graphdata, aes(x=long, y=lat), color = 'blue', linetype = 'dotted', size = 1) +
      theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
      theme_minimal()+ggtitle('Sample Travel Path') + labs(x = "", y = "", color = "Travel Map") + 
      theme(plot.title = element_text(size = 16, face = "bold"))
    ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts", "SampleTravelMap.png"), width = 18, height = 9)
  }
  
  # 7. Return path, total travel time, and total travel cost
  path_ids <- V(mynet)[mypath$vpath[[1]]]
  cost <- 0
  for (i in 1:(length(path)-1)) {
    cost <- cost + network[which((network$origin_point == path_ids[i] & network$destination_point == path_ids[i+1]) | 
                                   (network$destination_point == path_ids[i] & network$origin_point == path_ids[i+1])),]$travelcost
  }
  
  # drove <- ifelse(length(path) == 2, 1, 0) # Gives an indicator for who drove/did not drive. 
  toreturn <- c(path, cost)# , drove)
  return(toreturn)
}
####################


##### PART 2: Travel costs for each therapist + heat map
# Salary information by provider type -- measured in 2018 USD (source: BLS National Occupational Employment and Wage Estimates)
# Link: https://www.bls.gov/bls/blswage.htm
salaries <- data.frame(provtype = c(20, 21, 22, 23, 25, 31, 35, 37, 40, 200, 206, 240, 365, 400, 458, 822, 824, 825, 845, 853, 860), 
                       label = c("MH Fac", "MH Fac", "SA Fac", "MH Day Care", "Rehab Fac", "Extended Care", "Res Tmt Center", 
                                 "Day/Night Care Center", "MH Fac", "MD (nec)", "Multispecialty Physician Group", "Family Practice", 
                                 "Psychiatry", "Pediatrician", "Child Psychiatry", "Nursing", "Psychiatric Nurse", "Nurse Practitioner", 
                                 "Physician Assistant", "Therapist", "Psychologist"), 
                       salary_hr = c(21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 96.68, 96.68, 96.68, 
                                     105.95, 82, 105.95, 34.48, 65.40, 51.46, 52.22, 26.50, 37.99))


create_treatment_groups <- function(conf_date, conf_loc, threshold = 0.1, want_graph = F, normed = T) { 
  # Identifies treatment and control groups: 
  # conf_date: Conference date
  # conf_loc: Conference location
  # Threshold: quantile threshold for ID'ing treatment group (must be between 0 and 1)
  # want_graph: If you want to save graphs
  # normed: Whether you also want a treatment group normalized by income
  
  conf_month <- as.yearmon(conf_date)
  conf_q <- as.yearqtr(conf_date)
  
  spec_locs <- mydata[which(mydata$datemon == conf_month), ] %>% group_by(PROVID) %>% summarize(provtype = first(STDPROV), 
                                                                                                avg_lat = first(avg_lat), 
                                                                                                avg_long = first(avg_long))
  spec_locs <- left_join(spec_locs, salaries[, c("provtype", "salary_hr")], by = "provtype")
  
  spec_locs$travelcost <- rep(NA, nrow(spec_locs))
  for (i in 1:nrow(spec_locs)) {
    path <- find_my_path(c(spec_locs$avg_long[i], spec_locs$avg_lat[i]), conf_loc, conf_q, salary_hr = spec_locs$salary_hr)
    spec_locs$travelcost[i] <- as.numeric(path[length(path)])
  }
  
  # Identifying treatment group
  spec_locs$treatment <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, threshold), 1, 0)
  if (normed) {
    spec_locs$travelcost_normed <- spec_locs$travelcost / spec_locs$salary_hr # Normalize travel cost by hourly salary
    spec_locs$treatment_normed <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, threshold), 1, 0)
  }
  
  # Graphs, if wanted
  if (want_graph) {
    # Some cities to add to the density
    cities <- data.frame(names = c("NYC", "Chicago", "Los Angeles", "New Haven", "Austin", "Atlanta", "Mountain Home, ID"), 
                         x = c(269.55, 672.84, 1085.31,209.57, 854.91, 105.95, 1180), 
                         y = c(0.00116, 0.0017, 0.00049, 0.00091, 0.00146, 0.00032, 0.00009))
    
    # Distribution of Travel Costs -- unnormalized
    densgraph <- ggplot(spec_locs, aes(x=travelcost)) + geom_density(size = 3, color='#1FA49A') + 
      geom_vline(xintercept = quantile(spec_locs$travelcost, threshold), size = 2, linetype = 'dashed', color = 'red') + 
      scale_x_continuous(limits = c(0, 1300), breaks = c(0, 250, 500, 750, 1000, 1250)) +
      geom_point(data=cities, aes(x=x, y=y,label=names), size = 6, color="#2F6066") + 
      geom_label(data = cities, aes(x=x,y=y,label=names), hjust=0, vjust =0, size=6) +
      labs(x = "Travel Costs (2018 USD)", y = "Density") + ggtitle('Estimated Distribution of Travel Costs: Sep 2012 Conference in Boston, MA') + 
      theme_minimal() + theme(plot.title = element_text(size = 26, face = "bold"), 
                              axis.text=element_text(size=16),
                              axis.title=element_text(size=20,face="bold"))
    densgraph 
    
    # Heat map of travel costs -- unnormalized
    locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = travelcost), size =5) +
      geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_c(direction = -1, option = "B") + 
      theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
      theme_minimal()+ggtitle('Estimated Specialist Travel Cost: Sep 2012 Conference in Boston, MA') + scale_size(guide = F) + 
      labs(x = "", y = "", color = "Travel Cost ($)") + 
      theme(plot.title = element_text(size = 26, face = "bold"), legend.title = element_text(size=20), legend.text = element_text(size=16), 
            axis.text = element_blank())

    # Map of treatment group -- unnormalized
    treatgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(treatment)), size = 3) +
      geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
      theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
      theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 10%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    # Map of various treatment groups (unnormalized)
    altthresh1 <- 0.05
    altthresh2 <- 0.15
    spec_locs$alttreat1 <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, altthresh1), 1, 0)
    spec_locs$alttreat2 <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, altthresh2), 1, 0)
    
    treat2graph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(alttreat1)), size = 3) +
      geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
      theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
      theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 5%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    treat3graph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(alttreat2)), size = 3) +
      geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
      theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
      theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 15%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    # Saving graphs
    ggsave(densgraph, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TravelCostDistribution.png"), width = 18, height = 9)
    ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TravelCostMap.png"), width = 18, height = 9)
    ggsave(treatgraph, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TreatmentGroupMap.png"), width = 18, height = 9)
    ggsave(treat2graph, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_Alt1TreatmentGroupMap.png"), width = 18, height = 9)
    ggsave(treat3graph, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_Alt2TreatmentGroupMap.png"), width = 18, height = 9)
    
    if (normed) {
      # Distribution of Travel Costs -- normalized
      densgraph_normed <- ggplot(spec_locs, aes(x=travelcost_normed)) + geom_density(size = 1) + 
        geom_vline(xintercept = quantile(spec_locs$travelcost_normed, threshold), size = 1.3, linetype = 'dashed', color = 'red') + 
        labs(x = "Travel Costs (as multiple of hourly salaries)", y = "Density") + ggtitle('Estimated Distribution of Travel Costs: Sep 2012 Conf in Boston, MA') 
        theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"), 
                                axis.text=element_text(size=14),
                                axis.title=element_text(size=16,face="bold"))
      
      # Heat map of travel costs -- normalized
      locgraph_normed <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = travelcost_normed), size = 3) +
        geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_c(direction = -1) + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
        theme_minimal()+ggtitle('Specialist Travel Cost: Sep 2012 Conf in Boston, MA') + scale_size(guide = F) + labs(x = "", y = "", color = "Travel Cost (mult of salary)") + 
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      # Map of treatment group -- normalized
      treatgraph_normed <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(treatment_normed)), size = 3) +
        geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
        theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 10%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      # Map of various treatment groups (normalized)
      spec_locs$alttreat1_normed <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, altthresh1), 1, 0)
      spec_locs$alttreat2_normed <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, altthresh2), 1, 0)
      
      treat2graph_normed <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(alttreat1_normed)), size = 3) +
        geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
        theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 5%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      treat3graph_normed <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(alttreat2_normed)), size = 3) +
        geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_d() + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
        theme_minimal()+ggtitle('Treatment Group: Sep 2012 Conf in Boston, MA (threshold = 15%)') + scale_size(guide = F) + labs(x = "", y = "", color = "Treated") + 
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      ggsave(densgraph_normed, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TravelCostDistribution_normed.png"), width = 18, height = 9)
      ggsave(locgraph_normed, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TravelCostMap_normed.png"), width = 18, height = 9)
      ggsave(treatgraph_normed, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_TreatmentGroupMap_normed.png"), width = 18, height = 9)
      ggsave(treat2graph_normed, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_Alt1TreatmentGroupMap_normed.png"), width = 18, height = 9)
      ggsave(treat3graph_normed, file = here("4_Output/Figures/Conference_Impacts", "2012Boston_Alt2TreatmentGroupMap_normed.png"), width = 18, height = 9)
    }
  }
  
  # Returning data set for use in regression
  if (normed) {
    treat_inds <- spec_locs[, c("PROVID", "treatment", "treatment_normed")]
  } else {
    treat_inds <- spec_locs[, c("PROVID", "treatment")]
  }
  
  treat_inds
}


# Runs for either FBT conferences or Pharma conferences, depending on user input at beginning. 
if (conf_type == 1) {
  
  # Sample Conference graphs
  conf_date <- as.Date("2012-09-28", format = "%Y-%m-%d") # Date of conference (needs to be able to be turned into a yearqtr format)
  conf_loc <- c(-71.266289, 42.396099)
  treat_inds_0_05 <- create_treatment_groups(conf_date, conf_loc, threshold = 0.05, want_graph = F)
  treat_inds_0_1 <- create_treatment_groups(conf_date, conf_loc, threshold = 0.1, want_graph = T) # Spit out the graphs for this one as an example 
  treat_inds_0_15 <- create_treatment_groups(conf_date, conf_loc, threshold = 0.15, want_graph = F)
  ####################
  
  
  ### For each conference in confdata, construct data on treatment status as above
  confdata <- read.xlsx(here("2_Data/Conference_Impacts", "Conferences.xlsx"), sheetName = 'FBT Confs for Professionals')
  confdata <- confdata[!(confdata$State == "CAN"), ] # For now, ignore the Montreal conference. 
  confdata$Date <- as.Date(confdata$Date, format = "%Y-%m-%d")
  
  # Constructs a file (or files) with treatment/control groups for each conference/threshold
  if (file.exists(here("2_Data/Conference_Impacts", "allconfs_tmtgroups_FBT.Rda"))) {
    load(here("2_Data/Conference_Impacts", "allconfs_tmtgroups_FBT.Rda"))
} else {
    for (i in 1:nrow(confdata)) {
      conf_month <- as.yearmon(confdata$Date[i])
      conf_q <- as.yearqtr(confdata$Date[i])
      conf_loc <- c(confdata$Longitude[i], confdata$Latitude[i])
      
      spec_locs <- mydata[which(mydata$datemon == conf_month), ] %>% group_by(PROVID) %>% summarize(provtype = first(STDPROV), 
                                                                                                    avg_lat = first(avg_lat), 
                                                                                                    avg_long = first(avg_long))
      spec_locs <- left_join(spec_locs, salaries[, c("provtype", "salary_hr")], by = "provtype")
      
      spec_locs$travelcost <- rep(NA, nrow(spec_locs))
      spec_locs$travelcost_normed <- rep(NA, nrow(spec_locs))
      for (j in 1:nrow(spec_locs)) {
        path <- find_my_path(c(spec_locs$avg_long[j], spec_locs$avg_lat[j]), conf_loc, conf_q)
        spec_locs$travelcost[j] <- as.numeric(path[length(path)])
      }
      for (j in 1:nrow(spec_locs)) {
        path <- find_my_path(c(spec_locs$avg_long[j], spec_locs$avg_lat[j]), conf_loc, conf_q, salary_hr = spec_locs$salary_hr)
        spec_locs$travelcost_normed[j] <- as.numeric(path[length(path)])
      }
      spec_locs$travelcost_normed <- spec_locs$travelcost_normed / spec_locs$salary_hr
      spec_locs$conf_no <- rep(i, nrow(spec_locs))
      
      for (thresh in threshold) {
        varname <- paste("treatment", thresh, sep = "_")
        varname2 <- paste("treatment_normed", thresh, sep = "_")
        spec_locs[[varname]] <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, thresh), 1, 0)
        spec_locs[[varname2]] <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, thresh), 1, 0)
      }
    
      nam <- paste("spec_locs_confno", i, sep = "_")
      assign(nam, spec_locs)
      rm(spec_locs)
    }
    
    # Combining data frames
    dflist <- mget(paste0("spec_locs_confno_", 1:nrow(confdata)))
    allconfs <- bind_rows(dflist) 
    
    # Merging in conference info to each treated 
    confdata$conf_no <- as.numeric(confdata$conf_no)
    allconfs <- left_join(allconfs, confdata[, names(confdata) %in% c("conf_no", "Date", "Latitude", "Longitude", "Conference.Name")], by = "conf_no")
    
    # Saving to avoid having to rerun analysis above
    save(allconfs, file = here("2_Data/Conference_Impacts", "allconfs_tmtgroups_FBT.Rda")) # 
}
  
  for (thresh in threshold) {
    startname <- paste("treatment", thresh, sep = "_")
    endname <- paste("tmtdate", thresh, sep = "_")
    startname2 <- paste("treatment_normed", thresh, sep = "_")
    endname2 <- paste("tmtdate_normed", thresh, sep = "_")
    
    alltreated <- allconfs %>% group_by(PROVID) %>% summarize(!!endname := min(Date[!!sym(startname) == 1]),
                                                          !!endname2 := min(Date[!!sym(startname2) == 1]))

    alltreated[[endname]] <- as.Date(alltreated[[endname]], format = "%Y-%m-%d")
    alltreated[[endname2]] <- as.Date(alltreated[[endname2]], format = "%Y-%m-%d")
    
    mydata <- left_join(mydata, alltreated, by = "PROVID")
    rm(alltreated)
  }
  save(mydata, file = here("2_Data/Conference_Impacts", "Analysis_FBT.Rda")) # Save to avoid rerunning everything above 
  ####################
  
  
  # ##### Map of Conferences (no treatment/control split)
  # if (want_graph) {
  #   # Conferences only
  #   graphdata <- confdata %>% group_by(Latitude, Longitude) %>% summarize(count = n_distinct(Date))
  #   
  #   locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = graphdata, aes(x=Longitude, y = Latitude, size=1.5), color = 'blue') +
  #     theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  #     theme_minimal()+ggtitle('Location of Conferences') + scale_size(guide = F) + labs(x = "", y = "") + 
  #     theme(plot.title = element_text(size = 16, face = "bold"))
  #   
  #   ggsave(locgraph, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Conf_Locations.png"), width = 18, height = 9)
  #   
  #   # Conferences + Therapists
  #   tomerge <- mydata[which(mydata$dateq == "2014 Q1"), ] %>% group_by(avg_lat, avg_long) %>% summarize(count = n_distinct(PROVID)) %>% rename(Latitude = avg_lat,
  #                                                                                                                                              Longitude = avg_long)
  #   
  #   
  #   locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = tomerge, aes(x=Longitude, y = Latitude, size=count), color = 'red') +
  #     geom_jitter(data = graphdata, aes(x=Longitude, y = Latitude, size=30), color = 'blue') +
  #     theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  #     theme_minimal()+ggtitle('Location of Conferences') + scale_size(guide = F) + labs(x = "", y = "") + 
  #     theme(plot.title = element_text(size = 16, face = "bold"))
  #   
  #   ggsave(locgraph, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Conf_Locations_2.png"), width = 18, height = 9)
  # }
  # ####################
  # 
  # 
  # # Map with conferences + treated group(s)
  # # Only try to make this with a given threshold, not with all of them (obvi)
  # if (want_graph) {
  #   # Conferences + Therapists (colored by treatment)
  #   graphdata <- confdata %>% group_by(Latitude, Longitude) %>% summarize(count = n_distinct(Date))
  #   
  #   mydata <- mydata %>% group_by(PROVID) %>% mutate(wastreated = max(treated), wastreated_normed = max(treated_normed))
  #   specsdata <- mydata[which(mydata$dateq == "2014 Q1"), ] %>% group_by(avg_lat, avg_long) %>% summarize(count = n_distinct(PROVID), 
  #                                                                                                         wt = max(wastreated), 
  #                                                                                                         wt_n = max(wastreated_normed)) %>%
  #     rename(Latitude = avg_lat, Longitude = avg_long)
  #   
  #   locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = specsdata, aes(x=Longitude, y = Latitude, color = as.factor(wt)), size = 2) +
  #     scale_colour_viridis_d() + 
  #     geom_jitter(data = graphdata, aes(x=Longitude, y = Latitude, size=2), color = 'blue') +
  #     theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  #     theme_minimal()+ggtitle('Current Split of Treatment/Control Specialists') + scale_size(guide = F) + labs(x = "", y = "", color = "Treatment") + 
  #     theme(plot.title = element_text(size = 16, face = "bold"))
  #   ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts", "AllConfs_TmtGroups.png"), width = 18, height = 9)
  #   
  #   locgraph_n <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = specsdata, aes(x=Longitude, y = Latitude, color = as.factor(wt_n)), size = 2) +
  #     scale_colour_viridis_d() + 
  #     geom_jitter(data = graphdata, aes(x=Longitude, y = Latitude, size=2), color = 'blue') +
  #     theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  #     theme_minimal()+ggtitle('Current Split of Treatment/Control Specialists (Travel Cost Normed by Salary)') + scale_size(guide = F) + 
  #     labs(x = "", y = "", color = "Treatment") + 
  #     theme(plot.title = element_text(size = 16, face = "bold"))
  #   ggsave(locgraph_n, file = here("4_Output/Figures/Conference_Impacts", "AllConfs_TmtGroups_Normed.png"), width = 18, height = 9)
  # }
  
  
} else if (conf_type == 2) {
  # This is for pharmaceutical conferences.
  # # Using midpoint of each MSA in data, rather than specialist locations (assign treatment/control MSAs, not specialists)
  # load(here("2_Data/Conference_Impacts", "OutpatientPharma_Linked.RData"))
  # newspec_locs <- data.frame(MSA = sort(unique(pharma$MSA))) %>% filter(MSA > 0)
  # newspec_locs <- left_join(newspec_locs, myMSAs)
  
  ### For each conference in confdata, construct data on treatment status as above
  confdata <- read.xlsx(here("2_Data/Conference_Impacts", "Conferences.xlsx"), sheetName = 'Pharmacology Conferences')
  confdata <- confdata[!(confdata$State == "CAN"), ] # For now, ignore the Montreal conference. 
  confdata <- confdata[!is.na(confdata$conf_no), ] # Also ignore all un-numbered conferences (such as those for academics only)
  confdata$Date <- as.Date(confdata$Date, format = "%Y-%m-%d")
  
  # Constructs a file (or files) with treatment/control groups for each conference/threshold
  # NOTE: Uses the same data (mydata) filtered to only prescribers. Uses new conferences (pharma conferences). Prescription variables are added at the end. 
  if (file.exists(here("2_Data/Conference_Impacts", "allconfs_tmtgroups_Pharma.Rda"))) {
    load(here("2_Data/Conference_Impacts", "allconfs_tmtgroups_Pharma.Rda"))
  } else {
    prescibers <- mydata[which(mydata$STDPROV %in% c(200,206,240,365,400,458,824,825)), ] # Keeping only those that prescribe
    
    for (i in 1:nrow(confdata)) {
      conf_month <- as.yearmon(confdata$Date[i])
      conf_q <- as.yearqtr(confdata$Date[i])
      conf_loc <- c(confdata$Longitude[i], confdata$Latitude[i])
      
      spec_locs <- prescibers[which(prescibers$datemon == conf_month), ] %>% group_by(PROVID) %>% summarize(provtype = first(STDPROV), 
                                                                                                            avg_lat = first(avg_lat), 
                                                                                                            avg_long = first(avg_long))
      spec_locs <- left_join(spec_locs, salaries[, c("provtype", "salary_hr")], by = "provtype")
      
      spec_locs$travelcost <- rep(NA, nrow(spec_locs))
      spec_locs$travelcost_normed <- rep(NA, nrow(spec_locs))
      for (j in 1:nrow(spec_locs)) {
        path <- find_my_path(c(spec_locs$avg_long[j], spec_locs$avg_lat[j]), conf_loc, conf_q)
        spec_locs$travelcost[j] <- as.numeric(path[length(path)])
      }
      for (j in 1:nrow(spec_locs)) {
        path <- find_my_path(c(spec_locs$avg_long[j], spec_locs$avg_lat[j]), conf_loc, conf_q, salary_hr = spec_locs$salary_hr)
        spec_locs$travelcost_normed[j] <- as.numeric(path[length(path)])
      }
      spec_locs$travelcost_normed <- spec_locs$travelcost_normed / spec_locs$salary_hr
      spec_locs$conf_no <- rep(i, nrow(spec_locs))
      
      for (thresh in threshold) {
        varname <- paste("treatment", thresh, sep = "_")
        varname2 <- paste("treatment_normed", thresh, sep = "_")
        spec_locs[[varname]] <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, thresh), 1, 0)
        spec_locs[[varname2]] <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, thresh), 1, 0)
      }
      
      nam <- paste("spec_locs_confno", i, sep = "_")
      assign(nam, spec_locs)
      rm(spec_locs)
    }
    
    # Combining data frames
    dflist <- mget(paste0("spec_locs_confno_", 1:nrow(confdata)))
    allconfs <- bind_rows(dflist) 
    
    # Merging in conference info to each treated 
    confdata$conf_no <- as.numeric(confdata$conf_no)
    allconfs <- left_join(allconfs, confdata[, names(confdata) %in% c("conf_no", "Date", "Latitude", "Longitude", "Conference.Name")], by = "conf_no")
    
    # Saving to avoid having to rerun analysis above
    save(allconfs, file = here("2_Data/Conference_Impacts", "allconfs_tmtgroups_pharma_08232019.Rda")) # 
  }
  
  # Merging treatment group definitions in with main data
  for (thresh in threshold) {
    startname <- paste("treatment", thresh, sep = "_")
    endname <- paste("tmtdate", thresh, sep = "_")
    startname2 <- paste("treatment_normed", thresh, sep = "_")
    endname2 <- paste("tmtdate_normed", thresh, sep = "_")
    
    alltreated <- allconfs %>% group_by(PROVID) %>% summarize(!!endname := min(Date[!!sym(startname) == 1]),
                                                              !!endname2 := min(Date[!!sym(startname2) == 1]))
    
    alltreated[[endname]] <- as.Date(alltreated[[endname]], format = "%Y-%m-%d")
    alltreated[[endname2]] <- as.Date(alltreated[[endname2]], format = "%Y-%m-%d")
    
    mydata <- left_join(mydata, alltreated, by = "PROVID")
    rm(alltreated)
  }
  
  # Merging pharma behavior in with main data
  load(here("2_Data/Conference_Impacts/", "OutpatientPharma_Linked.RData"))
  final <- final %>% select(ENROLID, PROVID, datemon, pharma_date, num_scrips, olanzapine, drug_names)  %>% group_by(ENROLID, PROVID, datemon) %>%
    summarize(num_scrips = sum(num_scrips), olanzapine = max(olanzapine), drug_names = list(drug_names))
  mydata <- left_join(mydata, final, by = c("PROVID", "ENROLID", "datemon"))
  mydata[is.na(mydata$num_scrips),]$num_scrips <- 0
  mydata[is.na(mydata$olanzapine),]$olanzapine <- 0
  mydata <- mydata %>% group_by(PROVID, datemon) %>% mutate(any_scrips = ifelse(max(num_scrips) > 0, 1, 0), any_olanzapine = ifelse(max(olanzapine) == 1, 1, 0))
  
  # Save data to avoid re-running
  save(mydata, file = here("2_Data/Conference_Impacts", "Analysis_Pharma_08232019.Rda")) 
}
####################