##### Validation Exercises -- last modified 8.28.19
#   Uses my validation samples to explore the correlations between T^a, T^b, and D. 
#
#     NOTES: 
#
#     OUTPUT PART 1 (Adoption and Use Graphts): 
# 
#     MAJOR EDITS:  
#         - TBA: Update airport data with 2019, not 2017 Q4. 
################################################################################


##### User Input
want_graph <- T # Do you want to save graphs?
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
library(knitr)
library(kableExtra)
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


### Therapist Location Map (+ Conference)
#   List of all therapist locations in my validation sample
validate <- read_excel(here("2_Data/Conference_Impacts/Validation_Samples", "ICED_2019.xlsx"), sheet = "Unique Addresses")
validate$Latitude <- as.numeric(validate$Latitude)
validate$Longitude <- as.numeric(validate$Longitude)
validate <- validate %>% filter(Address != "London") # don't want international attendees. 

if (want_graph) {
  locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = validate, aes(x= Longitude, y = Latitude), color = '#2F6066', size=5) +
    geom_point(aes(y=as.numeric(40.7625228), x=as.numeric(-73.9836468)), color = '#A7E482', size = 6, shape=18) + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Location of ICED 2019 Attendees') + scale_size(guide = F) + labs(x = "", y = "") + 
    theme(plot.title = element_text(size = 26, face = "bold"), axis.text = element_blank())
  locgraph
  
  ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts/Validation", "MSAMaps_Attendee_Locations_ICED2019.png"), width = 18, height = 9)
}
#####


### Air data setup
load(here("2_Data/Conference_Impacts", "Airports.Rda"))

# All airports and their coordinates
all_airports <- data.frame(unique(union(airports$origin_point, airports$destination_point)))
all_airports$lat <- as.numeric(gsub(",.*","",all_airports$unique.union.airports.origin_point..airports.destination_point..))
all_airports$long <- as.numeric(gsub("^.*\\,","",all_airports$unique.union.airports.origin_point..airports.destination_point..))
all_airports <- all_airports %>% select(long, lat) %>% filter(long >= -140) %>% distinct()
####################


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
  network <- airports[which(airports$quarter == as.yearqtr("2017 Q4")),c("fare", "origin_point", "destination_point", "traveltime")]
    # Use airport data from 2017 (not 2019?)
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


##### PART 2: Travel costs for each therapist + heat map for the ICED 2019 conference
# Salary information by provider type -- measured in 2018 USD (source: BLS National Occupational Employment and Wage Estimates)
# Link: https://www.bls.gov/bls/blswage.htm
salaries <- data.frame(provtype = c(20, 21, 22, 23, 25, 31, 35, 37, 40, 200, 206, 240, 365, 400, 458, 822, 824, 825, 845, 853, 860), 
                       label = c("MH Fac", "MH Fac", "SA Fac", "MH Day Care", "Rehab Fac", "Extended Care", "Res Tmt Center", 
                                 "Day/Night Care Center", "MH Fac", "MD (nec)", "Multispecialty Physician Group", "Family Practice", 
                                 "Psychiatry", "Pediatrician", "Child Psychiatry", "Nursing", "Psychiatric Nurse", "Nurse Practitioner", 
                                 "Physician Assistant", "Therapist", "Psychologist"), 
                       salary_hr = c(21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 21.46, 96.68, 96.68, 96.68, 
                                     105.95, 82, 105.95, 34.48, 65.40, 51.46, 52.22, 26.50, 37.99))

### Computes travel costs for each therapist and the 2019 ICED conference. 
conf_date <- as.Date("2019-03-14", format = "%Y-%m-%d") 
conf_loc <- c(-73.9836468, 40.7625228)
conf_q <- as.yearqtr(conf_date)

load(here("2_Data/Conference_Impacts", "Analysis_FBT.Rda"))
spec_locs <- mydata %>% group_by(PROVID) %>% summarize(provtype = first(STDPROV), 
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
save(spec_locs, file=here("2_Data/Conference_Impacts/Validation_Samples", "ICED2019_Estimated_TravelCosts.RData"))
  # So you don't have to run that again

### Heat Map of Predicted Attendees
# Without salary normalization
locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = travelcost), size = 3) +
  geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_c(direction = -1) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Specialist Travel Cost: Mar 2019 Conference in New York, NY') + scale_size(guide = F) + labs(x = "", y = "", color = "Travel Cost ($)") + 
  theme(plot.title = element_text(size = 16, face = "bold"))

# Without salary normalization
locgraph_normed <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = travelcost_normed), size = 3) +
  geom_point(aes(x = conf_loc[1], y = conf_loc[2]), size = 5, color = 'red') + scale_color_viridis_c(direction = -1) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Specialist Travel Cost: Mar 2019 Conference in New York, NY (Normalized)') + scale_size(guide = F) + labs(x = "", y = "", color = "Travel Cost (mult of salary)") + 
  theme(plot.title = element_text(size = 16, face = "bold"))

ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts/Validation", "PredictedTravelCosts_Unnormed_2019NYC.png"), width = 18, height = 9)
ggsave(locgraph_normed, file = here("4_Output/Figures/Conference_Impacts/Validation", "PredictedTravelCosts_Normed_2019NYC.png"), width = 18, height = 9)
####################


### Creating "correct" treatment group using 2019 ICED data and marketscan
# Identifying distance between MarketScan therapists and conference institutions
keep_therapist <- function(long, lat) {   
  # Identifies minimum distance (in miles) between a given therapist's location and any attendee organization
  z <- rep(NA, nrow(validate)) # Vector of all distances
  for (i in 1:nrow(validate)) {
    z[i] <- distHaversine(c(validate$Longitude[i], validate$Latitude[i]),c(long, lat), r=3960)
      # Earth radius in miles (hence distance in miles)
    }
  return(min(z))
}

spec_locs$keep <- apply(spec_locs, 1, function(x) keep_therapist(x[4], x[3]))
spec_locs$true_tmt <- ifelse(spec_locs$keep <= 10, 1, 0)
  # Assign true treatment status to all those within 25 miles of an attendee organization
save(spec_locs, file=here("2_Data/Conference_Impacts/Validation_Samples", "ICED2019_Estimated_TravelCosts.RData"))
  # So you don't have to run that again

if (want_graph) {
  # Graphing the "true" treatment group
  locgraph <- ggplot(st) + geom_sf(fill='white') + geom_jitter(data = spec_locs, aes(x=avg_long, y = avg_lat, color = as.factor(true_tmt)), size = 2.5) + 
    scale_color_manual(values = c("darkturquoise", "orangered2"), labels = c("No", "Yes")) + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Estimated 2019 ICED Treatment Status: MarketScan Therapists') + labs(x = "", y = "", color = "Within 10 miles of an attendee") + 
    theme(plot.title = element_text(size = 16, face = "bold"))
  ggsave(locgraph, file = here("4_Output/Figures/Conference_Impacts/Validation", "Estimated_ICED2019_Attendees.png"), width = 18, height = 9)
}
####################


### Comparing the true treatment to estimated treatment groups: un-normed
thresholds <- c(0.01, 0.02, 0.05, 0.10, 0.15, 0.85, 0.90, 0.95, 0.98, 0.99)
for (thresh in thresholds) {
  varname <- paste("est_treat", thresh, sep = "_")
  spec_locs[[varname]] <- ifelse(spec_locs$travelcost <= quantile(spec_locs$travelcost, thresh), 1, 0)
  
  varname2 <- paste("est_treat_normed", thresh, sep = "_")
  spec_locs[[varname2]] <- ifelse(spec_locs$travelcost_normed <= quantile(spec_locs$travelcost_normed, thresh), 1, 0)
}

# Correlations between each treshold and the "true" treatment
corrs <- rep(NA, length(thresholds))
for(i in 1:length(thresholds)) {
  varname <- paste("est_treat", thresholds[i], sep = "_")
  corrs[i] <- cor(spec_locs$true_tmt, spec_locs[[varname]])
}

corrs_normed <- rep(NA, length(thresholds))
for(i in 1:length(thresholds)) {
  varname <- paste("est_treat_normed", thresholds[i], sep = "_")
  corrs_normed[i] <- cor(spec_locs$true_tmt, spec_locs[[varname]])
}

# Probability of errors 1: lower thresholds (unnormed)
# For strict treatment assignment, need Pr(assign x to treatment | x is "truly" control)
thresh_l <- thresholds[1:(length(thresholds)/2)]
error_prob_lower_unnormed <- rep(NA, length(thresh_l))
for (i in 1:length(thresh_l)) {
  varname <- paste("est_treat", thresh_l[i], sep = "_")
  error_prob_lower_unnormed[i] <- mean(spec_locs[which(spec_locs$true_tmt == 0), ][[varname]])
}

# Probability of errors 2: upper thresholds (unnormed)
# For loose treatment assignment, need Pr(assign x to control | x is "truly" treatment)
thresh_u <- thresholds[(length(thresholds)/2+1):length(thresholds)]
error_prob_upper_unnormed <- rep(NA, length(thresh_u))
for (i in 1:length(thresh_u)) {
  varname <- paste("est_treat", thresh_u[i], sep = "_")
  spec_locs$mytest <- ifelse(spec_locs[[varname]] == 1, 0, 1)
  error_prob_upper_unnormed[i] <- mean(spec_locs[which(spec_locs$true_tmt == 1), ]$mytest)
}
spec_locs <- spec_locs %>% select(-mytest)

# Making these into a table
tabledata <- cbind(thresh_l, error_prob_lower_unnormed, rev(thresh_u), rev(error_prob_upper_unnormed))
kable(tabledata, "latex", booktabs = T)


# Probability of errors 3: lower thresholds (normed)
# For strict treatment assignment, need Pr(assign x to treatment | x is "truly" control)
error_prob_lower_normed <- rep(NA, length(thresh_l))
for (i in 1:length(thresh_l)) {
  varname <- paste("est_treat_normed", thresh_l[i], sep = "_")
  error_prob_lower_normed[i] <- mean(spec_locs[which(spec_locs$true_tmt == 0), ][[varname]])
}

# Probability of errors 4: upper thresholds (normed)
# For loose treatment assignment, need Pr(assign x to control | x is "truly" treatment)
error_prob_upper_normed <- rep(NA, length(thresh_u))
for (i in 1:length(thresh_u)) {
  varname <- paste("est_treat_normed", thresh_u[i], sep = "_")
  spec_locs$mytest <- ifelse(spec_locs[[varname]] == 1, 0, 1)
  error_prob_upper_normed[i] <- mean(spec_locs[which(spec_locs$true_tmt == 1), ]$mytest)
}
spec_locs <- spec_locs %>% select(-mytest)
####################


### Comparing the true treatment to estimated treatment groups: normed
tabledata <- cbind(thresh_l, error_prob_lower_normed, rev(thresh_u), rev(error_prob_upper_normed))
kable(tabledata, "latex", booktabs = T)
####################