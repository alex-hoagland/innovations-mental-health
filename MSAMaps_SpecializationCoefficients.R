##### MSAMaps.R -- last updated 6.19.19
#   This chunk makes maps of specialist data, their specialization and adoption strategies, etc. 
#
#     NOTES: 
#         - Requires running Evaluating_Adopters.R to get Specs_TherapyTreatments.Rda
#         - Additionally, if you want extra map, need to save Model_MSAs as MSA_Spec_Coefs.Rda
#
#     OUTPUT: Figures are found in 4_Output/Figures/MSA_Maps
#         -  MSAMaps_Spec_Locations.png, map of therapist locations
#         -  MSAMaps_Spec_HHI.png, map of average therapist specialization (in MSA)
#         -  MSAMaps_Spec_Adoption_Ever.png: map of fraction of therapists who adopt FBT at all. 
#         -  MSAMaps_Spec_Adoption_Maudsley.png: map of fraction of therapists treating eligible patients who adopt FBT for AN. 
#         -  EXTRA: MSAMaps_Spec_Coefficients.png, map of MSA FE coefficients of therapist HHI/specialization regression  
# 
#     MAJOR EDITS:  
#         - TBA: 
################################################################################

### USER INPUT REQUIRED: 
want_graph <- T # Would you like to save the graphs?
spec_coefs <- F # Are you using the special MSA_Spec_Coefs.Rda data?
##########


### Packages
library(maps)
library(sf)
library(tigris)
library(tidyverse)
library(here)
# sf Data Options
options(tigris_class = "sf")
options(tigris_use_cache = T)


### Functions
Mode_allowNA <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


### Load data
load(here("2_Data", "Specs_TherapyTreatments.Rda"))
if (spec_coefs) {
  load(here("2_Data", "MSA_Spec_Coefs.Rda"))
}


### Map Data Setup
msa <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_cbsa") # MSAs
mdiv <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_metdiv") # Metro divisions
msa <- msa[, c("GEOID", "NAME", "NAMELSAD", "LSAD", "geometry")]
mdiv <- mdiv[, c("CBSAFP", "METDIVFP", "NAME", "NAMELSAD", "LSAD", "geometry")] %>% rename(GEOID = METDIVFP)
msa$CBSAFP <- msa$GEOID
mymap <- rbind(msa, mdiv)
rm(msa, mdiv)
mymap$state <- gsub('.*, ', '', mymap$NAME)
mymap <- mymap[!grepl("AK", mymap$state) & !grepl("HI", mymap$state) & !grepl("PR", mymap$state),] #  %>% 
  filter(LSAD != "M2") # Keep continental US and only MSAs
  # State map (to superimpose)
st <- read_sf(dsn = here("2_Data/MSA_ShapeFile"), layer = "tl_2017_us_state") # State Boundaries
st <- st[(st$STUSPS != "AK" & st$STUSPS != "HI" & st$STATEFP < 60), ] # Only continental US
  # Latitude and longitude for each region
mymap$centroids <- st_transform(mymap, 29101) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>% st_geometry()
mymap$longitude <- st_coordinates(mymap$centroids)[,1]
mymap$latitude <- st_coordinates(mymap$centroids)[,2]


### Merging in data for the main maps (locations and HHIs so far -- later add adoption status)
tomerge <- mydata %>% group_by(PROVID) %>% summarize(MSA1 = Mode_allowNA(spec_MSA1), 
                                                     MSA1_n = Mode_allowNA(spec_MSA1_name), 
                                                     MSA2 = Mode_allowNA(spec_MSA2), 
                                                     MSA2_n = Mode_allowNA(spec_MSA2_name), 
                                                     MSA3 = Mode_allowNA(spec_MSA3), 
                                                     MSA3_n = Mode_allowNA(spec_MSA3_name), 
                                                     totclaims = n(), 
                                                     share_ind = (sum(ind_therapy)/totclaims)^2, 
                                                     share_gp = (sum(gp_therapy)/totclaims)^2, 
                                                     share_fam = (sum(fam_therapy)/totclaims)^2, 
                                                     share_em = (sum(EM)/totclaims)^2, 
                                                     share_intake = (sum(intake)/totclaims)^2, 
                                                     share_consult = (sum(spec_consult)/totclaims)^2, 
                                                     share_hosp = (sum(hospitalizations)/totclaims)^2, 
                                                     share_pharma = (sum(pharma)/totclaims)^2, 
                                                     share_other = (sum(other)/totclaims)^2,
                                                     prov_hhi = share_ind + share_gp + share_fam + 
                                                       share_em + share_intake + share_consult + 
                                                       share_hosp + share_pharma + share_other, 
                                                     
                                                     adopt_fbt_ever = max(adopt_fbt_ever), 
                                                     adopt_fbt_an = max(adopt_fbt_an),
                                                     treat_an_elig = max(treat_an_elig),
                                                     adopt_fbt_anelig = max(adopt_fbt_anelig))



### Make and save main maps
# Map 0: Which locations are even covered in your MSA map?
msas <- union(unique(mydata$spec_MSA1), union(unique(mydata$spec_MSA2), unique(mydata$spec_MSA3)))
mymap$insample <- ifelse(mymap$GEOID %in% msas, 1, 0)
test <- ggplot(mymap) + geom_sf(fill='white') + geom_sf(data=st, fill='white') + geom_sf(data=mymap, aes(fill=insample)) +
  scale_fill_viridis_c(alpha = 0.7) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Where are therapists?')+labs(fill="In sample")

# Map 1: Therapist locations -- this has been moved to TravelCost_Constructor.R. 


# Map 2: Average specialist HHI by MSA
hhi <- tomerge %>% group_by(MSA1) %>% summarize(hhi = weighted.mean(prov_hhi, totclaims))
colnames(hhi) <- c("GEOID", "prov_hhi")
hhi$GEOID <- as.character(hhi$GEOID)
mymap <- left_join(mymap, hhi, by = "GEOID")
# mymap[is.na(mymap$prov_hhi), ]$prov_hhi <- 0
coefmap <- ggplot(mymap) + geom_sf(fill='white') + geom_sf(data=st, fill='white') + geom_sf(data=mymap, aes(fill=prov_hhi)) +
  scale_fill_gradientn(colors = terrain.colors(7)) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Average Therapist Treatment Specialization')+labs(fill="Avg. Provider HHI")
if (want_graph) {
  ggsave(coefmap, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Spec_HHI.png"), width = 9)
}


# Map 3: Fraction of FBT and FBT-AN adopters by MSA
adopters <- tomerge %>% group_by(MSA1) %>% summarize(tot_adopt_fbt_ever = sum(adopt_fbt_ever),
                                                     tot_adopt_fbt_an = sum(adopt_fbt_an), 
                                                     tot_treat_an_elig = sum(treat_an_elig), 
                                                     tot_adopt_fbt_anelig = sum(adopt_fbt_anelig),
                                                     frac_adopt_fbt_ever = tot_adopt_fbt_ever/n(), 
                                                     frac_adopt_fbt_an = tot_adopt_fbt_an/n(), 
                                                     frac_adopt_fbt_anelig = tot_adopt_fbt_anelig / tot_treat_an_elig)
adopters[is.na(adopters$frac_adopt_fbt_anelig), ]$frac_adopt_fbt_anelig <- 0
colnames(adopters)[1] <- c("GEOID")
adopters$GEOID <- as.character(adopters$GEOID)
mymap <- left_join(mymap, adopters)
adoptmap <- ggplot(mymap) + geom_sf(fill='white') + geom_sf(data=st, fill='white') + geom_sf(data=mymap, aes(fill=frac_adopt_fbt_ever)) +
  scale_fill_viridis_c(alpha = 0.7) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Therapist Utilization of FBT in ALL Treatments')+labs(fill="% of Providers Using")
adoptmap_an <- ggplot(mymap) + geom_sf(fill='white') + geom_sf(data=st, fill='white') + geom_sf(data=mymap, aes(fill=frac_adopt_fbt_anelig)) +
  scale_fill_viridis_c(alpha = 0.7) + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme_minimal()+ggtitle('Therapist Utilization of FBT in AN Treatments')+labs(fill="% of Providers Using")
if (want_graph) {
  ggsave(adoptmap, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Spec_Adoption_Ever.png"), width = 9)
  ggsave(adoptmap_an, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Spec_Adoption_Maudsley.png"), width = 9)
}


### Additional map of MSA FE Coefficients (if using special regression data)
if (spec_coefs) {
  coefs <- as.data.frame(model_MSAs$coefficients)
  coefs$MSA <- row.names(coefs)
  coefs <- coefs %>% filter(MSA != "share_fam") %>% mutate(MSA = substr(MSA, 17, 21))
  colnames(coefs) <- c("Specialization_Coefficient", "GEOID")
  coefs$GEOID <- as.character(coefs$GEOID)
  mymap <- left_join(mymap, coefs, by="GEOID")  
  
  # Coefficient Map
  tograph <- mymap[!is.na(mymap$Specialization_Coefficient), ]
  coefmap <- ggplot(tograph) + geom_sf(fill='white') + geom_sf(data=st, fill='white') + geom_sf(data=tograph, aes(fill=Specialization_Coefficient)) +
    scale_fill_gradient2() + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Specialization Coefficients by MSA')+labs(fill="Spec. Coeff.")
  
  # Saving
  if (want_graph) {
    ggsave(coefmap, file = here("4_Output/Figures/MSA_Maps", "MSAMaps_Spec_Coefficients.png"), width = 9)
  }
  file.remove(here("2_Data", "MSA_Spec_Coefs.Rda"))
}
##########
