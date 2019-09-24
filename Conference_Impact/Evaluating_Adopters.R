##### Evaluating_Adopters -- last modified 7.5.2019
#   This data prepares the treatment history data for all specialists in the sample, 
#   assigns them locations and decodes their treatment behaviors, then produces some
#   simple graphs on specialist behavior. 
#
#     NOTES: 
#         - Requires the alltmts data obtained by running the SAS program __ from MarketScan
#
#     OUTPUT: Figures are found in 4_Output/Figures/Trends_in_PatientCount
#         -  AllPats_SeekingSpecs_OverTime.png shows trends in claim volume over time (sharp discontinuity)
#         -  Therapist_TreatmentBehavior.png shows breakdown of claims into different treatment categories
#         -  Therapist_HHI_OverTime.png shows the degree of therapist specialization over time 
# 
#     MAJOR EDITS:  
#         - 7.5.19: Added table for fraction of claims dedicated to therapy for each provtype 
#                   to justify the inclusion of each specialist type in conference evaluations. 
#         - TBA: Map MSA FEs of model_MSAs in order to see which areas have greater/less specialization
################################################################################

# Packages
library(readxl)
library(xlsx)
library(tidyverse)
library(tidylog)
library(zoo) # Useful for date formats
library(reshape2)
library(here)


# Functions
Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}

Modes <- function(x) {
  ux <- unique(x[!is.na(x)])
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

##### Data setup -- Patient panel plus list of ALL therapists seen each quarter

if (!file.exists(here("2_Data", "Specs_AllTreatments.Rda"))) {
  vars_to_keep <- c("SVCDATE", "first_svc", "ENROLID", "PROVID", "STDPROV", "PLANTYP", "DX1", "DX2", "PROC1", "MSA", "MDC", "SEX", "AGE")
  alltmts <- read.csv("C:\\Users\\alcobe\\Downloads\\AllTmts_2007.csv")
  alltmts <- alltmts[, names(alltmts) %in% vars_to_keep]
  for (i in 2008:2017) {
    nam <- paste("C:\\Users\\alcobe\\Downloads\\AllTmts_", i, ".csv", sep = "")
    tomerge <- read.csv(nam)
    tomerge <- tomerge[, names(tomerge) %in% vars_to_keep]
    alltmts <- rbind(alltmts, tomerge)
  }
  rm(tomerge, i, nam)
  
  # Keeping only specialists
  specs <- c(20,21,22,23,25,30,31,35,37,40,200,206,240,365,400,458,822,824,825,845,853,860)
  alltmts <- alltmts[which(alltmts$STDPROV %in% specs), ] # Drops about 4 million claims (25% or so)
  
  # Summary -- how many specs are in the data?
  length(unique(alltmts$PROVID)) # We have 4,557 specialists here. 
  
  # Variable for provider location *in each month* (the mode MSA of their clientelle)
  # alltmts[which(alltmts$MSA == 0), ]$MSA <- NA # The 0 is a code for non-MSA, so drop if providers need to be kept in that area
  alltmts$SVCDATE <- as.Date(alltmts$SVCDATE, format = "%m/%d/%Y")
  alltmts$dateq <- as.yearqtr(alltmts$SVCDATE)
  alltmts$datemon <- as.yearmon(alltmts$SVCDATE)
  
  # Assigning MSAs to data -- this is done more precisely (incorporating all claims) in TravelCost_Constructor.R, so this is old code.
  #     # First pass -- if there are no ties, the mode MSA is assigned to the specialist for that month
  #     tomerge <- alltmts %>% group_by(PROVID, MSA, datemon) %>% drop_na(MSA) %>% filter(MSA != 0) %>% 
  #       summarize(numpats = n_distinct(ENROLID)) %>% group_by(PROVID, datemon) %>%
  #       filter(numpats == max(numpats)) %>% summarize(main_MSA = list(unique(MSA)), 
  #                                                     num_MSAs = length(unique(MSA)))
  #     
  #     # If there are ties, then the MSA at the last claimed day of the month is assigned to the specialist for that month
  #     test <- alltmts %>% group_by(PROVID, MSA, datemon) %>% drop_na(MSA) %>% filter(MSA != 0) %>% 
  #       summarize(numpats = n_distinct(ENROLID), mostrecent = max(SVCDATE))  %>%
  #       group_by(PROVID, datemon) %>% filter(numpats == max(numpats) & mostrecent == max(mostrecent)) %>% 
  #       summarize(recent_MSA = list(unique(MSA)), nrecent_MSA = length(unique(MSA)))
  #     tomerge <- left_join(tomerge, test, by = c("PROVID", "datemon"))
  #     rm(test)
  #     tomerge$main_MSA <- ifelse(tomerge$num_MSAs == 1, tomerge$main_MSA, tomerge$recent_MSA)
  #     tomerge$num_MSAs <- ifelse(tomerge$num_MSAs == 1, tomerge$num_MSAs, tomerge$nrecent_MSA)
  #     
  #     # If there are still ties, then the MSA is the one claimed the most on the last day of the month
  #     test <- alltmts %>% group_by(PROVID, datemon, MSA) %>% mutate(mostrecent = max(SVCDATE), 
  #                                                                   keep = ifelse(SVCDATE == mostrecent, 1, 0)) %>% select(-mostrecent)
  #     
  #     test <- test %>% filter(keep == 1) %>% group_by(PROVID, datemon, MSA) %>% drop_na(MSA) %>% filter(MSA != 0) %>% 
  #       summarize(numpats = n_distinct(ENROLID), mostrecent = max(SVCDATE)) %>%
  #       group_by(PROVID, datemon) %>% filter(numpats == max(numpats)) %>% summarize(recent2_MSA = list(unique(MSA)), 
  #                                                                                   nrecent2_MSA = length(unique(MSA)))
  #     
  #     tomerge <- left_join(tomerge, test, by = c("PROVID", "datemon"))
  #     rm(test)
  #     tomerge$main_MSA <- ifelse(tomerge$num_MSAs == 1, tomerge$main_MSA, tomerge$recent2_MSA)
  #     tomerge$num_MSAs <- ifelse(tomerge$num_MSAs == 1, tomerge$num_MSAs, tomerge$nrecent2_MSA)
  #     
  #     # We can handle some ties, but for now drop all those with 4+ MSAs still remaining
  #     tomerge$drop <- ifelse(tomerge$num_MSAs > 3, 1, 0)
  # 
  # # Merging this into the original data
  # tomerge <- tomerge[, names(tomerge) %in% c("PROVID", "datemon", "main_MSA", "drop")]
  # alltmts <- left_join(alltmts, tomerge, by = c("PROVID", "datemon")) %>% filter(drop != 1) %>% select(-drop)
  # rm(tomerge)
  # 
  # alltmts$spec_MSA1 <- as.numeric(ifelse(substr(alltmts$main_MSA, 1, 1) == "c", substr(alltmts$main_MSA, 3, 7), alltmts$main_MSA))
  # alltmts$spec_MSA2 <- as.numeric(ifelse(substr(alltmts$main_MSA, 1, 1) == "c", substr(alltmts$main_MSA, 10, 14), NA))
  # alltmts$spec_MSA3 <- as.numeric(ifelse(substr(alltmts$main_MSA, 1, 1) == "c", substr(alltmts$main_MSA, 17, 21), NA))
  # 
  # dict <- read.xlsx(here("2_Data/MSA_ShapeFile", "MSA_Dictionary.xlsx"), sheetIndex = 1) %>% select(1:2) # MSA dictionary
  # alltmts <- merge(alltmts, dict, all.x = T, by.x = 'spec_MSA1', by.y = 'MSA') %>% rename(spec_MSA1_name = MSA_name)
  # alltmts <- merge(alltmts, dict, all.x = T, by.x = 'spec_MSA2', by.y = 'MSA') %>% rename(spec_MSA2_name = MSA_name)
  # alltmts <- merge(alltmts, dict, all.x = T, by.x = 'spec_MSA3', by.y = 'MSA') %>% rename(spec_MSA3_name = MSA_name)
  # 
  # # Some of the MSAs weren't in the dictionary. Merge those in now
  # tomerge <- read_excel(here("2_Data/MSA_ShapeFile", "MSA_Dictionary.xlsx"), sheet = 2) %>% select(MSA_old, MSA_name)
  # 
  # alltmts <- merge(alltmts, tomerge, all.x = T, by.x = 'spec_MSA1', by.y = 'MSA_old') %>% rename(tomerge1 = MSA_name)
  # alltmts <- merge(alltmts, tomerge, all.x = T, by.x = 'spec_MSA2', by.y = 'MSA_old') %>% rename(tomerge2 = MSA_name)
  # alltmts <- merge(alltmts, tomerge, all.x = T, by.x = 'spec_MSA3', by.y = 'MSA_old') %>% rename(tomerge3 = MSA_name)
  # alltmts$spec_MSA1_name <- ifelse(is.na(alltmts$spec_MSA1_name), alltmts$tomerge1, as.character(alltmts$spec_MSA1_name))
  # alltmts$spec_MSA2_name <- ifelse(is.na(alltmts$spec_MSA2_name), alltmts$tomerge2, as.character(alltmts$spec_MSA2_name))
  # alltmts$spec_MSA3_name <- ifelse(is.na(alltmts$spec_MSA3_name), alltmts$tomerge3, as.character(alltmts$spec_MSA3_name))
  # alltmts <- alltmts %>% select(-c(tomerge1, tomerge2, tomerge3))
  # 
  # rm(dict, tomerge)
  
  # Note: right now, you have dropped all missing and non-MSAs, so nothing to worry about. 
  # Eventually, want to drop those with mode MSAs in crazy places (all across country). 
  
  # Save the data
  alltmts <- alltmts[order(alltmts$PROVID, alltmts$datemon), ] %>% select(PROVID, STDPROV, SVCDATE, dateq, datemon, ENROLID, 
                                                                          MSA, PLANTYP, SEX, AGE, MDC, DX1, DX2, PROC1, 
                                                                          SVCDATE, spec_MSA1, main_MSA, spec_MSA1_name, spec_MSA2, 
                                                                          spec_MSA2_name, spec_MSA3, spec_MSA3_name)
  save(alltmts, file=here("2_Data", "Specs_AllTreatments.Rda"))
} else if (!file.exists(here("2_Data", "Specs_TherapyTreatments.Rda"))) {
  # Organize therapy-only data 
  load(here("2_Data", "Specs_AllTreatments.Rda"))
  allprocs <- alltmts %>% group_by(PROC1) %>% summarize(allfreq = n()) %>% filter(PROC1 != "")
  procslist <- read.xlsx(file = here("2_Data", "AllProcs_EDOutpatient.xlsx"), sheetIndex = 1)
  colnames(procslist)[1] <- "PROC1"
  tomerge <- left_join(allprocs, procslist, by = "PROC1") %>% select(PROC1, Code.Description, Treatment.type, Therapy.treatment) %>% 
    rename(proc_desc = Code.Description, treat_type = Treatment.type, therapy = Therapy.treatment)
  tomerge[is.na(tomerge$therapy), ]$therapy <- 0
  tomerge[is.na(tomerge$treat_type), ]$treat_type <- 0
  mydata <- left_join(alltmts, tomerge, by = "PROC1") %>% filter(!is.na(PROVID))
  mydata <- mydata[which(mydata$therapy == 1), ] # Therapy treatments only. 
  rm(alltmts, tomerge, allprocs, procslist)
  
  tomerge <- mydata %>% group_by(PROC1) %>% summarize(desc = first(proc_desc))
  
  # Here, I'm dividing claims into categories -- E+M, Pharmacology, Therapy Types, etc. 
  # Categories: individual therapy, group therapy, family therapy, E+M, intake (diagnostics) specialist consultations, hospitalizations, pharma mgmt
  tomerge$ind_therapy <- ifelse(grepl("Individual", tomerge$desc) | grepl("individual", tomerge$desc) |
                                  grepl("ABT with modifications", tomerge$desc) | grepl("Exposure ABT", tomerge$desc) | 
                                  grepl("Nutrition therapy", tomerge$desc) | grepl("nutrition therapy", tomerge$desc) | grepl("Nutrition Therapy", tomerge$desc) | 
                                  grepl("cognitive skills development", tomerge$desc) | grepl("Applied Behavior Analysis", tomerge$desc) | 
                                  grepl("Intensive outpatient", tomerge$desc) | grepl("Psychosocial rehabilitation services", tomerge$desc) | 
                                  grepl("Activity therapy", tomerge$desc) | grepl("Hypnotherapy", tomerge$desc) | 
                                  grepl("Psychoanalysis", tomerge$desc) | grepl("Environmental manipualation", tomerge$desc) | 
                                  grepl("Electroconvulsive therapy", tomerge$desc) | grepl("Psychophysiological", tomerge$desc) | 
                                  grepl("substance abuse intervention", tomerge$desc) | grepl("Transcranial magentic stimulation", tomerge$desc) | 
                                  grepl("Therapeutic behavioral services", tomerge$desc) | grepl("Behavioral health counseling and therapy", tomerge$desc) | 
                                  grepl("Psychiatric health facility service", tomerge$desc) | grepl("mins therapy", tomerge$desc), 1, 0)
  tomerge$gp_therapy <- ifelse(grepl("Group", tomerge$desc) | grepl("group", tomerge$desc) |
                                 grepl("Community", tomerge$desc) | grepl("community", tomerge$desc) | 
                                 grepl("peer", tomerge$desc) | grepl("Peer", tomerge$desc), 1, 0)
  tomerge$fam_therapy <- ifelse(grepl("Family", tomerge$desc) | grepl("family", tomerge$desc), 1, 0)
  tomerge$EM <- ifelse(grepl("Evaluation", tomerge$desc) | grepl("eval", tomerge$desc) | 
                         grepl("E+M", tomerge$desc) | grepl("E/M", tomerge$desc) | grepl("E&M", tomerge$desc) | grepl("Patient History", tomerge$desc) | 
                         grepl("Case management", tomerge$desc) | grepl("case management", tomerge$desc) | grepl("Case Management", tomerge$desc) | 
                         grepl("Report", tomerge$desc) | grepl("service plan development", tomerge$desc) | grepl("and management", tomerge$desc) | 
                         grepl("rehabilitation goals", tomerge$desc), 1, 0)
  tomerge$intake <- ifelse(grepl("Diagnostic", tomerge$desc) | grepl("diagnostic", tomerge$desc) | 
                             grepl("Intake", tomerge$desc) | grepl("intake", tomerge$desc) | 
                             grepl("Assess", tomerge$desc) | grepl("assess", tomerge$desc) | 
                             grepl("tomergeing", tomerge$desc) | grepl("tomergeing", tomerge$desc) | 
                             grepl("Screening", tomerge$desc) | grepl("screening", tomerge$desc) | 
                             grepl("New patient", tomerge$desc) | grepl("new patient", tomerge$desc), 1, 0)
  tomerge$spec_consult <- ifelse(grepl("Telephone calls", tomerge$desc) | grepl("team conference", tomerge$desc) | 
                                   grepl("Coordinated care", tomerge$desc) | grepl("coordinated care", tomerge$desc) | 
                                   grepl("Interprofessional", tomerge$desc), 1, 0)
  tomerge$hospitalizations <- ifelse(grepl("Hospitalization", tomerge$desc) | grepl("hospitalization", tomerge$desc) | grepl("Hospital", tomerge$desc) | 
                                       grepl("Crisis", tomerge$desc) | grepl("crisis", tomerge$desc) | grepl("residential", tomerge$desc) |
                                       grepl("Inpatient hospital", tomerge$desc) | grepl("Hospital Care", tomerge$desc) | grepl("Observation care", tomerge$desc) | 
                                       grepl("Day Habilitation", tomerge$desc) | grepl("day treatment", tomerge$desc) | grepl("Day care", tomerge$desc) | 
                                       grepl("feeding tube", tomerge$desc) | grepl("HOSPITAL", tomerge$desc), 1, 0)
  tomerge$pharma <- ifelse(grepl("Pharmacological", tomerge$desc) | grepl("pharmacological", tomerge$desc) | 
                             grepl("Drug", tomerge$desc) | grepl("drug", tomerge$desc) | grepl("Medication", tomerge$desc) | grepl("feeding tube", tomerge$desc), 1, 0)
  tomerge$other <- ifelse(grepl("not otherwise specified", tomerge$desc) | grepl("Other psychiatric service", tomerge$desc) | 
                            grepl("prevention", tomerge$desc) | grepl("education", tomerge$desc) | grepl("attomerges", tomerge$desc) | 
                            grepl("Interactive complexity", tomerge$desc), 1, 0)
  # A manual correction
  tomerge[which(tomerge$PROC1 == 97804 | tomerge$PROC1 == "G0271"), ]$ind_therapy <- 0
  tomerge[which(tomerge$PROC1 == 90801 | tomerge$PROC1 == 90802), ]$EM <- 0
  tomerge[which(tomerge$PROC1 == "G0270" | tomerge$PROC1 == "G0271"), ]$intake <- 0
  tomerge[which(tomerge$PROC1 == "G0463"), ]$hospitalizations <- 0
  
  mydata <- left_join(mydata, tomerge, by = "PROC1") %>% select(-desc)
  save(mydata, file = here("2_Data", "Specs_TherapyTreatments.Rda"))
} else {
  load(here("2_Data", "Specs_TherapyTreatments.Rda"))
}
#####


##### Asking some basic questions about my therapists
# 1. What does their (therapy) treatment look like? 
# Graph over time -- total claims
graphdata <- mydata %>% group_by(datemon) %>% summarize(totclaims = n(), totpats = n_distinct(ENROLID))
graph <- ggplot(graphdata, aes(x=datemon, y=totpats)) + geom_line(size = 1, col = 'purple') + 
  geom_vline(xintercept = 2010.0, color = 'red', size=0.8, linetype = 'dotted') + 
  labs(x="Month", y="Number of Patients") + ggtitle("Number of Patients Seeking Specialist Treatment Over Time") + theme_minimal()
ggsave(graph, file = here("4_Output/Figures/Trends_in_PatientCount", "AllPats_SeekingSpecs_OverTime.png"), width = 9)
# This graph shows the weird drop in patient count around 2010. Probably a data issue. 

# Graph over time -- fraction of total claims to each group
graphdata <- mydata %>% group_by(datemon) %>% summarize(sum_ind = mean(ind_therapy), 
                                                        sum_gp = mean(gp_therapy), 
                                                        sum_fam = mean(fam_therapy), 
                                                        sum_em = mean(EM), 
                                                        sum_intake = mean(intake), 
                                                        sum_consult = mean(spec_consult), 
                                                        sum_hosp = mean(hospitalizations), 
                                                        sum_pharma = mean(pharma))
graphdata <- melt(graphdata, id = c("datemon"))

coul <-  brewer.pal(7, "Set2")
graph <- ggplot(graphdata, aes(x = datemon, y=value, color=variable)) + geom_line(size=1) + 
  labs(x="Quarter", y = "Number of Claims") + ggtitle("Therapist Treatment Behavior Over Time") + theme_minimal()
ggsave(graph, file = here("4_Output/Figures/Trends_in_PatientCount", "Therapist_TreatmentBehavior.png"), width = 9)

# TODO: Still need to check pharma codes just in case?
# I checked to see if I was missing any important codes -- looks like everything changes drastically. 
# Indicative that I have a weirdly different sample of patients between 2007-2009 and 2010 on.

# 2. What is the HHI of their treatment (over time) like? 
graphdata <- mydata %>% group_by(PROVID, datemon) %>% summarize(totclaims = n(), 
                                                              totpats = n_distinct(ENROLID),
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
                                                                share_hosp + share_pharma + share_other) %>%
  group_by(datemon) %>% summarize(mean = weighted.mean(prov_hhi, totpats))
graph <- ggplot(graphdata, aes(x=datemon, y=mean)) + geom_line(size=1, color='gold') + geom_smooth() + 
  labs(x='Month', y='Average Provider Treatment HHI') + ggtitle('Degree of provider specialization over time') + theme_minimal()
ggsave(graph, file = here("4_Output/Figures/Trends_in_PatientCount", "Therapist_HHI_OverTime.png"), width = 9)

# 3. What does this depend on? -- regress HHI_it on FEs for therapist, month/year, MSA, plantype, provider_type, HHI of DX's (patient variation)
### Overall notes:
# As expected, FBT has a negative coefficient -- the more you do FBT the less specialized you are. 
# Significant coefficients on provider types as well -- MH professionals are less specialized, pediatricians/nursing are more
# MSAs do seem to add explanatory power (but R-squared still about 1%)
# Patient characteristics add explanatory power as well -- TODO: add DX HHIs for real. 
# Overall, lots of variation within an individual over time -- therapists are responsive? 
###
regdata <- mydata %>% group_by(PROVID, datemon) %>% summarize(provtype = Mode(STDPROV), 
                                                              spec_msa = Mode(spec_MSA1),
                                                              totclaims = n(), 
                                                              totpats = n_distinct(ENROLID),
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
                                                                share_hosp + share_pharma + share_other)

library(plm)
regdata <- pdata.frame(regdata, index = c("PROVID", "datemon"))
regdata$spec_msa <- as.factor(regdata$spec_msa)

model_ind <- plm(prov_hhi ~ share_fam, data=regdata, model = 'within') 
  # Therapist FEs: R-squared is 0.002
model_time <- lm(prov_hhi ~ share_fam + factor(datemon), data=regdata) 
  # Month FEs: R-squared is 0.004
model_both <- plm(prov_hhi ~ share_fam, data=regdata, model = 'within', effect = 'twoways') 
  # Therapist + Month FEs: R-squared is 0.002
model_MSAs <- plm(prov_hhi ~ share_fam + factor(spec_msa), data=regdata, model = 'within', effect = 'twoways') 
  # Adding in MSA dummies: R-squared is 0.01

# Make a map of MSA coefficients
want_map <- F
if (want_map) {
  library(maps)
  library(tigris)
  
  # sf Data Options
  options(tigris_class = "sf")
  options(tigris_use_cache = T)
  mymap <- core_based_statistical_areas(cb=T)
  mymap$state <- gsub('.*, ', '', mymap$NAME)
  mymap <- mymap[!grepl("AK", mymap$state) & !grepl("HI", mymap$state) & !grepl("PR", mymap$state),]
  
  # Merging in Coefficients
  coefs <- as.data.frame(model_MSAs$coefficients)
  coefs$MSA <- row.names(coefs)
  coefs <- coefs %>% filter(MSA != "share_fam") %>% mutate(MSA = substr(MSA, 17, 21))
  colnames(coefs) <- c("Specialization_Coefficient", "GEOID")
  coefs$GEOID <- as.character(coefs$GEOID)
  mymap <- left_join(mymap, coefs, by="GEOID")
  
  # Coefficient Map
  tograph <- mymap[!is.na(mymap$Specialization_Coefficient), ]
  map_fbt <- ggplot(mymap) + geom_sf(fill='white') + geom_sf(data=tograph, aes(fill=freq)) + scale_fill_viridis_c(trans='sqrt', alpha=.7) + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme_minimal()+ggtitle('Specialization Coefficients by MSA')+labs(fill="Spec. Coeff.")
}

# Adding provider type dummies: 
spec_mhfac <- c(20, 21, 22, 23, 25, 31, 35, 37, 40)
spec_meddoc <- c(200, 206, 240)
spec_psychiatry <- c(365, 458, 824)
spec_pediatrician <- c(400, 458)
spec_nursing <- c(822, 824, 825, 845)
spec_therapists <- c(853)
spec_psychology <- c(860)

regdata$spec_mhfac <- ifelse(regdata$provtype %in% spec_mhfac, 1, 0)
regdata$spec_meddoc <- ifelse(regdata$provtype %in% spec_meddoc, 1, 0)
regdata$spec_psychiatry <- ifelse(regdata$provtype %in% spec_psychiatry, 1, 0)
regdata$spec_pediatrician <- ifelse(regdata$provtype %in% spec_pediatrician, 1, 0)
regdata$spec_nursing <- ifelse(regdata$provtype %in% spec_nursing, 1, 0)
regdata$spec_therapists <- ifelse(regdata$provtype %in% spec_therapists, 1, 0)
regdata$spec_psychology <- ifelse(regdata$provtype %in% spec_psychology, 1, 0)

model_provtypes <- plm(prov_hhi ~ share_fam + spec_mhfac + spec_meddoc + spec_psychiatry + 
                         spec_pediatrician + spec_nursing + spec_therapists + spec_psychology,
                       data=regdata, model = 'within', effect = 'twoways') # Adding in provider type dummies: R-squared is 0.006

# Adding in patient plan types and diagnoses HHI
mydata$ppo <- ifelse(mydata$PLANTYP == 6, 1, 0)
mydata$hmo <- ifelse(mydata$PLANTYP == 4, 1, 0)
mydata$hdhp <- ifelse(mydata$PLANTYP == 9, 1, 0)



tomerge <- mydata %>% group_by(PROVID, datemon) %>% summarize(num_dxs = length(unique(union(DX1, DX2))), 
                                                              frac_ppo = mean(ppo), 
                                                              frac_hmo = mean(hmo), 
                                                              frac_hdhp = mean(hdhp), 
                                                              ) # Could add DX HHI, but would take time

regdata <- left_join(regdata, tomerge, by = c("PROVID", "datemon"))
model_pattypes <- plm(prov_hhi ~ share_fam + num_dxs + frac_ppo + frac_hmo + frac_hdhp,
                      data=regdata, model = 'within', effect = 'twoways') # Adding in patient characteristics: R-squared is 0.02

# 4. Identify (i) adopters, and within that group, (ii) dedicated users and (iii) few-time experimenters
# What determines which of these categories you fall into? Similar regression to the above using (i) adoption and (ii) sticking_with_it as DVs. 

# Label all AN patients
mydata$dx_an <- ifelse(mydata$DX1 == "3071", 1, ifelse(substr(mydata$DX1, 1, 4) == "F500", 1, 0))
mydata$dx_an <- ifelse(mydata$DX2 == "3071", 1, ifelse(substr(mydata$DX2, 1, 4) == "F500", 1, mydata$dx_an)) 
mydata <- mydata %>% group_by(ENROLID, datemon) %>% mutate(dx_an = max(dx_an)) # Consistently label at the patient/month level

# Label all AN patients under 20
mydata$dx_an_under20 <- ifelse(mydata$dx_an == 1 & !is.na(mydata$AGE) & mydata$AGE < 20, 1, 0)

# Adoption dummies
mydata <- mydata %>% group_by(PROVID, datemon) %>% mutate(adopt_fbt_ever = ifelse(max(fam_therapy) == 1, 1, 0), 
                                                            # A. Adopting FBT at all
                                                          adopt_fbt_an = ifelse(max(fam_therapy * dx_an) == 1, 1, 0), 
                                                            # B. Adopting FBT for anorexia
                                                          treat_an_elig = ifelse(max(dx_an_under20 == 1), 1, 0), 
                                                            # C. Treating AN patients under 20 (eligible for FBT)
                                                          adopt_fbt_anelig = ifelse(max(fam_therapy * dx_an_under20) == 1, 1, 0))
                                                            # D. Giving FBT to the AN patients under 20

save(mydata, file = here("2_Data", "Specs_TherapyTreatments.Rda"))

# Want to know first month these were true for each therapist)
tomerge <- mydata %>% group_by(PROVID, adopt_fbt_ever == 1) %>% 
  arrange(PROVID) %>% mutate(count = row_number(), 
                             first_adopt_fbt_ever = adopt_fbt_ever & count == 1) %>% select(-count) # A. Adopting FBT at all
tomerge <- mydata %>% group_by(PROVID, adopt_fbt_an == 1) %>% 
  arrange(PROVID) %>% mutate(count = row_number(), 
                             first_adopt_fbt_an = adopt_fbt_an & count == 1) %>% select(-count) # B. Adopting FBT for anorexia
tomerge <- mydata %>% group_by(PROVID, treat_an_elig == 1) %>% 
  arrange(PROVID) %>% mutate(count = row_number(), 
                             first_treat_an_elig = treat_an_elig & count == 1) %>% select(-count) # C. Treating AN patients under 20 (eligible for FBT)
tomerge <- mydata %>% group_by(PROVID, adopt_fbt_anelig == 1) %>% 
  arrange(PROVID) %>% mutate(count = row_number(), 
                             first_adopt_fbt_anelig = adopt_fbt_anelig & count == 1) %>% select(-count) # D. Giving FBT to the AN patients under 20

# 5. For dedicated users, what fraction of their patients are getting FBT? What fraction of AN-FBT eligible patients are getting FBT? 
# Graph over time -- want to show that this is a dedicated portion of their clientelle. 


# 6. (Added 7.5.2019): Table showing frequency of (i) therapy treatments and (ii) ED diagnoses by total claim/patient count for each provider type
# This justifies the inclusion of each type of specialist in conference attending. 

# All treatments with procedure codes/labels
load(here("2_Data", "Specs_AllTreatments.Rda"))
allprocs <- alltmts %>% group_by(PROC1) %>% summarize(allfreq = n()) %>% filter(PROC1 != "")
procslist <- read.xlsx(file = here("2_Data", "AllProcs_EDOutpatient.xlsx"), sheetIndex = 1)
colnames(procslist)[1] <- "PROC1"
tomerge <- left_join(allprocs, procslist, by = "PROC1") %>% select(PROC1, Code.Description, Treatment.type, Therapy.treatment) %>% 
  rename(proc_desc = Code.Description, treat_type = Treatment.type, therapy = Therapy.treatment)
tomerge[is.na(tomerge$therapy), ]$therapy <- 0
tomerge[is.na(tomerge$treat_type), ]$treat_type <- 0
tabledata <- left_join(alltmts, tomerge, by = "PROC1") %>% filter(!is.na(PROVID))

# Keeping the providers used in TravelCost_Constructor.R (those with verifiable locations)
tabledata <- tabledata[which(tabledata$PROVID %in% speclist), ] # speclist <- sort(unique(mydata$PROVID)) after setting up mydata as in TravelCost_Constructor.R
                                                                # TODO: Clean this up more, lazy butt. 

# Collapsing by provtype -- keep mean of therapy, dx_an, and dx_ed
provtypes <- data.frame(STDPROV = c(20, 21, 22, 23, 25, 31, 35, 37, 40, 200, 206, 240, 365, 400, 458, 822, 824, 825, 845, 853, 860), 
                       label = c("MH Fac", "MH Fac", "SA Fac", "MH Day Care", "Rehab Fac", "Extended Care", "Res Tmt Center", 
                                 "Day/Night Care Center", "MH Fac", "MD (nec)", "Multispecialty Physician Group", "Family Practice", 
                                 "Psychiatry", "Pediatrician", "Child Psychiatry", "Nursing", "Psychiatric Nurse", "Nurse Practitioner", 
                                 "Physician Assistant", "Therapist", "Psychologist"))
tabledata <- left_join(tabledata, provtypes)

tabledata[is.na(tabledata$therapy),]$therapy <- 0
tabledata$dx_ed <- ifelse(substr(tabledata$DX1,1,4) == "3071" | substr(tabledata$DX1,1,4) == "3075" | substr(tabledata$DX1, 1, 3) == "F50", 1, 0) 
tabledata$dx_ed <- ifelse(substr(tabledata$DX2,1,4) == "3071" | substr(tabledata$DX2,1,4) == "3075" | substr(tabledata$DX2, 1, 3) == "F50", 1, tabledata$dx_ed) 
tabledata$dx_an <- ifelse(tabledata$DX1 == "3071", 1, ifelse(substr(tabledata$DX1, 1, 4) == "F500", 1, 0))
tabledata$dx_an <- ifelse(tabledata$DX2 == "3071", 1, ifelse(substr(tabledata$DX2, 1, 4) == "F500", 1, tabledata$dx_an))
mytable <- tabledata %>% group_by(label, ENROLID) %>% summarize(therapy = mean(therapy), dx_an = max(dx_an), dx_ed = max(dx_ed)) %>% 
  group_by(label) %>% summarize(therapy_mean = mean(therapy), dx_an_mean = mean(dx_an), dx_ed_mean = mean(dx_ed),
                                  therapy_se = sd(therapy)/sqrt(n()), dx_an_se = sd(dx_an)/sqrt(n()), dx_ed_se = sd(dx_ed)/sqrt(n()))

# Constructing the table
test <- mytable %>% select(label, dx_ed_mean, dx_ed_se) %>% gather(DX_ED, Value_ed, 2:3)
tomerge <- mytable %>% select(label, dx_an_mean, dx_an_se) %>% gather(DX_AN, Value_an, 2:3)
simple_table <- cbind(test, tomerge)
rm(test, tomerge)
simple_table <- simple_table[c(1,20,2,21,3,22,4,23,5,24,6,25,7,26,8,27,9,28,10,29,11,30,12,31,13,32,14,33,15,34,16,35,17,36,18,37,19,38),]
simple_table <- simple_table[, c(1,3,6)]
library(xtable)
print(xtable(simple_table,caption='Prevalence of ED/AN diagnoses among mental health patients by provider type', label='provtype_summary',digits=3), 
      file = here("4_output/Tables", "SummaryTable_ProviderTypes.txt"))
############################################################################################