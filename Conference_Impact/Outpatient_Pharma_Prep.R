##### Outpatient Pharma Prep -- last modified 6.24.19
#   Prepares outpatient pharmaceutical data for all AN patients treated by our specialists. ID's important drugs and graphs their use over time. 
#
#     NOTES: 
#         - Evaluating_Adopters.R needs to be run first to organize the specialist data
#
#     OUTPUT:
#         - ED_Outpatient_AllPharmacy.Rda saves the appropriate data file. 
#         - graphs of prescription shares over time (among all AN patients and among all receiving scrips) saved in "4_Output/Figures/Trends_in_PatientCount"
#           as "Pharma_Use_OverTime.png" (if wanted)
# 
#     MAJOR EDITS:  
#         - TBA: Make the graphs look nicer -- aggregate to quarter, smooth, etc. 
################################################################################

# Packages
library(readxl)
library(tidyverse)
library(tidylog)
library(zoo) # Useful for date formats
library(reshape2)
library(here)
library(stringr)
library(data.table)


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


##### 0. Setting up main data
if (file.exists(here("2_Data/Conference_Impacts", "OutpatientPharma.RData"))) {
  load(here("2_Data/Conference_Impacts", "OutpatientPharma.RData"))
} else {
  # Treatment data
  load(here("2_Data", "Specs_TherapyTreatments.Rda"))
  
  ### 0. Outpatient Pharmaceutical Data
  for (yr in 2007:2017) {
    nam <- paste("pharma", substr(yr,3,4), sep="") 
    filenam <- paste("OutPharma_AllPats_List_", yr, ".csv", sep="")
    assign(nam, read.csv(here("2_Data/Conference_Impacts", filenam)))
  }
  
  L <- list(pharma07, pharma08, pharma09, pharma10, pharma11, 
            pharma12, pharma13, pharma14, pharma15, pharma16, pharma17)
  vars_in_common <- Reduce(intersect, lapply(L, names))
  
  pharma07 <- pharma07 %>% select(vars_in_common)
  pharma08 <- pharma08 %>% select(vars_in_common)
  pharma09 <- pharma09 %>% select(vars_in_common)
  pharma10 <- pharma10 %>% select(vars_in_common)
  pharma11 <- pharma11 %>% select(vars_in_common)
  pharma12 <- pharma12 %>% select(vars_in_common)
  pharma13 <- pharma13 %>% select(vars_in_common)
  pharma14 <- pharma14 %>% select(vars_in_common)
  pharma15 <- pharma15 %>% select(vars_in_common)
  pharma16 <- pharma16 %>% select(vars_in_common)
  pharma17 <- pharma17 %>% select(vars_in_common)
  
  pharma <- rbind(pharma07, pharma08, pharma09, pharma10, pharma11, 
                  pharma12, pharma13, pharma14, pharma15, pharma16, pharma17)
  rm(L, vars_in_common, pharma07, pharma08, pharma09, pharma10, pharma11, 
     pharma12, pharma13, pharma14, pharma15, pharma16, pharma17, nam, yr, filenam)
  
  
  ### 1. Strip out all pharma claims that are outside the window of treatment for ED diagnosis
  mydata$dx_ed <- ifelse(substr(mydata$DX1, 1, 4) == "3071" | substr(mydata$DX1, 1, 4) == "3075" | substr(mydata$DX1, 1, 4) == "F5000" |
                                                    substr(mydata$DX1, 1, 4) == "F502" | substr(mydata$DX1, 1, 4) == "F508" | substr(mydata$DX1, 1, 4) == "F509" |
                                                    substr(mydata$DX2, 1, 4) == "3071" | substr(mydata$DX2, 1, 4) == "3075" | substr(mydata$DX2, 1, 4) == "F5000" |
                                                    substr(mydata$DX2, 1, 4) == "F502" | substr(mydata$DX2, 1, 4) == "F508" | substr(mydata$DX2, 1, 4) == "F509", 1, 0)
  mydata$dx_ed_under20 <- ifelse(mydata$dx_ed == 1 & mydata$AGE <= 20, 1, 0)
  mydata <- mydata %>% group_by(ENROLID, datemon) %>% mutate(dx_ed = max(dx_ed), dx_ed_under20 = max(dx_ed_under20))
  tokeep <- mydata %>% filter(dx_ed == 1) %>% group_by(ENROLID) %>% summarise(first_date = min(SVCDATE), last_date = max(SVCDATE))
  pharma <- left_join(pharma, tokeep, by = "ENROLID") %>% mutate(SVCDATE = as.Date(SVCDATE, format = "%m/%d/%Y")) %>%
    filter(SVCDATE >= first_date & SVCDATE <= last_date)
  rm(tokeep)
  ###
  
  ### Drug directories
  pharma[which(pharma$NDCNUM == 0), ]$NDCNUM <- NA
  pharma$ndc_formatted <- str_pad(pharma$NDCNUM, 11, pad = "0")
  pharma$pndc_formatted <- substr(pharma$ndc_formatted, 1, 9)
  ndc_dict <- read_excel(here("2_Data/Conference_Impacts/NDC_Database_FDA.xlsx")) %>% select(pndc_formatted, PROPRIETARYNAME, NONPROPRIETARYNAME) %>%
    group_by(pndc_formatted) %>% summarize(brand_name = first(PROPRIETARYNAME), gen_name = first(NONPROPRIETARYNAME))
  # Some duplicates in the dictionary to get rid of 
  pharma <- left_join(pharma, ndc_dict, by = "pndc_formatted", all.y = FALSE)
  
  ### 3 drugs of interest: Olanzapine, fluoxetine (BN), and Vyvanse
  wanted_drugs <- read_excel(here("2_Data/Conference_Impacts/Olanzapine_NDCs.xlsx")) 
  wanted_drugs$olanzapine <- ifelse(grepl("Olanzapine", wanted_drugs$Brand) | grepl("olanzapine", wanted_drugs$Brand) | grepl("OLANZAPINE", wanted_drugs$Brand), 1, 0)
  wanted_drugs$olanzapine <- ifelse(grepl("Olanzapine", wanted_drugs$Generic) | grepl("olanzapine", wanted_drugs$Generic) | grepl("OLANZAPINE", wanted_drugs$Generic), 1, wanted_drugs$olanzapine)
  wanted_drugs$vyvanse <- ifelse(grepl("Vyvanse", wanted_drugs$Brand) | grepl("vyvanse", wanted_drugs$Brand) | grepl("VYVANSE", wanted_drugs$Brand), 1, 0)
  wanted_drugs$vyvanse <- ifelse(grepl("Vyvanse", wanted_drugs$Generic) | grepl("vyvanse", wanted_drugs$Generic) | grepl("VYVANSE", wanted_drugs$Generic), 1, wanted_drugs$vyvanse)
  wanted_drugs$fluoxetine <- ifelse(grepl("Fluoxetine", wanted_drugs$Brand) | grepl("fluoxetine", wanted_drugs$Brand) | grepl("FLUOXETINE", wanted_drugs$Brand), 1, 0)
  wanted_drugs$fluoxetine <- ifelse(grepl("Fluoxetine", wanted_drugs$Generic) | grepl("fluoxetine", wanted_drugs$Generic) | grepl("FLUOXETINE", wanted_drugs$Generic), 1, wanted_drugs$fluoxetine)
  wanted_drugs <- wanted_drugs %>% select(pndc_formatted, olanzapine, vyvanse, fluoxetine)
  
  pharma <- left_join(pharma, wanted_drugs, by = "pndc_formatted")
  pharma[is.na(pharma$olanzapine), ]$olanzapine <- 0
  pharma[is.na(pharma$vyvanse), ]$vyvanse <- 0
  pharma[is.na(pharma$fluoxetine), ]$fluoxetine <- 0
  rm(ndc_dict, wanted_drugs)
  
  ### Some additional vars and saving the data
  pharma$datemon <- as.yearmon(pharma$SVCDATE)
  pharma$dateq <- as.yearqtr(pharma$SVCDATE)
  
  ##### Saving this data
  save(pharma, file=here("2_Data/Conference_Impacts/", "OutpatientPharma.RData"))
} 
####################


##### 1. Linking each patient's prescriptions to a provider/month --> assign each scrip fill to the last visit with a prescibing specialist for that ENROLID. 
# Bring in all providers that (i) meet with patient 3-6 months before fill and (ii) have the capacity to prescribe
load(here("2_Data/Conference_Impacts", "Analysis_FBT.Rda"))
tomerge <- mydata %>% group_by(ENROLID, PROVID, datemon) %>% summarize(spectype = mean(STDPROV), avg_lat = mean(avg_lat), avg_long = mean(avg_long)) %>%
  filter(spectype %in% c(200,206,240,365,400,458,824,825))
  # Filters out those who can't prescribe

working <- pharma %>% filter(REFILL == 0) %>% group_by(ENROLID, datemon) %>% summarize() %>% rename(pharma_date = datemon) # Each pharmaceutical recipient's ID/month

# Assign each scrip to the last prescribing physician that ENROLID saw (and calculate how long that's been)
merged <- left_join(tomerge, working, by = c("ENROLID"))
merged <- merged[!is.na(merged$pharma_date), ]
merged$date_diff <- 12*(as.numeric(merged$pharma_date) - as.numeric(merged$datemon))
final <- merged %>% filter(date_diff >= 0, date_diff <= 6) %>% group_by(ENROLID, pharma_date) %>% 
  summarize(PROVID_assigned = ifelse(length(PROVID[date_diff == min(date_diff)]) > 1, ifelse(length(PROVID[date_diff == min(date_diff) & spectype == 365]) > 1, NA, PROVID[date_diff == min(date_diff) & spectype == 365]), PROVID[date_diff == min(date_diff)]), 
            spectype_assigned = ifelse(length(PROVID[date_diff == min(date_diff)]) > 1, ifelse(length(PROVID[date_diff == min(date_diff) & spectype == 365]) > 1, NA, spectype[date_diff == min(date_diff) & spectype == 365]), spectype[date_diff == min(date_diff)]), 
            avg_lat = ifelse(length(PROVID[date_diff == min(date_diff)]) > 1, ifelse(length(PROVID[date_diff == min(date_diff) & spectype == 365]) > 1, NA, avg_lat[date_diff == min(date_diff) & spectype == 365]), avg_lat[date_diff == min(date_diff)]), 
            avg_long = ifelse(length(PROVID[date_diff == min(date_diff)]) > 1, ifelse(length(PROVID[date_diff == min(date_diff) & spectype == 365]) > 1, NA, avg_long[date_diff == min(date_diff) & spectype == 365]), avg_long[date_diff == min(date_diff)]), 
            datemon = ifelse(length(PROVID[date_diff == min(date_diff)]) > 1, ifelse(length(PROVID[date_diff == min(date_diff) & spectype == 365]) > 1, NA, datemon[date_diff == min(date_diff) & spectype == 365]), datemon[date_diff == min(date_diff)])) 
  # Favor psychiatrists prescribing if there is a conflict. Leaves about 2% of cases unresolved. 
final <- final %>% rename(PROVID = PROVID_assigned, spectype = spectype_assigned) %>% select(ENROLID, PROVID, datemon, pharma_date, everything())
final$datemon <- as.yearmon(final$datemon)
final <- final[!is.na(final$PROVID), ]
rm(working, tomerge, merged)

# Get information on (i) all scrips and (ii) olanzapine to merge in
tomerge <- pharma %>% filter(REFILL == 0) %>% select(ENROLID, datemon, ndc_formatted, brand_name, gen_name, olanzapine)
tomerge <- tomerge %>% group_by(ENROLID, datemon) %>% summarize(num_scrips = n_distinct(ndc_formatted),  
                                                                olanzapine = max(olanzapine), 
                                                                drug_names = list(brand_name)) %>% rename(pharma_date = datemon)
final <- left_join(final, tomerge, by = c("ENROLID", "pharma_date"))
save(final, file=here("2_Data/Conference_Impacts/", "OutpatientPharma_Linked.RData"))
####################


### 2. Lists for the drugs of potential interest
drugs_used <- pharma %>% group_by(pndc_formatted) %>% summarize(count = n(), 
                                                                brand_name = first(brand_name),
                                                                gen_name = first(gen_name), 
                                                                olanzapine = max(olanzapine), 
                                                                vyvanse = max(vyvanse), 
                                                                fluoxetine = max(fluoxetine)) # For reference
###


### 3. Graphs of usage for the three drugs -- ORIGINAL PRESCRIPTIONS ONLY
# Collapse data to patient level and then merge into original data (also collapsed at patient level)
tomerge <- pharma %>% filter(REFILL == 0) %>% # Only original prescriptions
  group_by(ENROLID, SVCDATE) %>% summarize(olanzapine = max(olanzapine), vyvanse = max(vyvanse), fluoxetine = max(fluoxetine))

# For specialist visits, pick only types that can prescribe drugs (filter out therapists and psychologists: 845, 853, 860)
graphdata <- mydata %>% filter(dx_an == 1) %>% filter(!(STDPROV %in% c(845,853,860))) %>% group_by(ENROLID, SVCDATE) %>% summarize() %>% drop_na(ENROLID)

# What SVCDATE in specialists is the closest (but prior to) the prescription fill date?
# Uses data.table
setDT(tomerge)
setDT(graphdata)
graphdata[, specdate:=SVCDATE]
setkey(tomerge, ENROLID, SVCDATE)
setkey(graphdata, ENROLID, SVCDATE)
tomerge <- graphdata[tomerge, roll=Inf]

tomerge <- tomerge %>% mutate(diff = as.numeric(tomerge$SVCDATE - tomerge$specdate)) %>% filter(diff <= 180 & diff >= -180) %>% select(-diff) %>%
  rename(pickupdate = SVCDATE) # Throw out scrips that are filled more than 6 months away from a specialist's visit

# Merging 
graphdata <- graphdata %>% select(-SVCDATE)
graphdata <- left_join(graphdata, tomerge, by = c("ENROLID", "specdate"))

# Fixing variables
graphdata$gotscrip <- ifelse(is.na(graphdata$olanzapine), 0, 1)
graphdata[is.na(graphdata$olanzapine), ]$olanzapine <- 0
graphdata[is.na(graphdata$vyvanse), ]$vyvanse <- 0
graphdata[is.na(graphdata$fluoxetine), ]$fluoxetine <- 0
graphdata$datemon <- as.yearmon(graphdata$specdate)

# Collapse to monthly and graph -- All Patients
graphdata_all <- graphdata %>% group_by(ENROLID, datemon) %>% summarize(gotscrip = max(gotscrip), 
                                                                        olanzapine = max(olanzapine), 
                                                                        vyvanse = max(vyvanse), 
                                                                        fluoxetine = max(fluoxetine)) %>% # Collapse to patient-month first
  group_by(datemon) %>% summarize(olanzapine = mean(olanzapine), 
                                  vyvanse = mean(vyvanse), 
                                  fluoxetine = mean(fluoxetine)) # Then collapse to % of patients by month
graphdata_all <- melt(graphdata_all, id.vars = 'datemon')

mygraph_pres <- ggplot(graphdata_all, aes(x=datemon,y=value,color=variable)) + geom_line(size = 0.8) + # geom_smooth() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients getting prescriptions", color="Drug") + ggtitle("Use of Key AN Drugs Over Time") + theme_minimal() + 
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"), 
        legend.title = element_text(size=16,face='bold'), 
        legend.text = element_text(size=14))

mygraph_paper <- ggplot(graphdata_all, aes(x=datemon,y=value,color=variable)) + geom_line(size=0.8) + # geom_smooth() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients getting prescriptions", color="Drug") + ggtitle("Use of Key AN Drugs Over Time") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"))

want_graphs <- T
if (want_graphs) {
  ggsave(mygraph_pres, file=here("4_Output/Figures/Trends_in_PatientCount", "PRESENTATION_Pharma_Use_OverTime.png"), width = 18, height = 9)
  ggsave(mygraph_paper, file=here("4_Output/Figures/Trends_in_PatientCount", "Pharma_Use_OverTime.png"), width = 18, height = 9)
}

# Collapse to monthly and graph -- Only those getting a scrip
graphdata_scrip <- graphdata %>% filter(gotscrip == 1) %>% group_by(ENROLID, datemon) %>% summarize(gotscrip = max(gotscrip), 
                                                                                                    olanzapine = max(olanzapine), 
                                                                                                    vyvanse = max(vyvanse), 
                                                                                                    fluoxetine = max(fluoxetine)) %>% # Collapse to patient-month first
  group_by(datemon) %>% summarize(olanzapine = mean(olanzapine), 
                                  vyvanse = mean(vyvanse), 
                                  fluoxetine = mean(fluoxetine))
graphdata_scrip <- melt(graphdata_scrip, id.vars = 'datemon')

mygraph_pres <- ggplot(graphdata_scrip, aes(x=datemon,y=value,color=variable)) + geom_line(size=0.8) + # geom_smooth() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients WITH SCRIPS getting drugs", color="Drug") + ggtitle("Use of Key AN Drugs Over Time") + theme_minimal() + 
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"), 
        legend.title = element_text(size=16,face='bold'), 
        legend.text = element_text(size=14))

mygraph_paper <- ggplot(graphdata_scrip, aes(x=datemon,y=value,color=variable)) + geom_line(size=0.8) + # geom_smooth() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients WITH SCRIPS getting drugs", color="Drug") + ggtitle("Use of Key AN Drugs Over Time") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"))

if (want_graphs) {
  ggsave(mygraph_pres, file=here("4_Output/Figures/Trends_in_PatientCount", "PRESENTATION_Pharma_Use_OverTime_ScripsOnly.png"), width = 18, height = 9)
  ggsave(mygraph_paper, file=here("4_Output/Figures/Trends_in_PatientCount", "Pharma_Use_OverTime_ScripsOnly.png"), width = 18, height = 9)
}
#################### 


##### Graph of only Vyvanse Use Post-FDA Approval
if (want_graphs) {
  load(here("2_Data", "ED_Outpatient_AllPharmacy.Rda"))
  load(here("2_Data", "Specs_TherapyTreatments.Rda"))
  
  test <- pharma %>% group_by(ENROLID, datemon) %>% summarize(vyvanse = max(vyvanse)) %>% group_by(datemon) %>% summarize(vyvanse = mean(vyvanse))
  mygraph_pres <- ggplot(test, aes(x=datemon,y=vyvanse)) + geom_line(size=0.8, color='purple') + geom_smooth() + 
    geom_vline(xintercept = 2015.00, linetype='dashed', color='red', size=0.8) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    labs(x="Month", y="% of patients WITH SCRIPS getting Vyvanse") + ggtitle("Use of Vyvanse") + theme_minimal() + 
    theme(plot.title = element_text(size = 16, face = "bold"), 
          axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"), 
          legend.title = element_text(size=16,face='bold'), 
          legend.text = element_text(size=14))
  ggsave(mygraph_pres, file=here("4_Output/Figures/Trends_in_PatientCount", "PRESENTATION_Vyvanse_FDAApproval.png"), width = 18, height = 9)
}
#################### 
