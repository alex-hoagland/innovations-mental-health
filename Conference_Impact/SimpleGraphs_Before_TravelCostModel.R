##### SimpleGraphs_Before_TravelCostModel -- last modified 6.24.19
#   This constructs graphs of adoption and FBT use, as well as simple iterations of the DiD study before travel costs are incorporated
#
#     NOTES: 
#         - Evaluating_Adopters.R needs to be run first to organize the specialist data
#         - Outpatient_Pharma_Prep.R needs to be run first to organize the prescriptions data
#
#     OUTPUT PART 1 (Adoption and Use Graphts): Figures are found in 4_Output/Figures/Trends_in_PatientCount. 
#                                               There are paper and presentation versions of each. 
#         -  PatsGettingFBT_with_Publications.png shows the share of AN patients receiving FBT in time with important AN-FBT publications
#         -  SpecsFirstAdoptions_with_Publications.png shows the timeline of first adoptions of FBT in AN, again with publications overlaid
#         -  PatsGettingFBT_byPROVTYPE_with_Publications.png shows the same graph as the first, but divided by provider type
#
#     OUTPUT PART 2 (Simple DiD models):
# 
#     MAJOR EDITS:  
#         - TBA: 
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

# Data
load(here("2_Data", "Specs_TherapyTreatments.Rda"))


##### PART 1: Adoption and Use Graphs
want_adoption_graphs <- F
if (want_adoption_graphs) {
  ### 1. Graph of FBT over time (with important publications shown; move elsewhere eventually)
  # Get fraction of AN patients receiving FBT for each month
  mydata$dx_an <- ifelse(mydata$DX1 == "3071", 1, ifelse(substr(mydata$DX1, 1, 4) == "F500", 1, 0))
  mydata$dx_an <- ifelse(mydata$DX2 == "3071", 1, ifelse(substr(mydata$DX2, 1, 4) == "F500", 1, mydata$dx_an)) 
  mydata <- mydata %>% group_by(ENROLID) %>% mutate(dx_an = max(dx_an)) # Want DX at patient level, not claim
  
  graphdata <- mydata %>% filter(dx_an == 1) %>% group_by(datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>%
    group_by(datemon) %>% summarize(frac_gotFBT = mean(gotFBT))
  
  # These dates are for Lock et al, Hughes et al (2014), Couturier et al (2013), Eisler et al (2007), Rienecke
  pubs_online_dates <- data.frame(short_title = c("Comparing FBT to AFT", "Implementation of FBT", "Effect of FBT: A Systematic Review", "An RCT of two forms of FBT",
                                                  "FBT of ED: Current Insights", "Treatment manual for AN: FBT", "Early Response to FBT", "Understanding uptake of FBT"),
                                  first_authors = c("Lock", "Hughes", "Couturier", "Eisler", "Rienecke", "Lock & LeGrange", "Doyle", "Couturier, Kimber"), 
                                  time = c("Oct 2010", "Sep 2013", "Jul 2012", "Mar 2007", "Jun 2017", "Jun 2015", "Oct 2009", "Aug 2012"), 
                                  cites = c(629, 30, 231, 309, 12, 940, 123, 72), 
                                  cites_per_year = c(70, 5, 33, 26, 5, 209, 12, 10)) # First appearance online of important FBT in anorexia articles
  pubs_online_dates$time <- as.yearmon(pubs_online_dates$time)
  pubs_online_dates <- pubs_online_dates %>% mutate(cites_share = 7*cites_per_year/sum(cites_per_year))
  
  mygraph_pres <- ggplot(graphdata, aes(x=datemon, y=frac_gotFBT)) + geom_line(size=0.8, color='#A7E482') + 
    geom_vline(data = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10), ], aes(xintercept = time), 
               size = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10), ]$cites_share, linetype = 'dashed', color = '#2F6066') + 
    # Adding the dates for important publications as dashed red lines
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x="Month", y="% of AN patients receiving FBT") + ggtitle("FBT Use in Anorexia Over Time, Contrasted with Principal Publications") + 
    theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"), 
                            axis.text=element_text(size=14),
                            axis.title=element_text(size=16,face="bold"))
  
  mygraph_paper <- ggplot(graphdata, aes(x=datemon, y=frac_gotFBT)) + geom_smooth(size=2, color='#1FA49A') +
    geom_vline(data = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10), ], aes(xintercept = time), linetype = 'dashed', color = 'red', size=2) + 
    # Adding the dates for important publications as dashed red lines
    scale_y_continuous(limits = c(0, 0.20), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017)) +
    labs(x="Year", y="% of Anorexia Patients receiving FBT") + ggtitle("Use of Family-based Therapy Over Time, Contrasted with Principal Publications") + 
    theme_classic() + theme(plot.title = element_text(size = 26, face = "bold"), text=element_text(size=26), axis.title.y = element_text(margin = margin(r=10)))
  mygraph_paper
  
  ggsave(mygraph_paper, file = here("4_output/figures/Trends_in_PatientCount", "PatsGettingFBT_with_Publications.png"), width = 18, height = 9)
  ggsave(mygraph_pres, file = here("4_output/figures/Trends_in_PatientCount", "PRESENTATION_PatsGettingFBT_with_Publications.png"), width = 18, height = 9)
  
  ### 2. Graph of first FBT adoption for AN over time
  mydata$an_fbt_adopted <- ifelse(mydata$dx_an == 1 & mydata$fam_therapy == 1, 1, 0)
  graphdata <- mydata %>% filter(an_fbt_adopted == 1) %>% group_by(PROVID) %>% summarize(datemon = min(datemon)) %>%
    group_by(datemon) %>% summarize(num_starting = n())
  graphdata <- graphdata[which(graphdata$datemon > "Dec 2007"), ] # Ignore the first year because of potential data issues.
  
  # This graph looks better quarterly, so aggregating that way
  graphdata$q <- as.yearqtr(graphdata$datemon)
  test <- graphdata %>% group_by(q) %>% summarize(num_starting = sum(num_starting))
  pubs_online_dates$q <- as.yearqtr(pubs_online_dates$time)
  
  mygraph_pres <- ggplot(test, aes(x=q, y=num_starting)) + geom_line(size=0.8, color='blueviolet') + 
    geom_vline(data = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10 & pubs_online_dates$time > "Dec 2007"), ], aes(xintercept = q), 
               size = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10 & pubs_online_dates$time > "Dec 2007"), ]$cites_share, linetype = 'dashed', color = 'red') + 
    # Adding the dates for important publications as dashed red lines
    scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + 
    labs(x="Treatment Quarter", y="# of Specialists Using FBT in AN treatment for the first time") + ggtitle("First use of FBT in Anorexia, Contrasted with Principal Publications") + 
    theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"), 
                            axis.text=element_text(size=14),
                            axis.title=element_text(size=16,face="bold"))
  
  mygraph_paper <- ggplot(test, aes(x=q, y=num_starting)) + geom_line(size=0.8, color='chartreuse3') + 
    geom_vline(data = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10 & pubs_online_dates$time > "Dec 2007"), ], aes(xintercept = q), 
               size = pubs_online_dates[which(pubs_online_dates$cites_per_year > 10 & pubs_online_dates$time > "Dec 2007"), ]$cites_share, linetype = 'dashed', color = 'red') + 
    # Adding the dates for important publications as dashed red lines
    scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + 
    labs(x="Treatment Quarter", y="# of Specialists Using FBT in AN treatment for the first time") + ggtitle("First use of FBT in Anorexia, Contrasted with Principal Publications") + 
    theme_minimal() + theme(plot.title = element_text(size = 12, face = "bold"))
  
  ggsave(mygraph_paper, file = here("4_output/figures/Trends_in_PatientCount", "SpecsFirstAdoptions_with_Publications.png"), width = 18, height = 9)
  ggsave(mygraph_pres, file = here("4_output/figures/Trends_in_PatientCount", "PRESENTATION_SpecsFirstAdoptions_with_Publications.png"), width = 18, height = 9)
  
  # Then split the graph above by provider types. 
  spec_treatmentcenter <- c(20, 21, 22, 23, 25, 31, 35, 37, 40)
  spec_meddoc <- c(200, 206, 240, 400)
  spec_psychiatry <- c(365, 458, 824)
  spec_nursing <- c(822, 825, 845)
  spec_therapists <- c(853)
  spec_psychology <- c(860)
  
  mydata$spec_treatmentcenter <- ifelse(mydata$STDPROV %in% spec_treatmentcenter, 1, 0)
  mydata$spec_meddoc <- ifelse(mydata$STDPROV %in% spec_meddoc, 1, 0)
  mydata$spec_psychiatry <- ifelse(mydata$STDPROV %in% spec_psychiatry, 1, 0)
  mydata$spec_nursing <- ifelse(mydata$STDPROV %in% spec_nursing, 1, 0)
  mydata$spec_therapists <- ifelse(mydata$STDPROV %in% spec_therapists, 1, 0)
  mydata$spec_psychology <- ifelse(mydata$STDPROV %in% spec_psychology, 1, 0)
  mat <- mydata[, names(mydata) %in% c("spec_treatmentcenter", "spec_meddoc", "spec_psychology", "spec_psychiatry", "spec_nursing", "spec_therapists")]
  colnames(mat) <- substr(colnames(mat), 6, nchar(colnames(mat)))
  mydata$spectype <- factor(as.matrix(mat)%*%c(1:length(colnames(mat))), labels = colnames(mat))
  rm(mat)
  
  # Faceting the graph to clean it up
  graphdata <- mydata %>% filter(dx_an == 1) %>% group_by(datemon, ENROLID, spectype) %>% summarize(gotFBT = max(fam_therapy)) %>%
    group_by(datemon, spectype) %>% summarize(frac_gotFBT = mean(gotFBT))
  graphdata <- graphdata[!(graphdata$spectype %in% c('meddoc', 'nursing')), ]
  graphdata$q <- as.yearqtr(graphdata$datemon)
  graphdata <- graphdata %>% group_by(q, spectype) %>% summarise(frac_gotFBT = mean(frac_gotFBT))
  
  pubs_online_dates_gp <- pubs_online_dates[rep(seq_len(nrow(pubs_online_dates)), each=4), ]
  pubs_online_dates_gp$spectype <- rep(c('treatmentcenter', 'therapists', 'psychology', 'psychiatry'), nrow(pubs_online_dates))
  
  mygraph_pres <- ggplot(graphdata, aes(x=q, y=frac_gotFBT, color=spectype)) + geom_line(size=0.8) + facet_wrap(~spectype) + 
    geom_vline(data = pubs_online_dates_gp[which(pubs_online_dates_gp$cites_per_year > 10), ], aes(group = spectype, xintercept = as.numeric(q)), 
               size = pubs_online_dates_gp[which(pubs_online_dates_gp$cites_per_year > 10), ]$cites_share, linetype = 'dashed', color = 'red') + 
    # Adding the dates for important publications as dashed red lines
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + scale_color_brewer(palette = 'Spectral') + 
    labs(x="Treatment Quarter", y="% of AN patients receiving FBT") + ggtitle("FBT Use in Anorexia Over Time, Contrasted with Principal Publications") + 
    theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"), 
                            axis.text=element_text(size=14),
                            axis.title=element_text(size=16,face="bold")) 
  
  mygraph_paper <- ggplot(graphdata, aes(x=q, y=frac_gotFBT, color=spectype)) + geom_line(size=0.8) + facet_wrap(~spectype) + 
    geom_vline(data = pubs_online_dates_gp[which(pubs_online_dates_gp$cites_per_year > 10), ], aes(group = spectype, xintercept = as.numeric(q)), 
               size = pubs_online_dates_gp[which(pubs_online_dates_gp$cites_per_year > 10), ]$cites_share, linetype = 'dashed', color = 'red') + 
    # Adding the dates for important publications as dashed red lines
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + scale_color_brewer(palette = 'Spectral') + 
    labs(x="Treatment Quarter", y="% of AN patients receiving FBT") + ggtitle("FBT Use in Anorexia Over Time, Contrasted with Principal Publications") + 
    theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold')) 
  
  ggsave(mygraph_paper, file = here("4_output/figures/Trends_in_PatientCount", "PatsGettingFBT_byPROVTYPE_with_Publications.png"), width = 18, height = 9)
  ggsave(mygraph_pres, file = here("4_output/figures/Trends_in_PatientCount", "PRESENTATION_PatsGettingFBT_byPROVTYPE_with_Publications.png"), width = 18, height = 9)
  
}
####################


##### PART 2: Simple conference analysis
mydata$dx_an <- ifelse(mydata$DX1 == "3071", 1, ifelse(substr(mydata$DX1, 1, 4) == "F500", 1, 0))
mydata$dx_an <- ifelse(mydata$DX2 == "3071", 1, ifelse(substr(mydata$DX2, 1, 4) == "F500", 1, mydata$dx_an)) 
mydata <- mydata %>% group_by(ENROLID) %>% mutate(dx_an = max(dx_an)) # Want DX at patient level, not claim

# Is there a spike in use of FBT in an MSA following a conference there? 

### Case 1: Boston, MA -- September 28, 2012
# Keep those whose primary MSA in September 2012 is in Mass: 
treatment_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340)
# Ad-hoc control states: NY, San Diego area CA, and OR.
control_MSAs <- c(10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Filtering the data
mydata$tokeep <- ifelse(mydata$datemon == "Sep 2012" & mydata$spec_MSA1 %in% union(treatment_MSAs, control_MSAs), 1, 0) # Keep only therapists in these locs
graphdata <- mydata %>% group_by(PROVID) %>% filter(max(tokeep) == 1 & datemon >= "Mar 2012" & datemon <= "Mar 2013") # Keep a 12-month window around the conference


# Dummy variables for which of the groups you are (1 = MA, 2 = NY, 3 = CA, 4 = OR)
graphdata <- graphdata %>% group_by(PROVID) %>% mutate(mainloc = first(spec_MSA1[datemon == "Sep 2012"]), 
                                                       group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                      ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 2, 
                                                                             ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 3, 4))))

# Collapsing data: share of AN patients in each group getting FBT over time
graphdata_an <- graphdata %>% filter(dx_an == 1) %>% # Keep only AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_an$group <- factor(graphdata_an$group, labels = c("Treatment (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

graphdata_all <- graphdata %>% # Don't filter out non-AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_all$group <- factor(graphdata_all$group, labels = c("Treatment (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata_an, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = 2012.667, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients receiving FBT", color="Group") + ggtitle("Effect of 2012 Boston conference on FBT usage: AN Patients") + theme_minimal()

mygraph_all <- ggplot(graphdata_all, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
         geom_vline(xintercept = 2012.667, color='red', linetype='dashed', size=0.6) + 
         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
         labs(x="Month", y="% of ALL patients receiving FBT", color="Group") + ggtitle("Effect of 2012 Boston conference on FBT usage: ALL Patients") + theme_minimal()

ggsave(mygraph_all, file=here("4_Output/Figures/Conference_Impacts", "Boston2012_SimpleDesign_AllPats.png"), width=18, height = 9)
ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Boston2012_SimpleDesign_AN.png"), width=18, height = 9)
      
# Simple diff-in-diff regression using this data
# All patients
graphdata_all$treated <- ifelse(graphdata_all$group == "Treatment (MA)", 1, 0) 
graphdata_all$post <- ifelse(graphdata_all$datemon >= "Sep 2012", 1, 0)
graphdata_all$inter <- graphdata_all$treated * graphdata_all$post
diffindiff_all <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_all)  

# AN patients only
graphdata_an$treated <- ifelse(graphdata_an$group == "Treatment (MA)", 1, 0)
graphdata_an$post <- ifelse(graphdata_an$datemon >= "Sep 2012", 1, 0)
graphdata_an$inter <- graphdata_an$treated * graphdata_an$post
diffindiff_an <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_an) 


### Case 2: Las Vegas, NV -- June 16, 2016
# Keep those whose primary MSA in June 2016 is in NV: 
treatment_MSAs <- c(16180, 29820, 39900)
# Ad-hoc control states: Mass, NY, San Diego area CA, and OR.
control_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340, # MA
                  10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Filtering the data
mydata$tokeep <- ifelse(mydata$datemon == "Jun 2016" & mydata$spec_MSA1 %in% union(treatment_MSAs, control_MSAs), 1, 0) # Keep only therapists in these locs
graphdata <- mydata %>% group_by(PROVID) %>% filter(max(tokeep) == 1 & datemon >= "Dec 2015" & datemon <= "Dec 2016") # Keep a 12-month window around the conference


# Dummy variables for which of the groups you are (1 = NV, 2 = MA, 3 = NY, 4 = CA, 5 = OR)
graphdata <- graphdata %>% group_by(PROVID) %>% mutate(mainloc = first(spec_MSA1[datemon == "Jun 2016"]), 
                                                       group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                      ifelse(mainloc %in% c(12700, 14454, 15764, 44140, 38340, 49340), 2, 
                                                                      ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 
                                                                      ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 4, 5)))))

# Collapsing data: share of AN patients in each group getting FBT over time
graphdata_an <- graphdata %>% filter(dx_an == 1) %>% # Keep only AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_an$group <- factor(graphdata_an$group, labels = c("Treatment (NV)", "Control 4 (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

graphdata_all <- graphdata %>% # Don't filter out non-AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_all$group <- factor(graphdata_all$group, labels = c("Treatment (NV)", "Control 4 (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata_an, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = 2016.417, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients receiving FBT", color="Group") + ggtitle("Effect of 2016 Las Vegas conference on FBT usage: AN Patients") + theme_minimal()

mygraph_all <- ggplot(graphdata_all, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = 2016.417, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of ALL patients receiving FBT", color="Group") + ggtitle("Effect of 2016 Las Vegas conference on FBT usage: ALL Patients") + theme_minimal()

ggsave(mygraph_all, file=here("4_Output/Figures/Conference_Impacts", "Vegas2016_SimpleDesign_AllPats.png"), width=18, height = 9)
ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Vegas2016_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
# All patients
graphdata_all$treated <- ifelse(graphdata_all$group == "Treatment (NV)", 1, 0) 
graphdata_all$post <- ifelse(graphdata_all$datemon >= "Jun 2016", 1, 0)
graphdata_all$inter <- graphdata_all$treated * graphdata_all$post
diffindiff_all <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_all)  

# AN patients only
graphdata_an$treated <- ifelse(graphdata_an$group == "Treatment (NV)", 1, 0) 
graphdata_an$post <- ifelse(graphdata_an$datemon >= "Jun 2016", 1, 0)
graphdata_an$inter <- graphdata_an$treated * graphdata_an$post
diffindiff_an <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_an) 


### Case 3: A conference in an area with more AN-FBT anyway -- Chicago April 2010
confdate <- "Apr 2010"
window_lower <- "Oct 2009"
window_upper <- "Oct 2010"
confdate_no <- as.numeric(as.yearmon(confdate))
# Keep those whose primary MSA in April 2010 is in NV: 
treatment_MSAs <- c(14010, 16580, 16060, 19500, 16974, 20994, 19180, 28100, 29404, 37900, 40420, 44100)
# Ad-hoc control states: Mass, NY, San Diego area CA, and OR.
control_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340, # MA
                  10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Filtering the data
mydata$tokeep <- ifelse(mydata$datemon == confdate & mydata$spec_MSA1 %in% union(treatment_MSAs, control_MSAs), 1, 0) # Keep only therapists in these locs
graphdata <- mydata %>% group_by(PROVID) %>% filter(max(tokeep) == 1 & datemon >= window_lower & datemon <= window_upper) # Keep a 12-month window around the conference


# Dummy variables for which of the groups you are (1 = IL, 2 = MA, 3 = NY, 4 = CA, 5 = OR)
graphdata <- graphdata %>% group_by(PROVID) %>% mutate(mainloc = first(spec_MSA1[datemon == confdate]), 
                                                       group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                      ifelse(mainloc %in% c(12700, 14454, 15764, 44140, 38340, 49340), 2, 
                                                                             ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 
                                                                                    ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 4, 5)))))

# Collapsing data: share of AN patients in each group getting FBT over time
graphdata_an <- graphdata %>% filter(dx_an == 1) %>% # Keep only AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_an$group <- factor(graphdata_an$group, labels = c("Treatment (IL)", "Control 4 (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

graphdata_all <- graphdata %>% # Don't filter out non-AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_all$group <- factor(graphdata_all$group, labels = c("Treatment (IL)", "Control 4 (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata_an, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients receiving FBT", color="Group") + ggtitle("Effect of 2010 Chicago conference on FBT usage: AN Patients") + theme_minimal()

mygraph_all <- ggplot(graphdata_all, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of ALL patients receiving FBT", color="Group") + ggtitle("Effect of 2010 Chicago conference on FBT usage: ALL Patients") + theme_minimal()

ggsave(mygraph_all, file=here("4_Output/Figures/Conference_Impacts", "Chicago2010_SimpleDesign_AllPats.png"), width=18, height = 9)
ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Chicago2010_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
# All patients
graphdata_all$treated <- ifelse(graphdata_all$group == "Treatment (IL)", 1, 0) 
graphdata_all$post <- ifelse(graphdata_all$datemon >= confdate_no, 1, 0)
graphdata_all$inter <- graphdata_all$treated * graphdata_all$post
diffindiff_all <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_all)  

# AN patients only
graphdata_an$treated <- ifelse(graphdata_an$group == "Treatment (IL)", 1, 0)
graphdata_an$post <- ifelse(graphdata_an$datemon >= confdate_no, 1, 0)
graphdata_an$inter <- graphdata_an$treated * graphdata_an$post
diffindiff_an <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_an) 


### Case 4: San Diego
confdate <- "Feb 2014"
window_lower <- "Aug 2013"
window_upper <- "Aug 2014"
confdate_no <- as.numeric(as.yearmon(confdate))
# Keep those whose primary MSA in April 2010 is in NV: 
treatment_MSAs <- c(41740, 31084, 37100, 42200, 40140, 42020) # Southern CA
# Ad-hoc control states: Mass, NY, San Diego area CA, and OR.
control_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340, # MA
                  10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Filtering the data
mydata$tokeep <- ifelse(mydata$datemon == confdate & mydata$spec_MSA1 %in% union(treatment_MSAs, control_MSAs), 1, 0) # Keep only therapists in these locs
graphdata <- mydata %>% group_by(PROVID) %>% filter(max(tokeep) == 1 & datemon >= window_lower & datemon <= window_upper) # Keep a 12-month window around the conference


# Dummy variables for which of the groups you are (1 = IL, 2 = MA, 3 = NY, 4 = CA, 5 = OR)
graphdata <- graphdata %>% group_by(PROVID) %>% mutate(mainloc = first(spec_MSA1[datemon == confdate]), 
                                                       group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                      ifelse(mainloc %in% c(12700, 14454, 15764, 44140, 38340, 49340), 2, 
                                                                             ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 4))))


# Collapsing data: share of AN patients in each group getting FBT over time
graphdata_an <- graphdata %>% filter(dx_an == 1) %>% # Keep only AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_an$group <- factor(graphdata_an$group, labels = c("Treatment (CA)", "Control 2 (MA)", "Control 1 (NY)", "Control 3 (OR)"))

graphdata_all <- graphdata %>% # Don't filter out non-AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_all$group <- factor(graphdata_all$group, labels = c("Treatment (CA)", "Control 2 (MA)", "Control 1 (NY)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata_an, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients receiving FBT", color="Group") + ggtitle("Effect of 2014 SD conference on FBT usage: AN Patients") + theme_minimal()

mygraph_all <- ggplot(graphdata_all, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of ALL patients receiving FBT", color="Group") + ggtitle("Effect of 2014 SD conference on FBT usage: All Patients") + theme_minimal()

ggsave(mygraph_all, file=here("4_Output/Figures/Conference_Impacts", "SD2014_SimpleDesign_AllPats.png"), width=18, height = 9)
ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "SD2014_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
# All patients
graphdata_all$treated <- ifelse(graphdata_all$group == "Treatment (CA)", 1, 0) 
graphdata_all$post <- ifelse(graphdata_all$datemon >= confdate_no, 1, 0)
graphdata_all$inter <- graphdata_all$treated * graphdata_all$post
diffindiff_all <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_all)  

# AN patients only
graphdata_an$treated <- ifelse(graphdata_an$group == "Treatment (CA)", 1, 0) 
graphdata_an$post <- ifelse(graphdata_an$datemon >= confdate_no, 1, 0)
graphdata_an$inter <- graphdata_an$treated * graphdata_an$post
diffindiff_an <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_an)


### Case 5: April 2015 in Boston (to contrast with Olanzipane use in 7.2.)
confdate <- "Apr 2015"
window_lower <- "Oct 2014"
window_upper <- "Oct 2015"
confdate_no <- as.numeric(as.yearmon(confdate))
# Keep those whose primary MSA in April 2010 is in NV: 
treatment_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340) # MA
# Ad-hoc control states: Mass, NY, San Diego area CA, and OR.
control_MSAs <- c(41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Filtering the data
mydata$tokeep <- ifelse(mydata$datemon == confdate & mydata$spec_MSA1 %in% union(treatment_MSAs, control_MSAs), 1, 0) # Keep only therapists in these locs
graphdata <- mydata %>% group_by(PROVID) %>% filter(max(tokeep) == 1 & datemon >= window_lower & datemon <= window_upper) # Keep a 12-month window around the conference


# Dummy variables for which of the groups you are (1 = IL, 2 = MA, 3 = NY, 4 = CA, 5 = OR)
graphdata <- graphdata %>% group_by(PROVID) %>% mutate(mainloc = first(spec_MSA1[datemon == confdate]), 
                                                       group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                      ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 2, 
                                                                             ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 4))))


# Collapsing data: share of AN patients in each group getting FBT over time
graphdata_an <- graphdata %>% filter(dx_an == 1) %>% # Keep only AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_an$group <- factor(graphdata_an$group, labels = c("Treatment (MA)", "Control 2 (S. CA)", "Control 1 (NY)", "Control 3 (OR)"))

graphdata_all <- graphdata %>% # Don't filter out non-AN patients
  group_by(group, datemon, ENROLID) %>% summarize(gotFBT = max(fam_therapy)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_FBT = mean(gotFBT)) # Collapse to group-month level 
graphdata_all$group <- factor(graphdata_all$group, labels = c("Treatment (MA)", "Control 2 (S. CA)", "Control 1 (NY)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata_an, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of AN patients receiving FBT", color="Group") + ggtitle("Effect of 2015 Boston conference on FBT usage: AN Patients") + theme_minimal()

mygraph_all <- ggplot(graphdata_all, aes(x=datemon, y=frac_FBT, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="% of ALL patients receiving FBT", color="Group") + ggtitle("Effect of 2015 Boston conference on FBT usage: All Patients") + theme_minimal()

ggsave(mygraph_all, file=here("4_Output/Figures/Conference_Impacts", "Boston2015_SimpleDesign_AllPats.png"), width=18, height = 9)
ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Boston2015_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
# All patients
graphdata_all$treated <- ifelse(graphdata_all$group == "Treatment (MA)", 1, 0) 
graphdata_all$post <- ifelse(graphdata_all$datemon >= confdate_no, 1, 0)
graphdata_all$inter <- graphdata_all$treated * graphdata_all$post
diffindiff_all <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_all)  

# AN patients only
graphdata_an$treated <- ifelse(graphdata_an$group == "Treatment (MA)", 1, 0) 
graphdata_an$post <- ifelse(graphdata_an$datemon >= confdate_no, 1, 0)
graphdata_an$inter <- graphdata_an$treated * graphdata_an$post
diffindiff_an <- lm(frac_FBT ~ treated + post + inter + factor(group), data=graphdata_an)


### Case 6: A continuing education program (Oregon?)
# Need to find a good one of these. 


### Case 7: Non-FBT treatments (drugs? etc.)
load(here("2_Data", "ED_Outpatient_AllPharmacy.Rda"))

# 7.1: (Olanzapine 1): October 2015 in San Antonio
confdate <- "Oct 2015"
window_lower <- "Apr 2015"
window_upper <- "Apr 2016"
confdate_no <- as.numeric(as.yearmon(confdate))
# Keep those whose primary MSA in April 2015 is in Mass: 
treatment_MSAs <- c(10180, 11100, 15180, 12420, 13140, 21340, 19124, 23104, 26420, 31180, 
                    32580, 33260, 30980, 36220, 43300, 41660, 46340, 48660, 41700, 12420, 
                    28660, 47380, 47020, 17780, 18580, 29700)
# Ad-hoc control states: MA, NY, San Diego area CA, and OR.
control_MSAs <- c(# 12700, 14454, 15764, 44140, 38340, 49340, # MA
                  10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Collapse data to patient level and then merge into original data (also collapsed at patient level)
workingdata <- pharma %>% filter(REFILL == 0) %>% # Only original prescriptions
  filter(datemon >= window_lower & datemon <= window_upper) # Keep only claims in time window -- still need to filter out by location
workingdata <- workingdata %>% mutate(tokeep = ifelse(datemon == confdate & MSA %in% union(treatment_MSAs, control_MSAs), 1, 0)) %>%
  group_by(ENROLID) %>% filter(max(tokeep) == 1)

# Dummy variables for which of the groups you are (1 = MA, 2 = MA, 3 = NY, 4 = CA, 5 = OR)
# workingdata <- workingdata %>% group_by(ENROLID) %>% mutate(mainloc = first(MSA[datemon == confdate]), 
#                                                             group = ifelse(mainloc %in% treatment_MSAs, 1,
#                                                                      ifelse(mainloc %in% c(12700, 14454, 15764, 44140, 38340, 49340), 2, 
#                                                                      ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 
#                                                                      ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 4, 5)))))

workingdata <- workingdata %>% group_by(ENROLID) %>% mutate(mainloc = first(MSA[datemon == confdate]), 
                                                            group = ifelse(mainloc %in% treatment_MSAs, 1,
                                                                           #ifelse(mainloc %in% c(12700, 14454, 15764, 44140, 38340, 49340), 2, 
                                                                                  ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 3, 
                                                                                         ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 4, 5))))#)


# Collapsing data: share of AN patients in each group getting FBT over time
graphdata <- workingdata %>% 
  group_by(group, datemon, ENROLID) %>% summarize(got_olanz = max(olanzapine)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_olanz = mean(got_olanz)) # Collapse to group-month level 
# graphdata$group <- factor(graphdata$group, labels = c("Treatment (TX)", "Control 1 (MA)", "Control 2 (NY)", "Control 3 (Southern CA)", "Control 4 (OR)"))
graphdata$group <- factor(graphdata$group, labels = c("Treatment (TX)", "Control 2 (NY)", "Control 3 (Southern CA)", "Control 4 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata, aes(x=datemon, y=frac_olanz, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="Olanzipane as a % of First AN prescriptions", color="Group") + ggtitle("Effect of 2015 Texas conference on Olanzipane usage for AN Patients") + theme_minimal()

ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "SanAntonio2015_Olanzipane_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
graphdata$treated <- ifelse(graphdata$group == "Treatment (TX)", 1, 0) 
graphdata$post <- ifelse(graphdata$datemon >= confdate_no, 1, 0)
graphdata$inter <- graphdata$treated * graphdata$post
diffindiff_an <- lm(frac_olanz ~ treated + post + inter + factor(group), data=graphdata)

# 7.2: (Olanzapine 2): April 2015 in Boston
confdate <- "Apr 2015"
window_lower <- "Oct 2014"
window_upper <- "Oct 2015"
confdate_no <- as.numeric(as.yearmon(confdate))
# Keep those whose primary MSA in April 2015 is in Mass: 
treatment_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340)
# Ad-hoc control states: NY, San Diego area CA, and OR.
control_MSAs <- c(10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Collapse data to patient level and then merge into original data (also collapsed at patient level)
workingdata <- pharma %>% filter(REFILL == 0) %>% # Only original prescriptions
  filter(datemon >= window_lower & datemon <= window_upper) # Keep only claims in time window -- still need to filter out by location
# tomerge <- workingdata %>% group_by(ENROLID, SVCDATE) %>% summarize()

# For specialist visits, pick only types that can prescribe drugs (filter out therapists and psychologists: 845, 853, 860)
#graphdata <- mydata %>% filter(dx_an == 1) %>% filter(!(STDPROV %in% c(845,853,860))) %>% group_by(PROVID, ENROLID, SVCDATE) %>% 
 # summarize(spec_MSA1 = first(spec_MSA1), spec_MSA1_name = first(spec_MSA1_name), provtype = first(STDPROV)) %>% drop_na(ENROLID)

# What SVCDATE in specialists is the closest (but prior to) the prescription fill date?
# Uses data.table
# setDT(tomerge)
# setDT(graphdata)
# graphdata[, specdate:=SVCDATE]
# setkey(tomerge, ENROLID, SVCDATE)
# setkey(graphdata, ENROLID, SVCDATE)
# tomerge <- graphdata[tomerge, roll=Inf]
# 
# tomerge <- tomerge %>% mutate(diff = as.numeric(tomerge$SVCDATE - tomerge$specdate)) %>% filter(diff <= 180 & diff >= -180) %>% select(-diff) %>%
#   rename(pickupdate = SVCDATE) # Throw out scrips that are filled more than 6 months away from a specialist's visit

# Merging & Filtering on Location
# tomerge$SVCDATE <- tomerge$pickupdate
# workingdata <- left_join(workingdata, tomerge, by = c("ENROLID", "SVCDATE"))

# Before you filter out based on provider IDs, just filter on patient location and see what happens
workingdata <- workingdata %>% mutate(tokeep = ifelse(datemon == confdate & MSA %in% union(treatment_MSAs, control_MSAs), 1, 0)) %>%
  group_by(ENROLID) %>% filter(max(tokeep) == 1)

# Dummy variables for which of the groups you are (1 = MA, 2 = NY, 3 = CA, 4 = OR)
workingdata <- workingdata %>% group_by(ENROLID) %>% mutate(mainloc = first(MSA[datemon == confdate]), 
                                                           group = ifelse(mainloc %in% treatment_MSAs, 1, 
                                                                          ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 2, 
                                                                                 ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 3, 4))))


# Collapsing data: share of AN patients in each group getting FBT over time
graphdata <- workingdata %>% 
  group_by(group, datemon, ENROLID) %>% summarize(got_olanz = max(olanzapine)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_olanz = mean(got_olanz)) # Collapse to group-month level 
graphdata$group <- factor(graphdata$group, labels = c("Treatment (MA)", "Control 1 (NY)", "Control 2 (Southern CA)", "Control 3 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata, aes(x=datemon, y=frac_olanz, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="Olanzipane as a % of First AN prescriptions", color="Group") + ggtitle("Effect of 2015 Boston conference on Olanzipane usage for AN Patients") + theme_minimal()

ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Boston2015_Olanzipane_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
graphdata$treated <- ifelse(graphdata$group == "Treatment (MA)", 1, 0) 
graphdata$post <- ifelse(graphdata$datemon >= confdate_no, 1, 0)
graphdata$inter <- graphdata$treated * graphdata$post
diffindiff_an <- lm(frac_olanz ~ treated + post + inter + factor(group), data=graphdata)

# 7.3: (Olanzapine 3): September 2012 in Boston
confdate <- "Sep 2012"
window_lower <- "Mar 2012"
window_upper <- "Mar 2013"
confdate_no <- as.numeric(as.yearmon(confdate))
# Treatment group: MA
treatment_MSAs <- c(12700, 14454, 15764, 44140, 38340, 49340)
# Ad-hoc control states: NY, San Diego area CA, and OR.
control_MSAs <- c(10580, 15380, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540, # NY
                  41740, 31084, 37100, 42200, 40140, 42020, # Southern CA
                  13460, 10540, 18700, 21660, 24420, 32780, 38900, 41420) # Oregon

# Collapse data to patient level and then merge into original data (also collapsed at patient level)
workingdata <- pharma %>% filter(REFILL == 0) %>% # Only original prescriptions
  filter(datemon >= window_lower & datemon <= window_upper) # Keep only claims in time window -- still need to filter out by location
workingdata <- workingdata %>% mutate(tokeep = ifelse(datemon == confdate & MSA %in% union(treatment_MSAs, control_MSAs), 1, 0)) %>%
  group_by(ENROLID) %>% filter(max(tokeep) == 1)

# Dummy variables for which of the groups you are (1 = MA, 2 NY, 3 = CA, 4 = OR)
workingdata <- workingdata %>% group_by(ENROLID) %>% mutate(mainloc = first(MSA[datemon == confdate]), 
                                                            group = ifelse(mainloc %in% treatment_MSAs, 1,
                                                                           ifelse(mainloc %in% c(15380, 10580, 20524, 21300, 24020, 27060, 28740, 35004, 35614, 40380, 45060, 48060, 46540), 2, 
                                                                                  ifelse(mainloc %in% c(41740, 31084, 37100, 42200, 40140, 42020), 3, 4))))


# Collapsing data: share of AN patients in each group getting FBT over time
graphdata <- workingdata %>% 
  group_by(group, datemon, ENROLID) %>% summarize(got_olanz = max(olanzapine)) %>% # Count each patient as 0/1 for FBT
  group_by(group, datemon) %>% summarize(frac_olanz = mean(got_olanz)) # Collapse to group-month level 
graphdata$group <- factor(graphdata$group, labels = c("Treatment (MA)", "Control 2 (NY)", "Control 3 (Southern CA)", "Control 4 (OR)"))

# Plot shares of AN patients getting FBT over time in each area. 
mygraph_an <- ggplot(graphdata, aes(x=datemon, y=frac_olanz, color=group)) + geom_line(size=0.8) + 
  geom_vline(xintercept = confdate_no, color='red', linetype='dashed', size=0.6) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Month", y="Olanzipane as a % of First AN prescriptions", color="Group") + ggtitle("Effect of 2012 Boston conference on Olanzipane usage for AN Patients") + theme_minimal()

ggsave(mygraph_an, file=here("4_Output/Figures/Conference_Impacts", "Boston2012_Olanzipane_SimpleDesign_AN.png"), width=18, height = 9)

# Simple diff-in-diff regression using this data
graphdata$treated <- ifelse(graphdata$group == "Treatment (MA)", 1, 0) 
graphdata$post <- ifelse(graphdata$datemon >= confdate_no, 1, 0)
graphdata$inter <- graphdata$treated * graphdata$post
diffindiff_an <- lm(frac_olanz ~ treated + post + inter + factor(group), data=graphdata)

####################