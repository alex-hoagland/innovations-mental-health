##### Instrumented_EventStudy -- last modified 8.13.2019
#   Evaluates the MR-LATE estimator for a generalized DID (DDIV) using various instruments
#
#     NOTES: 
#       -- Multiple treatment bands should have been estimated previously using TravelCost_Constructor.R. 
#
#     The instruments currently evaluated are: 
#       -- 1. Relative patient volume around conference registration
#       -- 2. Conference reach
#       -- 3. Tourism flows?
#
#     OUTPUT: 
#       -- Graph with corrected event study (4 -- 2x2 for FBT/drugs and homogeneous/specialist type heterogeneity)
# 
#     MAJOR EDITS:  
#       -- TBA: Everything above
#       -- TBA: Incorporate code from TravelCost_Constructor.R in order to clean up distinction between two files. 
#       -- TBA: To avoid negative weighting (as in Borusyak & Jaravel 2017), use as many post-treatment periods as possible.
################################################################################

### USER-SPECIFIED VARIABLES
want_graphs <- T
want_tables <- F
instrument <- 1 # Number based on list above. 
thresholds <- c(.01, .99) # What are the two mis-measured treatment definitions you are using?
  # Want to use the following pairs: 1/99, 2/98, 5/95, 10/90, 15/85. 
  # Make sure the smaller number always goes first. 
normed <- F # Do you want to do this with normed or unnormed travel costs?

n_bootstrap <- 1000 # How many times would you like to bootstrap?
####################


# Packages
library(readxl)
library(xlsx)
library(tidyverse)
library(tidylog)
library(zoo) # Useful for date formats
library(reshape2)
library(here)
library(fastDummies)


### 0. Data setup
# Data needed
load(here("2_Data/Conference_Impacts", "Analysis.Rda"))

# # Using full ED diagnoses, not just AN
# mydata$dx_ed <- ifelse(substr(mydata$DX1, 1, 4) == "3071" | substr(mydata$DX1, 1, 4) == "3075" | substr(mydata$DX1, 1, 4) == "F5000" | 
#                          substr(mydata$DX1, 1, 4) == "F502" | substr(mydata$DX1, 1, 4) == "F508" | substr(mydata$DX1, 1, 4) == "F509" | 
#                          substr(mydata$DX2, 1, 4) == "3071" | substr(mydata$DX2, 1, 4) == "3075" | substr(mydata$DX2, 1, 4) == "F5000" | 
#                          substr(mydata$DX2, 1, 4) == "F502" | substr(mydata$DX2, 1, 4) == "F508" | substr(mydata$DX2, 1, 4) == "F509", 1, 0)
# mydata$dx_ed_under20 <- ifelse(mydata$dx_ed == 1 & mydata$AGE <= 20, 1, 0)
# mydata <- mydata %>% group_by(ENROLID, datemon) %>% mutate(dx_ed = max(dx_ed), dx_ed_under20 = max(dx_ed_under20))
# save(mydata, file = here("2_Data/Conference_Impacts", "Analysis.Rda"))

# Dropping some variables we don't need for speed
if (normed) {
  nam1 <- paste("treated_normed_", thresholds[1], sep = "")
  nam2 <- paste("treated_normed_", thresholds[2], sep = "")
  date1 <- paste("tmtdate_normed_", thresholds[1], sep = "")
  date2 <- paste("tmtdate_normed_", thresholds[2], sep = "")
} else {
  nam1 <- paste("treated_", thresholds[1], sep = "")
  nam2 <- paste("treated_", thresholds[2], sep = "")
  date1 <- paste("tmtdate_", thresholds[1], sep = "")
  date2 <- paste("tmtdate_", thresholds[2], sep = "")
}
colnames <- sort(as.vector(mydata %>% ungroup %>% select(starts_with("treated_"), starts_with("tmtdate_")) %>% colnames())) 
colnames <- setdiff(colnames, c(nam1, nam2, date1, date2))
mydata <- mydata[, !(names(mydata) %in% colnames)]
  # Keep only the treatment variables pertaining to the two thresholds chosen above
####################


### 1. Instrument Creation
if (instrument == 1) {
  print('Using instrument 1: Relative Patient Volume.')
  
  # Each provider's mean patient volume (ignores 0 months)
  test <- mydata %>% group_by(PROVID, datemon) %>% summarise(patvol = n_distinct(ENROLID)) %>% group_by(PROVID) %>% summarize(avg_patvol = mean(patvol))
  mydata <- left_join(mydata, test, by = "PROVID")
  rm(test)
  
  # For each period, create two vars: (i) continuous var of (avg vol from 4-6 months prior)/(overall average volume) and (ii) binned version of 1
  test <- mydata %>% group_by(PROVID, datemon) %>% summarise(mon_patvol = n_distinct(ENROLID)) %>% slice(rep(1:n(), each = 3)) 
    # Repeat each row 3 times so that it feeds into 3 different months (4, 5, and 6 months later)
  test$datemon <- as.yearmon(ifelse(as.numeric(rownames(test)) %% 3 == 0, test$datemon + 0.3334, 
                             ifelse(as.numeric(rownames(test)) %% 3 == 1, test$datemon + 0.4167, 
                             test$datemon + 0.5)))
  test <- test %>% filter(datemon < 2018) %>% group_by(PROVID, datemon) %>% summarize(mon_patvol = mean(mon_patvol))
  mydata <- left_join(mydata, test, by = c("PROVID", "datemon"))
  mydata$inst <- mydata$mon_patvol / mydata$avg_patvol
  # mydata$inst_binned <- ifelse(mydata$inst < 0.5, 1, 
  #                              ifelse(mydata$inst >= 0.5 & mydata$inst < 1, 2, 
  #                                     ifelse(mydata$inst >= 1 & mydata$inst < 1.5, 3, 
  #                                            ifelse(mydata$inst >= 1.5 & mydata$inst < 2, 4, 5)))) # May want to update/delete this later.
  mydata$inst_binary <- ifelse(mydata$inst < 1, 1, 0)
  
  # Drop the first three months for each therapist (removes 4%)
  mydata$mon_num <- as.numeric(mydata$datemon)
  working <- mydata %>% group_by(PROVID) %>% mutate(start_mon = min(mon_num)+0.25) %>% filter(mon_num > start_mon)
  working <- working %>% group_by(PROVID) %>% mutate(leavesample = max(is.na(mon_patvol))) %>% filter(leavesample == 0)
  
  
    # Still about 150k obs from therapists who leave/re-enter sample. Drop those providers entirely. (removes 40%, leaves about 1400 providers)
    # You may not want this later on? 
  
  
  allprovs <- sort(unique(working$PROVID)) # List of PROVID's to keep in analysis later. 
  
  # Transforming the instrument for event studies -- defining the instrumented event as the period with lowest lagged volume. 
  test <- working %>% group_by(PROVID) %>% filter(inst == min(inst)) %>% summarize(inst_event = first(datemon))
  working <- left_join(working, test, by = "PROVID")
  working$inst_reltime <- working$datemon - working$inst_event
  
  # Check correlations in relative time:
  # working$test <- working$datemon - as.yearmon(working$tmtdate_0.02)
  # working$test1 <- working$datemon - as.yearmon(working$tmtdate_0.98)
  # cor(working[!is.na(working$test),]$test, working[!is.na(working$test),]$inst_reltime) # Correlation about 0.65.
  # cor(working[!is.na(working$test1),]$test1, working[!is.na(working$test1),]$inst_reltime) # Correlation about 0.65. 
  
  rm(test)
  
} else {
  print('ERROR: Input a correct instrument code.')
}
# Note: Can't check first stage because true treatment is unobserved. 
####################


### 2. Constructing the Event Study Datasets (One for each mismeasured treatment)
es_data_setup <- function(data, want_graph = T, FBTuse_dummy = T,
                            youth = F, bins_to_keep = 6) {
  # data: full analysis data -- should have a dummy variable for treatment status and a tmtdate variable
  # normed: Do you want to use the groups implied by travel costs that are salary-normalized? 
  # want_graph: Do you want to save the graph? 
  # FBTuse_dummy: Do you want the main DV to be a dummy for FBT use in ED, or the fraction of ED patients getting FBT? 
  # youth: Do you want to examine treatment of AN patients under 20? (default is F)
  # bins_to_keep: Periods before/after conference to use in dummy variables
  
  # Filter only ED patients, construct relevant outcome variable and treatment status/date
  if (FBTuse_dummy) {
      if (youth) {
        regdata <- data %>% filter(dx_ed_under20 == 1) %>% 
          group_by(PROVID, datemon) %>% summarize(usedFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                  inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime))
      } else {
        regdata <- data %>% filter(dx_ed == 1) %>% 
          group_by(PROVID, datemon) %>% summarize(usedFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                  inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime))
      }
  } else {
      if (youth) {
        regdata <- data %>% filter(dx_ed_under20 == 1) %>% 
          group_by(PROVID, ENROLID, datemon) %>% summarize(gotFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), 
                                                           inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime)) %>%
          group_by(PROVID, datemon) %>% summarize(usedFBT = mean(gotFBT), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                  inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime))
      } else {
        regdata <- data %>% filter(dx_ed == 1) %>% 
          group_by(PROVID, ENROLID, datemon) %>% summarize(gotFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), 
                                                           inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime)) %>%
          group_by(PROVID, datemon) %>% summarize(usedFBT = mean(gotFBT), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                  inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime))
      }
  }
  
  # Generate dummy variables for bins (6 before and 6 after treatment) 
  regdata <- regdata %>% ungroup() %>% mutate(timebin = round((regdata$datemon - as.yearmon(tmtdate)) * 12), 
                                              mons_rel_change = ifelse(timebin >= bins_to_keep*-1 & timebin <= bins_to_keep, timebin, 0))#  %>% 
  #  replace_na(list(mons_rel_change = 0)) %>% select(-timebin)
  
  esdata <- dummy_cols(regdata, select_columns = c("mons_rel_change")) %>% select(-c(timebin, mons_rel_change, mons_rel_change_NA))

  # Generate dummy variables for instrument (6 before/after instrumented event)
  esdata$inst_reltime <- round(esdata$inst_reltime * 12)
  esdata$mons_rel_inst <- ifelse(esdata$inst_reltime >= bins_to_keep*-1 & esdata$inst_reltime <= bins_to_keep, esdata$inst_reltime, 0)
  esdata <- dummy_cols(esdata, select_columns = c("mons_rel_inst")) %>% select(-c(mons_rel_inst))
  names(esdata) <- gsub(x = names(esdata), pattern = "-", replacement = "b_") # Changes periods prior to move to have a "_b_" rather than a negative sign
  
  # What to return
  esdata
}
####################


# ### 3.0. 2SLS estimation (no bootstrap)
# mr_late_regression <- function(data1, data2) {
#   # Run 2SLS of event study using YT as the dependent variable and the vector of instrument dummies as Z. 
#   # Run once for each data set. 
#   colnames <- sort(as.vector(data1 %>% select(starts_with("mons_rel_change_")) %>% colnames())) 
#   colnames_inst <- sort(as.vector(data1 %>% select(starts_with("mons_rel_inst_")) %>% colnames())) 
#   colnames <- setdiff(colnames, "mons_rel_change_b_1") # Reference group is negative 1
#   colnames_inst <- setdiff(colnames_inst, "mons_rel_inst_b_1") # Reference group is negative 1
#   iv_reg1 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
#                       instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
#                       endog = paste(colnames, collapse = "+"),
#                       data = data1, model = "within" , effect = "twoways") 
#   iv_reg2 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
#                       instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
#                       endog = paste(colnames, collapse = "+"),
#                       data = data2, model = "within" , effect = "twoways") 
#   
#   # The MR-LATE estimator is difference: (conservative estimator - liberal estimator)
#   iv_coefs1 <- data.frame(summary(iv_reg1)$coefficients[,1])
#   iv_coefs2 <- data.frame(summary(iv_reg2)$coefficients[,1])
#   iv_coefs <- iv_coefs1 - iv_coefs2
#   
#   iv_coefs
# }
# ####################


### 3.1. Estimate MR-LATE by bootstrapping (panel bootstrap -- sample i's and take all of their observations with you.)
bootstrap_results <- function(n, data1, data2) {
  ncols <- length(as.vector(data1 %>% select(starts_with("mons_rel_change_")))) - 1
  boot_results <- matrix(NA, nrow = n, ncol = ncols)
  
  colnames <- sort(as.vector(data1 %>% select(starts_with("mons_rel_change_")) %>% colnames()))
  colnames <- setdiff(colnames, "mons_rel_change_b_1") # Reference group is negative 1
  colnames_inst <- sort(as.vector(data1 %>% select(starts_with("mons_rel_inst_")) %>% colnames())) 
  colnames_inst <- setdiff(colnames_inst, "mons_rel_inst_b_1") # Reference group is negative 1
  
  boot_results <- as.data.frame(boot_results, col.names = colnames) # Empty bootstrap matrix. 
  
  allprovs <- union(sort(unique(data1$PROVID)), sort(unique(data2$PROVID))) # Specialists from which you can bootstrap
  
  for (j in 1:n) {
    bootprovs <- sort(sample(allprovs, size = length(allprovs), replace = T))
    
    esdata1_b <- do.call(rbind, lapply(1:length(bootprovs), function(i)  data1[data1$PROVID==bootprovs[i],] %>% mutate(PROVID = PROVID + i)))
    esdata2_b <- do.call(rbind, lapply(1:length(bootprovs), function(i)  data2[data2$PROVID==bootprovs[i],] %>% mutate(PROVID = PROVID + i)))
    
    iv_reg1 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
                        instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
                        endog = paste(colnames, collapse = "+"),
                        data = esdata1_b, model = "within" , effect = "twoways") 
    iv_reg2 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
                        instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
                        endog = paste(colnames, collapse = "+"),
                        data = esdata2_b, model = "within" , effect = "twoways") 
    
    # The MR-LATE estimator is difference: (conservative estimator - liberal estimator)
    boot_results[j, ] <- t(data.frame(summary(iv_reg1)$coefficients[,1]) - data.frame(summary(iv_reg2)$coefficients[,1]))
  }
  
  boot_results
}
####################


### 4. Estimating the different specifications (FBT/Drugs times all provs/spec provs)
# The options we have are: FBT/Drugs, dummy/continuous DV, youth/all pats, normed/unnormed tmt, homogeneous/heterogeneous, 5 threshold pairs (32*5=160 combos FOR EACH IV). 

## SPEC 1: FBT, Dummy, All Pats, Unnormed, Homogeneous (will recreate with all thresholds). 
# want_graphs <- T
# thresholds <- c(.02, .98) # What are the two mis-measured treatment definitions you are using?
# Want to use the following pairs: 1/99, 2/98, 5/95, 10/90, 15/85. 

# Set up two data sets (once for each threshold):
working$tmtdate <- working[[date1]]
working$treated <- working[[nam1]]
esdata1 <- es_data_setup(working, want_graph, FBTuse_dummy = F,
                         youth = F, bins_to_keep = 6)
esdata1$depvar <- esdata1$usedFBT * esdata1$treated # New DV is product of y and mismeasured T

working$tmtdate <- working[[date2]]
working$treated <- working[[nam2]]
esdata2 <- es_data_setup(working, want_graph, FBTuse_dummy = F,
                         youth = F, bins_to_keep = 6)
esdata2$depvar <- esdata2$usedFBT * esdata2$treated # New DV is product of y and mismeasured T
# Run the regressions:
bootstrap <- bootstrap_results(n_bootstrap, esdata1, esdata2) 
colnames <- lapply(1:length(colnames), function(i) paste(colnames[i], "_", sep = ""))
names(bootstrap) <- colnames
# Save bootstrap to never have to run again
save(bootstrap, file = here("2_Data/Conference_Impacts/Bootstrap_Matrices", "FBTDummy_01_99_Unnormed.Rda"))

# Collapse and graph the results: 
if (want_graphs) {
  tograph <- bootstrap %>% summarize_all(funs(mean, sd)) %>% 
    gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
    spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
    mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE) 
  # Change SD to standard error, create lower/upper bounds for point estimates
  tograph <- rbind(tograph, list("mons_rel_change_b_1", 0, 0, 0, 0)) # Adding back the reference group
  tograph$period <- as.numeric(ifelse(substr(tograph$Measurement, 17,17) == "b", 
                                      paste("-", as.numeric(gsub("[^0-9.]", "",tograph$Measurement)), sep = ""), 
                                      as.numeric(gsub("[^0-9.]", "",tograph$Measurement)))) # Creating time value
  ESgraph <- ggplot(tograph, aes(x=period, y=Mean, group=1)) + geom_line(color = "darkturquoise", size = 1.) + 
    geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
    geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
    labs(x = "Months Before/After Conference", y = "Use of FBT") + 
    ggtitle('Estimated Effect of FBT Conferences on ED Treatments') + 
    theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
  ggsave(ESgraph, file = here("4_output/figures/Conference_Impacts/Bootstrapped", "FBTDummy_01_99_Unnormed.png"), width = 18, height = 9)
}
####################


### 5. Heterogeneity by Provider Type
spec_mhfac <- c(20, 21, 22, 23, 25, 31, 35, 37, 40)
spec_meddoc <- c(200, 206, 240)
spec_psychiatry <- c(365, 458, 824)
spec_pediatrician <- c(400, 458) 
spec_nursing <- c(822, 824, 825, 845)
spec_therapists <- c(853)
spec_psychology <- c(860)

working$het_psycho <- ifelse(working$STDPROV %in% spec_psychology, 1, 0)
working$het_nonmh <- ifelse(working$STDPROV %in% spec_meddoc | working$STDPROV %in% spec_pediatrician, 1, 0)
working[which(working$STDPROV == 458), ]$het_nonmh <- 0 # don't count child psychiatry here
working$het_nursing <- ifelse(working$STDPROV %in% spec_nursing, 1, 0)
working$het_ther <- ifelse(working$STDPROV %in% spec_therapists, 1, 0)

es_data_setup_SPECS <- function(data, want_graph = T, FBTuse_dummy = T,
                          youth = F, bins_to_keep = 6) {
  # data: full analysis data -- should have a dummy variable for treatment status and a tmtdate variable
  # normed: Do you want to use the groups implied by travel costs that are salary-normalized? 
  # want_graph: Do you want to save the graph? 
  # FBTuse_dummy: Do you want the main DV to be a dummy for FBT use in ED, or the fraction of ED patients getting FBT? 
  # youth: Do you want to examine treatment of AN patients under 20? (default is F)
  # bins_to_keep: Periods before/after conference to use in dummy variables
  
  # Filter only ED patients, construct relevant outcome variable and treatment status/date
  if (FBTuse_dummy) {
    if (youth) {
      regdata <- data %>% filter(dx_ed_under20 == 1) %>% 
        group_by(PROVID, datemon) %>% summarize(usedFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther))
    } else {
      regdata <- data %>% filter(dx_ed == 1) %>% 
        group_by(PROVID, datemon) %>% summarize(usedFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther))
    }
  } else {
    if (youth) {
      regdata <- data %>% filter(dx_ed_under20 == 1) %>% 
        group_by(PROVID, ENROLID, datemon) %>% summarize(gotFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), 
                                                         inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                         het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther)) %>%
        group_by(PROVID, datemon) %>% summarize(usedFBT = mean(gotFBT), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther))
    } else {
      regdata <- data %>% filter(dx_ed == 1) %>% 
        group_by(PROVID, ENROLID, datemon) %>% summarize(gotFBT = max(fam_therapy), tmtdate = first(tmtdate), treated = max(treated), 
                                                         inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                         het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther)) %>%
        group_by(PROVID, datemon) %>% summarize(usedFBT = mean(gotFBT), tmtdate = first(tmtdate), treated = max(treated), numpats = n_distinct(ENROLID), 
                                                inst = mean(inst), inst_binary = mean(inst_binary), inst_event = mean(inst_event), inst_reltime = mean(inst_reltime), 
                                                het_psycho = mean(het_psycho), het_nonmh = mean(het_nonmh), het_nursing = mean(het_nursing), het_ther = mean(het_ther))
    }
  }
  
  # Keep dummy vars as dummies by rounding
  regdata <- regdata %>% mutate(het_psycho = ifelse(het_psycho >= 0.5, 1, 0), 
                                het_nonmh = ifelse(het_nonmh >= 0.5, 1, 0), 
                                het_nursing = ifelse(het_nursing >= 0.5, 1, 0), 
                                het_ther = ifelse(het_ther >= 0.5, 1, 0))
  
  # Generate dummy variables for bins (6 before and 6 after treatment) 
  regdata <- regdata %>% ungroup() %>% mutate(timebin = round((regdata$datemon - as.yearmon(tmtdate)) * 12), 
                                              mons_rel_change = ifelse(timebin >= bins_to_keep*-1 & timebin <= bins_to_keep, timebin, 0))#  %>% 
  #  replace_na(list(mons_rel_change = 0)) %>% select(-timebin)
  
  esdata <- dummy_cols(regdata, select_columns = c("mons_rel_change")) %>% select(-c(timebin, mons_rel_change, mons_rel_change_NA))
  
  # Generate dummy variables for instrument (6 before/after instrumented event)
  esdata$inst_reltime <- round(esdata$inst_reltime * 12)
  esdata$mons_rel_inst <- ifelse(esdata$inst_reltime >= bins_to_keep*-1 & esdata$inst_reltime <= bins_to_keep, esdata$inst_reltime, 0)
  esdata <- dummy_cols(esdata, select_columns = c("mons_rel_inst")) %>% select(-c(mons_rel_inst))
  names(esdata) <- gsub(x = names(esdata), pattern = "-", replacement = "b_") # Changes periods prior to move to have a "_b_" rather than a negative sign
  
  
  # Generate dummy variables for interactions
  for (het in c("het_psycho", "het_ther")) { # Ignoring "het_nonmh", "het_ther" for now
    internam <- paste("mons", het, "0", sep = "_")
    esdata[[internam]] <- esdata[[het]] * esdata[["mons_rel_change_0"]]
    
    instnam <- paste("mons_ins", het, "0", sep = "_")
    esdata[[instnam]] <- esdata[[het]] * esdata[["mons_rel_inst_0"]]
    
    for (i in 1:6) {
      pos_nam <- paste("mons_rel_change", i, sep = "_")
      pos_nam_i <- paste("mons_rel_inst", i, sep = "_")
      internam1 <- paste("mons", het, i, sep = "_")
      internam1_i <- paste("mons_ins", het, i, sep = "_")
      
      pre_nam <- paste("mons_rel_change_b", i, sep = "_")
      pre_na_i <- paste("mons_rel_inst_b", i, sep = "_")
      internam2 <- paste("mons_b", het, i, sep = "_")
      internam2_i <- paste("mons_ins_b", het, i, sep = "_")
      
      esdata[[internam1]] <- esdata[[het]] * esdata[[pos_nam]]
      esdata[[internam1_i]] <- esdata[[het]] * esdata[[pos_nam_i]]
      esdata[[internam2]] <- esdata[[het]] * esdata[[pre_nam]]
      esdata[[internam2_i]] <- esdata[[het]] * esdata[[pre_na_i]]
    }
  }

  # What to return
  esdata
}


working$tmtdate <- working[[date1]]
working$treated <- working[[nam1]]
esdata1 <- es_data_setup_SPECS(working, want_graph, FBTuse_dummy = F,
                         youth = F, bins_to_keep = 6)
esdata1$depvar <- esdata1$usedFBT * esdata1$treated # New DV is product of y and mismeasured T

working$tmtdate <- working[[date2]]
working$treated <- working[[nam2]]
esdata2 <- es_data_setup_SPECS(working, want_graph, FBTuse_dummy = F,
                         youth = F, bins_to_keep = 6)
esdata2$depvar <- esdata2$usedFBT * esdata2$treated # New DV is product of y and mismeasured T

### 3.0. 2SLS estimation (no bootstrap)
# Run 2SLS of event study using YT as the dependent variable and the vector of instrument dummies as Z.
# Run once for each data set.
colnames <- sort(as.vector(esdata1 %>% select(starts_with("mons_rel_change_"), starts_with("mons_het"), starts_with("mons_b_het")) %>% colnames()))
colnames_inst <- sort(as.vector(esdata1 %>% select(starts_with("mons_rel_inst_"), starts_with("mons_ins_het"), starts_with("mons_ins_b_het")) %>% colnames()))
colnames <- setdiff(colnames, c("mons_rel_change_b_1", "mons_b_het_nonmh_1", "mons_b_het_psycho_1", "mons_b_het_nursing_1", "mons_b_het_ther_1")) 
colnames_inst <- setdiff(colnames_inst, c("mons_rel_inst_b_1", "mons_ins_b_het_nonmh_1", "mons_ins_b_het_psycho_1", "mons_ins_b_het_nursing_1", "mons_ins_b_het_ther_1"))
  # Reference group is negative 1

iv_reg1 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
                    instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
                    endog = paste(colnames, collapse = "+"),
                    data = esdata1, model = "within" , effect = "twoways")
iv_reg2 <- plm::plm(as.formula(paste("depvar ~", paste(colnames, collapse = "+"), sep = "")),
                    instruments = paste(paste(colnames, collapse = "+"), paste(colnames_inst, collapse = "+"), sep = ""),
                    endog = paste(colnames, collapse = "+"),
                    data = esdata2, model = "within" , effect = "twoways")

# The MR-LATE estimator is difference: (conservative estimator - liberal estimator)
iv_coefs1 <- data.frame(summary(iv_reg1)$coefficients[,1])
iv_coefs2 <- data.frame(summary(iv_reg2)$coefficients[,1])
iv_coefs <- iv_coefs1 - iv_coefs2
####################