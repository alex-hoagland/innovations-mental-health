##### Simple_ESTable -- last modified 8.15.2019
#   Puts together a table for a single ES regression
#
#     NOTES: 
#       -- This is done after running the bootstrapping program on the BU SCC. 
#
#     OUTPUT: 
#       -- Table __
#       -- Can also produce individual graphs if you want to see them. 
# 
#     MAJOR EDITS:  
#       -- TBA: Add in code to produce table (if you want it?)
################################################################################


### 0. User specified variables 
# This tells the file which bootstrap files to merge
intervention <- "Pharma" # Options are FBT, Pharma, or HHI
depvar <- "Olanz" # Options are: Dummy or Continuous for FBT and AllScrips or Olanz for Pharma
normed <- F # Options are true/false
het <- T # Options are true/false
numperiods <- 6 # Number of bins before/after treatment. Default is 6
youth <- F # True or false. 
threshold <- c("02", "98") # What treatment thresholds to use -- need to be in 2-digit character format

want_graphs <- T # There will be other code to produce graphs in paper, this is just to compare. 
####################

# Packages
library(tidyverse)
library(tidylog)
library(reshape2)
library(here)

if (normed) {
  norm <- "Normed"
} else {
  norm <- "Unnormed"
}

if (het) {
  datanam <- paste(intervention, depvar, norm, threshold[1], threshold[2], "HET", sep = "_")

} else {
  datanam <- paste(intervention, depvar, norm, threshold[1], threshold[2], sep = "_")
}

if (numperiods != 6) {
  datanam <- paste(datanam, "_", numperiods, "Periods", sep = "")
  filenam <- paste(datanam, "_", numperiods, "Periods", sep = "")
}

if (youth) {
  datanam <- paste(datanam, "_YOUTH", sep = "")
  filenam <- paste(datanam, "_YOUTH", sep = "")
}

# Data needed
datanam <- paste(datanam, ".RData", sep = "")
filenam <- paste(datanam, ".png", sep = "")
load(file = here("4_Output/Bootstrapped_Data/", datanam))
mydata <- boot_results_complete


# Update column names in order to make data wrangling easier.
colnames <- colnames(mydata)
colnames <- paste(colnames, "_", sep = "")
names(mydata) <- colnames

# Collapsing each data frame
mydata <- mydata %>% summarize_all(list(~mean(.), ~sd(.))) %>% 
    gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
    spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
    mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE)
####################


### 1. Table 
 # Add this in later -- probably don't want a table here. 
####################


### 2. Graph 
if (want_graphs) {
  if (het == TRUE & intervention == "FBT") {
    mydata <- rbind(mydata, list("mons_rel_change_b_1", 0, 0, 0, 0),
                    list("mons_b_het_psycho_1", 0, 0, 0, 0),
                    list("mons_b_het_ther_1", 0, 0, 0, 0))
    mydata$period <- as.numeric(ifelse(grepl("b", mydata$Measurement),
                                       paste("-", as.numeric(gsub("[^0-9.]", "",mydata$Measurement)), sep = ""),
                                       as.numeric(gsub("[^0-9.]", "",mydata$Measurement))))
    mydata$cat <- ifelse(grepl("psycho", mydata$Measurement), "Psychologist",
                         ifelse(grepl("ther", mydata$Measurement), "Therapist", "Other Mental Health Professional"))
    mycolors <- c('#A7E482', '#1FA49A', '#2F6066')
    mygraph <- ggplot(mydata, aes(x=period, y=Mean,color=factor(cat))) + geom_line(size = 3) + 
      scale_color_manual(values=mycolors) + 
      geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40', size=1.5) + 
      geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40', size=1.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.2) +
      facet_wrap(~cat) +
      labs(x = "Months Before/After Conference", y = "Likelihood of FBT Adoption", color="Professional Type") +
      ggtitle('Estimated Effect of Professional Conferences on Family-Based Therapy Adoption by Specialty') +
      theme_minimal() + theme(plot.title = element_text(size = 26, face = "bold"), 
                              axis.text = element_text(size=20), axis.title = element_text(size=26), 
                              strip.text.x = element_text(size=20),legend.position = 'none')
    ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
  } else if (het == TRUE & intervention == "Pharma") {
    mydata <- mydata[which(mydata$Measurement != "mons_b_het_psychi_1"), ]
    mydata <- rbind(mydata, list("mons_rel_change_b_1", 0, 0, 0, 0),
                    list("mons_b_het_psychi_1", 0, 0, 0, 0),
                    list("mons_b_het_nonmh_1", 0, 0, 0, 0))
    mydata$period <- as.numeric(ifelse(grepl("b", mydata$Measurement),
                                       paste("-", as.numeric(gsub("[^0-9.]", "",mydata$Measurement)), sep = ""),
                                       as.numeric(gsub("[^0-9.]", "",mydata$Measurement))))
    mydata$cat <- ifelse(grepl("psychi", mydata$Measurement), "Psychiatrist",
                         ifelse(grepl("nonmh", mydata$Measurement), "General Medicine", "Other Mental Health Prescriber"))
    mydata$cat <- factor(mydata$cat, levels = c("Other Mental Health Prescriber", "General Medicine", "Psychiatrist"))
    mycolors <- c('#A7E482', '#1FA49A', '#2F6066')
    mygraph <- ggplot(mydata, aes(x=period, y=Mean,color=factor(cat))) + geom_line(size = 3.) +
      scale_color_manual(values=mycolors) + 
      geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40', size=1.5) +
      geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40', size=1.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.2) +
      facet_wrap(~cat) +
      labs(x = "Months Before/After Conference", y = "Likelihood of Olanzapine Prescription", color="Professional Type") +
      ggtitle('Estimated Effect of Professional Conferences on Olanzapine Prescriptions by Specialty') +
      theme_minimal() + theme(plot.title = element_text(size = 26, face = "bold"), 
                              axis.text = element_text(size=20), axis.title = element_text(size=26), 
                              strip.text.x = element_text(size=20),legend.position = 'none')
    ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
  } else if (het == FALSE) {
    ylab <- paste("Use of", intervention, ":", depvar, sep = " ")
    mydata <- rbind(mydata, list("mons_rel_change_b_1", 0, 0, 0, 0))
    mydata$period <- as.numeric(ifelse(substr(mydata$Measurement, 17,17) == "b", 
                                           paste("-", as.numeric(gsub("[^0-9.]", "",mydata$Measurement)), sep = ""), 
                                           as.numeric(gsub("[^0-9.]", "",mydata$Measurement))))
    mydata <- mydata %>% filter(period <= 6 & period >= -6)
    mygraph <-  ggplot(mydata, aes(x=period, y=Mean, group=1)) + geom_line(color = '#2F6066', size = 3) + 
                       geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40', size=1.5) +
                       geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40', size=1.5) +  
                       geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 2) + 
                       geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.2) + 
                       labs(x = "Months Before/After Conference", y = 'Degree of Therapist Specialization (HHI)') + 
                       ggtitle('Estimated Effect of Professional Conferences on Therapist Specialization (HHI)') + 
                       theme_minimal() + theme(plot.title = element_text(size = 26, face = "bold"), 
                                               axis.text = element_text(size=20), axis.title = element_text(size=26))
    mygraph
    # A7E482 is green, 1FA49A is blue
    ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
  }
}
####################