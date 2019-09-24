##### ChangingTreatmentThresholds_Table -- last modified 8.15.2019
#   Puts together a table for changing treatment thresholds. 
#
#     NOTES: 
#       -- This is done after running the bootstrapping program on the BU SCC. 
#
#     OUTPUT: 
#       -- Graphs in 4_Output/Figures/Conference_Impacts/Bootstrapped/: All confidence intervals, as well as for specific coefficients
#       -- Table __
#       -- Can also produce individual graphs if you want to see them. 
# 
#     MAJOR EDITS:  
#       -- TBA: Add in code to produce table (if you want it?)
################################################################################


### 0. User specified variables 
# This tells the file which bootstrap files to merge
intervention <- "FBT" # Options are FBT or Pharma
depvar <- "Dummy" # Options are: Dummy or Continuous
normed <- F # Options are true/false
het <- 0 # Options are 0: no het; 1: youth; and 2: specialist type

threshold <- c("15", "85") # What treatment thresholds to use -- need to be in 2-digit character format

want_graphs <- F # There will be other code to produce graphs in paper, this is just to compare. 
####################

# Packages
library(tidyverse)
library(tidylog)
library(reshape2)
library(here)


### 0. Load, collapse, and merge table data. 
# List of thresholds
thresh_l <- c("01", "02", "05", "10", "15")
thresh_u <- c("99", "98", "95", "90", "85")

if (normed) {
  norm <- "Normed"
} else {
  norm <- "Unnormed"
}

# Data needed
joined_data <- data.frame(Measurement = character(), 
                          Mean = double(), 
                          SE = double(), 
                          lb = double(), 
                          ub = double())
for (i in 1:5) {
  datanam <- paste(intervention, depvar, norm, thresh_l[i], thresh_u[i], sep = "_")
  datanam <- paste(datanam, ".RData", sep = "")
  load(here("4_Output/Bootstrapped_Data/", datanam))
  
  # Renaming one file
  if (exists("boot_results_02_98")) {
    boot_results_complete <- boot_results_02_98
    rm(boot_results_02_98)
  }
  
  # Update column names in order to make data wrangling easier.
  colnames <- colnames(boot_results_complete)
  colnames <- paste(colnames, "_", sep = "")
  names(boot_results_complete) <- colnames
  
  # Collapsing each data frame
  boot_results_complete <- boot_results_complete %>% summarize_all(list(~mean(.), ~sd(.))) %>% 
    gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
    spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
    mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE)
  
  boot_results_complete$thresh_lb <- thresh_l[i]
  boot_results_complete$thresh_ub <- thresh_u[i]
  
  joined_data <- rbind(joined_data, boot_results_complete)
  
  rm(boot_results_complete)
}


### 1. Graph confidence intervals
joined_data$variable <- as.numeric(ifelse(substr(joined_data$Measurement, 17,17) == "b", 
                                          paste("-", as.numeric(gsub("[^0-9.]", "",joined_data$Measurement)), sep = ""), 
                                          as.numeric(gsub("[^0-9.]", "",joined_data$Measurement))))


# Graph for only beta_0, beta_1, and beta_2 (main text)
working <- joined_data[which(joined_data$variable == 0 | joined_data$variable == 1 | joined_data$variable == 2), ]
someplot <- ggplot(working, aes(x=thresh_lb, y=Mean, ymin=lb, ymax=ub, color=as.factor(variable))) + geom_pointrange() + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1) + 
  #ylim(-.1,.1) +
  coord_flip() + facet_wrap(~variable) + 
  theme_minimal() + labs(x = 'Lower Bound of Treatment Threshold', y = 'Coefficient Value', color = 'Model Coefficient (Period rel to tmt)') + ggtitle('Effect of Varying Treatment Threshold on Model Coefficients')
filenam <- paste(intervention, depvar, norm, "Robustness_Treatment_Thresholds_SIMPLIFIED.png", sep = "_")
ggsave(someplot, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)

# Graph for all coefficients (appendix)
joined_data$variable <- factor(joined_data$variable, labels = c("gamma[-6]", "gamma[-5]", "gamma[-4]", "gamma[-3]", "gamma[-2]", "gamma[0]", 
                                                                "gamma[1]",  "gamma[2]",  "gamma[3]",  "gamma[4]",  "gamma[5]",  "gamma[6]"))
allplot <- ggplot(joined_data, aes(x=thresh_lb, y=Mean, ymin=lb, ymax=ub, color=as.factor(variable))) + geom_pointrange(size=1.5) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1) + 
  #ylim(-.1,.1) +
  coord_flip() + facet_wrap(~variable, labeller = 'label_parsed') + 
  theme_minimal() + labs(x = 'Lower Bound of Treatment Threshold', y = 'Coefficient Value', color = 'Model Coefficient (Period rel to tmt)') + 
  ggtitle('Effect of Varying Treatment Threshold on Model Coefficients') + theme(plot.title = element_text(size = 26, face = "bold"), 
                                                                                 axis.text = element_text(size=20), axis.title = element_text(size=26), 
                                                                                 strip.text.x = element_text(size=20),legend.position = 'none')
allplot 

filenam <- paste(intervention, depvar, norm, "Robustness_Treatment_Thresholds.png", sep = "_")
ggsave(allplot, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
####################


### 2. Table for confidence intervals
 # Add this in later -- probably don't want a table here. 
####################


### 3. Graph for each individual ES. 
if (want_graphs) {
  for (i in 1:length(bootlist)) {
    tograph <- bootlist[[i]]
    tograph <- rbind(tograph, list("mons_rel_change_b_1", 0, 0, 0, 0))
    tograph$period <- as.numeric(ifelse(substr(tograph$Measurement, 17,17) == "b", 
                                           paste("-", as.numeric(gsub("[^0-9.]", "",tograph$Measurement)), sep = ""), 
                                           as.numeric(gsub("[^0-9.]", "",tograph$Measurement))))
    mygraph <-  ggplot(tograph, aes(x=period, y=Mean, group=1)) + geom_line(color = "darkturquoise", size = 1.) + 
                       geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
                       geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
                       geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
                       labs(x = "Months Before/After Conference", y = "Use of FBT") + 
                       ggtitle('Estimated Effect of FBT Conferences on ED Treatments') + 
                       theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    filenam <- paste("FBTDummy_", thresh_l[i], "_", thresh_u[i], "_Unnormed.png", sep = "")
    ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
  }
}
####################