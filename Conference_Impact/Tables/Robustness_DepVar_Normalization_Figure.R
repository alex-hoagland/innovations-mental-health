##### Robustness_DepVar_Normalization_Figure -- last modified 8.21.2019
#   Puts together the figure showing how results change when you modify the DV or normalization scheme. 
#
#     NOTES: 
#       -- This is done after running the bootstrapping program on the BU SCC. 
#
#     OUTPUT: 
#       -- Graphs in 4_Output/Figures/Conference_Impacts/Bootstrapped/: All confidence intervals, as well as for first 3 coefficients
#       -- Table __
# 
#     MAJOR EDITS:  
#       -- TBA: Add in code to produce table (if you want it?)
################################################################################


### 0. User specified variables 
# This tells the file which bootstrap files to merge
intervention <- "FBT" # Options are FBT or Pharma
threshold <- c("02", "98") # What treatment thresholds to use -- need to be in 2-digit character format
####################

# Packages
library(tidyverse)
library(tidylog)
library(reshape2)
library(here)


### 0. Load, collapse, and merge table data.
joined_data <- data.frame(Measurement = character(), 
                          Mean = double(), 
                          SE = double(), 
                          lb = double(), 
                          ub = double())
for (depvar in c("Dummy", "Continuous")) {
  for (norm in c("Normed", "Unnormed")) {
    datanam <- paste(intervention, depvar, norm, threshold[1], threshold[2], sep = "_")
    datanam <- paste(datanam, ".RData", sep = "")
    load(file = here("4_Output/Bootstrapped_Data/", datanam))
    
    filenam <- paste(depvar, ", ", norm, sep = "")

    # Update column names in order to make data wrangling easier.
    colnames <- colnames(boot_results_02_98)
    colnames <- paste(colnames, "_", sep = "")
    names(boot_results_02_98) <- colnames
    
    # Collapsing each data frame
    boot_results_02_98 <- boot_results_02_98 %>% summarize_all(list(~mean(.), ~sd(.))) %>% 
      gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
      spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
      mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE)
    boot_results_02_98$regtype <- filenam
    
    joined_data <- rbind(joined_data, boot_results_02_98)
    
    rm(boot_results_02_98)
  }
}



### 1. Graph confidence intervals
joined_data$variable <- as.numeric(ifelse(substr(joined_data$Measurement, 17,17) == "b", 
                                    paste("-", as.numeric(gsub("[^0-9.]", "",joined_data$Measurement)), sep = ""), 
                                    as.numeric(gsub("[^0-9.]", "",joined_data$Measurement))))

# Graph for only beta_0, beta_1, and beta_2 (main text)
working <- joined_data[which(joined_data$variable == 0 | joined_data$variable == 1 | joined_data$variable == 2), ]
someplot <- ggplot(working, aes(x=regtype, y=Mean, ymin=lb, ymax=ub, color=as.factor(variable))) + geom_pointrange(size=0.9) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1) + 
  #ylim(-.1,.1) +
  coord_flip() + facet_wrap(~variable) + 
  theme_minimal() + labs(x = 'Regression Model Characteristics', y = 'Coefficient Value', color = 'Model Coefficient (Period rel to tmt)') + 
  ggtitle('Result Robustness: Varying Dependent Variable and Treatment Assignment')
savenam <- paste(intervention, "_Robustness_DepVar_Normalization_SIMPLIFIED.png", sep = "")
# ggsave(someplot, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", savenam), width = 18, height = 9)

# Graph for all coefficients (appendix)
joined_data$variable <- factor(joined_data$variable, labels = c("gamma[-6]", "gamma[-5]", "gamma[-4]", "gamma[-3]", "gamma[-2]", "gamma[0]", 
                                                                "gamma[1]",  "gamma[2]",  "gamma[3]",  "gamma[4]",  "gamma[5]",  "gamma[6]"))
allplot <- ggplot(joined_data, aes(x=regtype, y=Mean, ymin=lb, ymax=ub, color=as.factor(variable))) + geom_pointrange(size=1.5) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1) + 
  #ylim(-.1,.1) +
  coord_flip() + facet_wrap(~variable, labeller = 'label_parsed') + 
  theme_minimal() + labs(x = 'Regression Model Characteristics', y = 'Coefficient Value', color = 'Model Coefficient (Period rel to tmt)') + 
  ggtitle('Result Robustness: Varying Dependent Variable and Treatment Assignment') + theme(plot.title = element_text(size = 26, face = "bold"), 
                                                                                            axis.text = element_text(size=20), axis.title = element_text(size=26), 
                                                                                            strip.text.x = element_text(size=20),legend.position = 'none')
allplot

savenam <- paste(intervention, "_Robustness_DepVar_Normalization.png", sep = "")
ggsave(allplot, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", savenam), width = 18, height = 9)
####################


### 2. Table for confidence intervals
 # Add this in later -- probably don't want a table here. 
########################################