##### Combining_HalfEstimators.R -- last modified 8.23.2019
#   Makes a figure showing how the difference in estimators (shows the estimators for both thresholds and their difference)
#
#     NOTES: 
#       -- This is done after running the bootstrapping program on the BU SCC. 
#
#     OUTPUT: 
#       -- Figure in "4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators/"
# 
#     MAJOR EDITS:  
#       -- TBA: 
################################################################################


### 0. User specified variables 
# This tells the file which bootstrap files to merge
intervention <- "FBT" # Options are FBT or Pharma
depvar <- "Dummy" # Options are: Dummy or Continuous for FBT and AllScrips or Olanz for Pharma
normed <- F # Options are true/false
het <- 0 # Options are 0: no het; 1: youth; and 2: specialist type

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

if (het == 0) {
  datanam1 <- paste(intervention, depvar, norm, threshold[1], sep = "_")
  datanam2 <- paste(intervention, depvar, norm, threshold[2], sep = "_")
  filenam1 <- paste(datanam1, ".png", sep = "")
  filenam2 <- paste(datanam2, ".png", sep = "")
} else if (het == 1) {
  # datanam1 <- paste("HET_Youth", intervention, depvar, norm, threshold[1], threshold[2], sep = "_")
  # filenam <- paste(datanam, ".png", sep = "")
} else if (het == 2) {
  # datanam <- paste("HET_Specialists", intervention, depvar, norm, threshold[1], threshold[2], sep = "_")
  # filenam <- paste(datanam, ".png", sep = "")
}

# Data needed
datanam1 <- paste(datanam1, ".RData", sep = "")
datanam2 <- paste(datanam2, ".RData", sep = "")
load(file = here("4_Output/Bootstrapped_Data/HalfResults", datanam1))
mydata1 <- boot_results_complete
load(file = here("4_Output/Bootstrapped_Data/HalfResults", datanam2))
mydata2 <- boot_results_complete
rm(boot_results_complete)

# Update column names in order to make data wrangling easier.
colnames <- colnames(mydata1)
colnames <- paste(colnames, "_", sep = "")
names(mydata1) <- colnames

colnames <- colnames(mydata2)
colnames <- paste(colnames, "_", sep = "")
names(mydata2) <- colnames

# Collapsing each data frame
mydata1 <- mydata1 %>% summarize_all(list(~mean(.), ~sd(.))) %>% 
    gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
    spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
    mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE)
mydata2 <- mydata2 %>% summarize_all(list(~mean(.), ~sd(.))) %>% 
  gather(key='key', value='value') %>% separate(key, into = c('Measurement', 'Statistic'), sep = "__", extra = 'merge') %>%
  spread(Statistic, value) %>% rename(Mean = mean, SE = sd) %>% 
  mutate(lb = Mean - 1.96*SE, ub = Mean + 1.96*SE)
####################


### 1. Table 
 # Add this in later -- probably don't want a table here. 
####################


### 2. Graph 
if (want_graphs) {
  if (het > 0) {
    # mydata <- rbind(mydata, list("mons_rel_change_b_1", 0, 0, 0, 0), 
    #                 list("mons_b_het_psycho_1", 0, 0, 0, 0), 
    #                 list("mons_b_het_ther_1", 0, 0, 0, 0))
    # mydata$period <- as.numeric(ifelse(grepl("b", mydata$Measurement), 
    #                                    paste("-", as.numeric(gsub("[^0-9.]", "",mydata$Measurement)), sep = ""), 
    #                                    as.numeric(gsub("[^0-9.]", "",mydata$Measurement))))
    # mydata$cat <- ifelse(grepl("ther", mydata$Measurement), "Therapist", 
    #                      ifelse(grepl("psycho", mydata$Measurement), "Psychologist", "Other MH Professional"))
    # mygraph <- ggplot(mydata, aes(x=period, y=Mean,color=factor(cat))) + geom_line(size = 1.) + 
    #   geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
    #   geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
    #   geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
    #   facet_wrap(~cat) + 
    #   labs(x = "Months Before/After Conference", y = "Use of FBT", color="Professional Type") + 
    #   ggtitle('Estimated Effect of FBT Conferences on ED Treatments') + 
    #   theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    # ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/", filenam), width = 18, height = 9)
  } else {
    ylab <- paste("Use of", intervention, ":", depvar, sep = " ")
    mydata1 <- rbind(mydata1, list("mons_rel_change_b_1", 0, 0, 0, 0))
    mydata1$period <- as.numeric(ifelse(substr(mydata1$Measurement, 17,17) == "b", 
                                           paste("-", as.numeric(gsub("[^0-9.]", "",mydata1$Measurement)), sep = ""), 
                                           as.numeric(gsub("[^0-9.]", "",mydata1$Measurement))))
    mydata2 <- rbind(mydata2, list("mons_rel_change_b_1", 0, 0, 0, 0))
    mydata2$period <- as.numeric(ifelse(substr(mydata2$Measurement, 17,17) == "b", 
                                       paste("-", as.numeric(gsub("[^0-9.]", "",mydata2$Measurement)), sep = ""), 
                                       as.numeric(gsub("[^0-9.]", "",mydata2$Measurement))))
    mydata1$model <- "lower"
    mydata2$model <- "upper"
    mydata <- rbind(mydata1, mydata2)
    
    mygraph <-  ggplot(mydata, aes(x=period, y=Mean, group=model, color=model)) + geom_line(size = 1.) + 
                       # geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
                       geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
                       geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
                       scale_y_continuous(limits = c(-0.1, 0.1)) + 
                       labs(x = "Months Before/After Conference", y = ylab, color = "Threshold") + 
                       ggtitle('Estimated Effect of FBT Conferences on ED Treatments by Threshold') + 
                       theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    
    filenam <- paste(intervention, depvar, "HalfEstimators_Complete.png", sep = "_")
    ggsave(mygraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators", filenam), width = 18, height = 9)
    
    # Piecewise graphs
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    # Only lower
    lowergraph <-  ggplot(mydata[which(mydata$model == "lower"), ], aes(x=period, y=Mean, group=model, color=model)) + geom_line(size = 1.) + 
      # geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
      scale_y_continuous(limits = c(-0.1, 0.1)) + 
      labs(x = "Months Before/After Conference", y = ylab, color = "Threshold") + 
      ggtitle('Estimated Effect of FBT Conferences on ED Treatments by Threshold') + 
      theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    
    filenam_l <- paste(intervention, depvar, "HalfEstimators_Lower.png", sep = "_")
    ggsave(lowergraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators", filenam_l), width = 18, height = 9)
    
    # Only upper
    uppergraph <-  ggplot(mydata[which(mydata$model == "upper"), ], aes(x=period, y=Mean, group=model, color=model)) + geom_line(size = 1.) + 
      scale_color_manual(values = gg_color_hue(4)[3]) + 
      # geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
      scale_y_continuous(limits = c(-0.1, 0.1)) + 
      labs(x = "Months Before/After Conference", y = ylab, color = "Threshold") + 
      ggtitle('Estimated Effect of FBT Conferences on ED Treatments by Threshold') + 
      theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    
    filenam_u <- paste(intervention, depvar, "HalfEstimators_Upper.png", sep = "_")
    ggsave(uppergraph, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators", filenam_u), width = 18, height = 9)
    
    # With dashed line for differences
    mygraph_dash <-  ggplot(mydata, aes(x=period, y=Mean, group=model, color=model)) + geom_line(size = 1.) + 
      # geom_line(aes(x=period, y = lb), linetype='dashed', color = 'grey40') + geom_line(aes(x=period, y = ub), linetype='dashed', color = 'grey40') +  
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
      geom_segment(aes(x=1, y=mydata[which(mydata$model == "lower" & mydata$period == 1), ]$Mean, xend=1, yend=mydata[which(mydata$model == "upper" & mydata$period == 1), ]$Mean), 
                   linetype='dashed', color='black', size=1.5) + 
      scale_y_continuous(limits = c(-0.1, 0.1)) + 
      labs(x = "Months Before/After Conference", y = ylab, color = "Threshold") + 
      ggtitle('Estimated Effect of FBT Conferences on ED Treatments by Threshold') + 
      theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    
    filenam_dash <- paste(intervention, depvar, "HalfEstimators_DashedLine.png", sep = "_")
    ggsave(mygraph_dash, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators", filenam_dash), width = 18, height = 9)
    
    # Complete with Differences
    tomerge <- mydata1 %>% mutate(Mean = mydata1$Mean - mydata2$Mean, model = "Differenced", SE = 0, lb = 0, ub = 0)
    mydata <- rbind(mydata, tomerge)
    mysize <- c(rep(1, nrow(mydata1)*2), rep(1.5, nrow(mydata1)))
    mygraph_diffs <-  ggplot(mydata, aes(x=period, y=Mean, group=model, color=model)) + geom_line(aes(size=mysize)) + 
      scale_color_manual(values = c(gg_color_hue(4)[4], gg_color_hue(4)[1], gg_color_hue(4)[3])) + 
      scale_size(guide=FALSE) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + 
      scale_y_continuous(limits = c(-0.15, 0.15)) + 
      labs(x = "Months Before/After Conference", y = ylab, color = "Threshold") + 
      ggtitle('Estimated Effect of FBT Conferences on ED Treatments by Threshold') + 
      theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"))
    
    filenam_diffs <- paste(intervention, depvar, "HalfEstimators_Differences.png", sep = "_")
    ggsave(mygraph_diffs, file = here("4_Output/Figures/Conference_Impacts/Bootstrapped/HalfEstimators", filenam_diffs), width = 18, height = 9)
  }
}
####################