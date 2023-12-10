library(data.table)
library(zoo)
library(plyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
detach(package:dplyr)
library(dplyr)

#setwd("/Users/emmharv/Google Drive/My Drive/Cornell University/1 - Fall 2023/INFO 6850 | The Structure of Information Networks")

## Checking convergence time over different thresholds/distributions
out = data.frame(threshold=double(), iters_unif=integer(), iters_norm=integer())
i = 1
for (judge_threshold in seq(0, 1, by=0.1)) {
  
  for (j in 1:1000) {
    
    ## Uniform
    judge_threshold_min = c(0)
    judge_threshold_max = c(1)
    while ((min(judge_threshold_max) - max(judge_threshold_min)) > 0.05) {
      
      observed_threshold = runif(1)
      judge_vote = ifelse(observed_threshold >= judge_threshold, 1, 0)
      
      if (judge_vote == 0) {
        judge_threshold_min = c(judge_threshold_min, max(observed_threshold, max(judge_threshold_min)))
        judge_threshold_max = c(judge_threshold_max, min(judge_threshold_max))
      } else {
        judge_threshold_min = c(judge_threshold_min, max(judge_threshold_min))
        judge_threshold_max = c(judge_threshold_max, min(observed_threshold, min(judge_threshold_max)))
      }
    }
    iter_unif = length(judge_threshold_min)
    
    ## Normal
    judge_threshold_min = c(0)
    judge_threshold_max = c(1)
    while ((min(judge_threshold_max) - max(judge_threshold_min)) > 0.05) {
      
      observed_threshold = rnorm(1, mean=0.5, sd=0.1667)
      judge_vote = ifelse(observed_threshold >= judge_threshold, 1, 0)
      
      if (judge_vote == 0) {
        judge_threshold_min = c(judge_threshold_min, max(observed_threshold, max(judge_threshold_min)))
        judge_threshold_max = c(judge_threshold_max, min(judge_threshold_max))
      } else {
        judge_threshold_min = c(judge_threshold_min, max(judge_threshold_min))
        judge_threshold_max = c(judge_threshold_max, min(observed_threshold, min(judge_threshold_max)))
      }
    }
    iter_norm = length(judge_threshold_min)
    
    out[i, ] = list(judge_threshold, iter_unif, iter_norm)
    i = i + 1
  }
}

View(out %>% group_by(threshold) %>% summarise(unif = mean(iters_unif), norm = mean(iters_norm)))
View(out %>% group_by(threshold) %>% summarise(unif = sd(iters_unif), norm = sd(iters_norm)))
View(out %>% group_by(threshold) %>% summarise(unif = min(iters_unif), norm = min(iters_norm)))
View(out %>% group_by(threshold) %>% summarise(unif = max(iters_unif), norm = max(iters_norm)))

