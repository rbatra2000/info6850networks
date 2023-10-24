library(data.table)
library(zoo)
library(plyr)
library(tidyverse)
library(dplyr)
detach(package:dplyr)
library(dplyr)

## setwd("/Users/emmharv/Google Drive/My Drive/Cornell University/1 - Fall 2023/INFO 6850 | The Structure of Information Networks")

## Without weights, you can observe outcomes and pretty quickly arrive at an approximation of the 
## threshold after observing repeated decisions
amicus_orgs_all = runif(1000)
judge_threshold = 0.5
judge_threshold_min = c(0)
judge_threshold_max = c(1)
observed_thresholds = c()

while ((min(judge_threshold_max) - max(judge_threshold_min)) > 0.05) {

  amicus_orgs_case = sample(amicus_orgs_all, 30)
  
  vote_threshold = runif(1)
  amicus_orgs_case_votes = ifelse(amicus_orgs_case > vote_threshold, 1, 0)
  
  observed_threshold = sum(amicus_orgs_case_votes) / length(amicus_orgs_case_votes)
  judge_vote = ifelse(observed_threshold >= judge_threshold, 1, 0)
  
  observed_thresholds = c(observed_thresholds, observed_threshold)
  
  if (judge_vote == 0) {
    judge_threshold_min = c(judge_threshold_min, max(observed_threshold, max(judge_threshold_min)))
    judge_threshold_max = c(judge_threshold_max, min(judge_threshold_max))
  } else {
    judge_threshold_min = c(judge_threshold_min, max(judge_threshold_min))
    judge_threshold_max = c(judge_threshold_max, min(observed_threshold, min(judge_threshold_max)))
  }
  
}

plot(1:length(judge_threshold_max), judge_threshold_max, type="l", ylim=c(0, 1), col="blue")
lines(1:length(judge_threshold_min), judge_threshold_min, col="red")

## With weights, you absolutely can't 
amicus_orgs_weights = runif(1000, min=-1, max=2)
judge_threshold = 0.5
judge_threshold_min = c(0)
judge_threshold_max = c(1)
observed_thresholds = c()

for (i in 1:100) {
  
  index = sample(1:1000, 30)
  amicus_orgs_case = amicus_orgs_all[index]
  amicus_orgs_case_weights = amicus_orgs_weights[index]
  
  vote_threshold = runif(1)
  amicus_orgs_case_votes = ifelse(amicus_orgs_case > vote_threshold, 1, 0)
  
  observed_threshold = sum(amicus_orgs_case_votes) / length(amicus_orgs_case_votes)
  actual_threshold = sum(amicus_orgs_case_votes * amicus_orgs_case_weights) / 
    length(amicus_orgs_case_votes)
  judge_vote = ifelse(actual_threshold >= judge_threshold, 1, 0)
  
  observed_thresholds = c(observed_thresholds, observed_threshold)
  
  if (judge_vote == 0) {
    judge_threshold_min = c(judge_threshold_min, max(observed_threshold, max(judge_threshold_min)))
    judge_threshold_max = c(judge_threshold_max, min(judge_threshold_max))
  } else {
    judge_threshold_min = c(judge_threshold_min, max(judge_threshold_min))
    judge_threshold_max = c(judge_threshold_max, min(observed_threshold, min(judge_threshold_max)))
  }
  
}

plot(1:length(judge_threshold_max), judge_threshold_max, type="l", ylim=c(0, 1), col="blue")
lines(1:length(judge_threshold_min), judge_threshold_min, col="red")
