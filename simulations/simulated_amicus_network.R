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

plot_data = data.frame(index = 1:length(judge_threshold_max), 
                       lower_bound = judge_threshold_min, 
                       upper_bound = judge_threshold_max, 
                       threshold = judge_threshold)

p <- plot_data %>%
  ggplot(aes(x=index, y=upper_bound)) + 
    geom_line(aes(y = lower_bound), color="#B8DE29FF") + 
    geom_line(aes(y = upper_bound), color="#3CBB75FF") +
  geom_line(aes(y = threshold), color="black", linetype="dashed") +
    geom_ribbon(#data=subset(x, 2 <= x & x <= 3), 
              aes(ymin=lower_bound,ymax=upper_bound), fill="#95D840FF", alpha=0.3) +
  theme_classic() + 
  xlab("number of iterations") + 
  ylab("threshold")

p
ggsave(filename="no_weights.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)

## With weights, you absolutely can't 
amicus_orgs_weights = runif(1000, min=0, max=2)
judge_threshold = 0.5
judge_threshold_min = c(0)
judge_threshold_max = c(1)
observed_thresholds = c()
judge_votes = c()

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
  judge_votes = c(judge_votes, judge_vote)
  
  if (judge_vote == 0) {
    judge_threshold_min = c(judge_threshold_min, max(observed_threshold, max(judge_threshold_min)))
    judge_threshold_max = c(judge_threshold_max, min(judge_threshold_max))
  } else {
    judge_threshold_min = c(judge_threshold_min, max(judge_threshold_min))
    judge_threshold_max = c(judge_threshold_max, min(observed_threshold, min(judge_threshold_max)))
  }
  
}

plot_data = data.frame(index = 1:length(judge_threshold_max), 
                       lower_bound = judge_threshold_min, 
                       upper_bound = judge_threshold_max, 
                       threshold = judge_threshold)

contradiction = min((plot_data %>% filter(lower_bound >= upper_bound))$index)

p <- plot_data %>%
  ggplot(aes(x=index, y=upper_bound)) + 
  geom_line(aes(y = lower_bound), color="#B8DE29FF") + 
  geom_line(aes(y = upper_bound), color="#3CBB75FF") +
  geom_line(aes(y = threshold), color="black", linetype="dashed") +
  geom_ribbon(data=subset(plot_data, index < contradiction), 
    aes(ymin=lower_bound,ymax=upper_bound), fill="#95D840FF", alpha=0.3) +
  geom_ribbon(data=subset(plot_data, index >= contradiction), 
              aes(ymin=upper_bound,ymax=lower_bound), fill="red", alpha=0.3) +
  theme_classic() + 
  xlab("number of iterations") + 
  ylab("threshold")

p
ggsave(filename="weights.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)

plot_data2 = data.frame(observed_threshold = observed_thresholds, 
                        vote = as.factor(ifelse(judge_votes == 1, "conservative", "liberal")), 
                        actual_threshold = judge_threshold)

plot_data2 = plot_data2 %>% 
  arrange(desc(observed_threshold)) %>%
  mutate(index = 1:nrow(plot_data2))

p <- plot_data2 %>%
  ggplot(aes(x=index, y=observed_threshold, color=vote)) + 
  geom_point() + 
  geom_line(aes(y = actual_threshold), color="black", linetype="dashed") +
  scale_color_manual(values=c('springgreen4', 'firebrick1')) + 
  theme_classic() + 
  xlab("number of cases observed") + 
  ylab("observed threshold")

p

ggsave(filename="showing_contradictions.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)
