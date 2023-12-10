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

data <- data.frame('threshold'=seq(0, 99, 1))

data <- data %>%
  mutate(sigmoid = round(1 / (1 + exp(-seq(-4, 4, length.out=100))) * 100),
         log = c(1, round(log(seq(1.5, 100, length.out=99)) * 21)))

p <- data %>%
  ggplot(aes(x=threshold, y=sigmoid)) +
  geom_area(fill="#238A8DFF", alpha=0.5) +
  geom_line(color="#238A8DFF") +
  geom_abline(intercept = 0, slope = 1, color="#2D708EFF", linetype="dashed")+
  ylab("F(x) = cumulative distribution function of thresholds") +
  xlab("x = thresholds") +
  theme_classic() #+ ggtitle("Equilibrium = 96")

print(p)
ggsave(filename="equilibrium_96.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)

p <- data %>%
  ggplot(aes(x=threshold, y=sigmoid)) +
  geom_area(fill="#238A8DFF", alpha=0.5) +
  geom_line(color="#238A8DFF") +
  geom_abline(intercept = 0, slope = 1, color="#2D708EFF", linetype="dashed")+
  ylab("F(x) = cumulative distribution function of thresholds") +
  xlab("x = thresholds") +
  ylim(0, 100) +
  theme_classic() #+ ggtitle("Equilibrium = 0")

print(p)
ggsave(filename="equilibrium_2.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)

distros <- data.frame(normal=pmax(0, pmin(100, rnorm(1000000, mean=50, sd=14))), 
                      uniform=runif(1000000, 0, 100))

p <- distros %>%
  ggplot(aes(x=normal)) +
  geom_density(fill="#481567FF", color="#e9ecef", alpha=0.5) + 
  ylab("% of cases") +
  xlab("aggregate amici opinion (% conservative)") +
  theme_classic()

print(p)
ggsave(filename="normal.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)

p <- distros %>%
  ggplot(aes(x=uniform)) +
  geom_density(fill="#481567FF", color="#e9ecef", alpha=0.5) + 
  ylab("% of cases") +
  xlab("aggregate amici opinion (% conservative)") +
  theme_classic()

print(p)
ggsave(filename="uniform.png", plot=p, device="png",
       height=4, width=4, units="in", dpi=500)
