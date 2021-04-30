## load required libraries
library(tidyverse)
library(stringi)

## read in data
badgers1 <- read_csv("Badgers.csv")

## create IDs

ID <- sprintf("%s%s", stri_rand_strings(100, 2, '[A-Z]'),
        stri_rand_strings(100, 3, '[0-9]'))


badgers1 <- badgers1 %>%
  mutate(ID = ID) %>%
  mutate(Year = 2010) %>%
  mutate(rainfall = "low")

saveRDS(badgers1, "Badgers2010.rds")


badgers2 <- badgers1 %>%
  mutate(Year = 2011) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.2, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength - rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "medium")

badgers3 <- badgers2 %>%
  mutate(Year = 2012) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.2, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength - rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "medium")

badgers4 <- badgers3 %>%
  mutate(Year = 2013) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.8, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength + rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "high")

badgers5 <- badgers4 %>%
  mutate(Year = 2014) %>%
  mutate(Age = Age + 1) %>%
  #mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.8, 0.05)) %>%
  #mutate(BadgerWhiskerLength = BadgerWhiskerLength + rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "low")

badgers6 <- badgers5 %>%
  mutate(Year = 2015) %>%
  mutate(Age = Age + 1) %>%
  #mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.8, 0.05)) %>%
  #mutate(BadgerWhiskerLength = BadgerWhiskerLength + rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "low")

badgers7 <- badgers6 %>%
  mutate(Year = 2016) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.8, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength + rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "high")

badgers8 <- badgers7 %>%
  mutate(Year = 2017) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.2, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength - rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "medium")

badgers9 <- badgers8 %>%
  mutate(Year = 2018) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.2, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength - rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "medium")

badgers10 <- badgers9 %>%
  mutate(Year = 2019) %>%
  mutate(Age = Age + 1) %>%
  mutate(BadgerTailLength = BadgerTailLength + rnorm(nrow(badgers1), 0.8, 0.05)) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength + rnorm(nrow(badgers1), 0.05, 0.005)) %>%
  mutate(rainfall = "high")

## stack data

badgers <- bind_rows(badgers1, badgers2, badgers3, badgers4, badgers5, badgers6, badgers7, badgers8, badgers9, badgers10)

## add some NA
badgers <- badgers %>%
  mutate(Age = ifelse(rbinom(nrow(badgers), 1, 0.05) == 1, NA, Age)) %>%
  mutate(Sex = ifelse(rbinom(nrow(badgers), 1, 0.05) == 1, NA, Sex)) %>%
  mutate(BadgerTailLength = ifelse(rbinom(nrow(badgers), 1, 0.02) == 1, NA, BadgerTailLength))

saveRDS(badgers, "Badgers201020.rds")

## 