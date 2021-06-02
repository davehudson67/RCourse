## load required libraries
library(tidyverse)
library(stringi)

## read in data
badgers1 <- read_csv("Badgers.csv")

## create IDs
ID <- sprintf("%s%s", stri_rand_strings(100, 2, '[A-Z]'),
        stri_rand_strings(100, 3, '[0-9]'))

badgers1 <- badgers1 %>%
  mutate(BadgerTailLength = BadgerTailLength/100) %>%
  mutate(BadgerWhiskerLength = BadgerWhiskerLength * 0.39)

badgers1 <- badgers1 %>%
  mutate(ID = ID)
#  mutate(Year = 2010) %>%
#  mutate(rainfall = "low")

#saveRDS(badgers1, "Badgers2010.rds")
write.csv(badgers1, "badgers2010.csv")

badgers1 <- badgers

badgers2 <- badgers1 %>%
  mutate(Year = 2011) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 1.5, 2)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "medium")

badgers3 <- badgers2 %>%
  mutate(Year = 2012) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 1.5, 2)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "medium")

badgers4 <- badgers3 %>%
  mutate(Year = 2013) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 5, 2)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "high")

badgers5 <- badgers4 %>%
  mutate(Year = 2014) %>%
  mutate(Age = Age + 1) %>%
  #mutate(Tail = Tail + rnorm(nrow(badgers1), 5, 0.1)) %>%
  #mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 0.005)) %>%
  mutate(rainfall = "low")

badgers6 <- badgers5 %>%
  mutate(Year = 2015) %>%
  mutate(Age = Age + 1) %>%
  #mutate(Tail = Tail + rnorm(nrow(badgers1), 5, 0.1)) %>%
  #mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 0.005)) %>%
  mutate(rainfall = "low")

badgers7 <- badgers6 %>%
  mutate(Year = 2016) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 5, 2)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "high")

badgers8 <- badgers7 %>%
  mutate(Year = 2017) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 1.5, 0.5)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "medium")

badgers9 <- badgers8 %>%
  mutate(Year = 2018) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 1.5, 0.5)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "medium")

badgers10 <- badgers9 %>%
  mutate(Year = 2019) %>%
  mutate(Age = Age + 1) %>%
  mutate(Tail = Tail + rnorm(nrow(badgers1), 5, 0.5)) %>%
  mutate(Whisker = Whisker + rnorm(nrow(badgers1), 3, 1)) %>%
  mutate(rainfall = "high")

## stack data

badgers <- bind_rows(badgers2, badgers3, badgers4, badgers5, badgers6, badgers7, badgers8, badgers9, badgers10)

## add some NA
badgers <- badgers %>%
  mutate(Age = ifelse(rbinom(nrow(badgers), 1, 0.05) == 1, NA, Age)) %>%
  mutate(Sex = ifelse(rbinom(nrow(badgers), 1, 0.05) == 1, NA, Sex))
#  mutate(Tail = ifelse(rbinom(nrow(badgers), 1, 0.02) == 1, NA, Tail))

saveRDS(badgers, "Badgers201120.rds")
write.csv(badgers, "badgers201120.csv")

## 