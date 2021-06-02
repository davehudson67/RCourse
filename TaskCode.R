## TaskCode

## load required libraries
library(tidyverse)

## read in data
badgers <- read_csv("data/badgers2010.csv")

## viewing data
glimpse(badgers)
head(badgers)

## removing first column
#badgers <- badgers[ , -1]
#badgers$BadgerEarLength <- NULL
badgers <- select(badgers, -X1)

## renaming columns
rename(badgers, Whisker = BadgerWhiskerLength)

## task 1 : rename the other long variables to Whisker, Tail and Food.
badgers <- rename(badgers, Whisker = BadgerWhiskerLength)
badgers <- rename(badgers, Tail = BadgerTailLength)
badgers <- rename(badgers, Food = FavouriteFood)
badgers <- rename(badgers, ear = BadgerEarLength)

head(badgers)

## reorder columns
badgers <- badgers[ , c(7, 2, 3, 1, 4, 5, 6)]
badgers <- badgers[ , c(1, 3, 5, 2, 4, 6, 7)]

names(badgers)

## task 2 : reorder columns to: ID, Age, Sex, Whisker, Ear, Tail, Food, using tidyverse functions
badgers <- select(badgers, ID, Age, Sex, Whisker, ear, Tail, Food)
badgers <- rename(badgers, Ear = ear)

## filter
filter(badgers, Sex == "Male")
filter(badgers, Sex == "Male" & Food == "peanuts")
filter(badgers, Sex == "Male" | Tail < 5)

## arrange
arrange(badgers, Tail)
arrange(badgers, Ear)
arrange(badgers, Whisker)

## errors in data collection
## convert to NA
badgers$Tail[badgers$Tail < 0] <- NA
## adjust to positive


## mutate
badgers <- mutate(badgers, Age_months = Age * 12)
badgers <- mutate(badgers, FemaleCub = if_else(Sex == "Female" & Age < 1, 1, 0))


## task 3 : add a column to indicate male cubs
badgers <- mutate(badgers, MaleCub = if_else(Sex == "Male" & Age < 1, 1, 0))

## task 4 : convert all units to cms
badgers <- mutate(badgers, Whisker = Whisker / 0.39)
badgers <- mutate(badgers, Tail = Tail * 100)

## task 5 : add a year variable (2010) and a rainfall variable (low)
badgers <- mutate(badgers, rainfall = "low")
badgers <- mutate(badgers, Year = 2010)

## Use summarise and pipes to generate summary statistics
badgers %>%
  group_by(Sex) %>%
  summarise(mean(Whisker))

## task 6 : find the median ear length for those badgers whose favourite food is peanuts, is it greater or smaller than thos whose favourite food is apples?
badgers %>%
  group_by(Food) %>%
  summarise(median(Ear))

## task 7 : remove FemaleCub, Malecub, Age_months
badgers <- select(badgers, - c(MaleCub, FemaleCub, Age_months))

######################################################################################################################
## combine datasets
## task 8 : load new dataset
badgers2011_20 <- read_csv("data/badgers201120.csv")

## task 9: remove first column
badgers2011_20 <- select(badgers2011_20, -X1)

## task 10: provide summary of mean Tail length by Year
badgers2011_20 %>%
  group_by(Year) %>%
  summarise(mean(Tail, na.rm = TRUE))

## combine datasets
badgers <- bind_rows(badgers, badgers2011_20)
rm(badgers2011_20)

## assigning variables
badgers$ID <- as.factor(badgers$ID)

badgers <- badgers %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(rainfall = as.factor(rainfall)) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Food = as.factor(Food))

summary(badgers)

## dealing with missing sex
badgers$Sex <- as.numeric(badgers$Sex)
badgers$Sex[is.na(badgers$Sex)] <- 0

badgers <- badgers %>%
  group_by(ID) %>%
  mutate(Sex = max(Sex)) %>%
  mutate(Sex = if_else(Sex == 1, "Female", "Male"))

badgers$Sex <- as.factor(badgers$Sex)

summary(badgers)

#########################################################################################################################

## Data Visualisation using ggplot
## load new data

badgersA <- read_csv("data/BadgerA.csv")

badgersA <- badgersA %>%
  rename(Whisker = BadgerWhiskerLength) %>%
  rename(Paw = BadgerFrontPaw) %>%
  rename(Weight = BadgerWeightPOPA)

badgersB <- read_csv("data/BadgerB.csv")
badgersB <- badgersB %>%
  rename(Whisker = BadgerWhiskerLength) %>%
  rename(Paw = BadgerFrontPaw) %>%
  rename(Weight = BadgerWeightPOPB)

badgersC <- read_csv("data/BadgerC.csv")
badgersC <- badgersC %>%
  rename(Whisker = BadgerWhiskerLength) %>%
  rename(Paw = BadgerFrontPaw) %>%
  rename(Weight = BadgerWeightPOPC)

## combine datasets
badgers <- bind_rows(badgersA, badgersB, badgersC, .id = "Population")

## barplots ##

## add the dataframe
ggplot(badgers) +
  geom_bar(aes(x = Food))

ggplot(badgers) +
  geom_bar(aes(x = Food, fill = Sex))

ggplot(badgers) +
  geom_bar(aes(x = Food, fill = Sex), position = "dodge")


## scatterplot ##

ggplot(badgersA) +
  geom_point(aes(x = Whisker, y = Weight))

ggplot(badgersA) +
  geom_point(aes(x = Whisker, y = Weight, colour = Sex))

ggplot(badgers) +
  geom_point(aes(x = Whisker, y = Weight, colour = Population))

ggplot(badgers) +
  geom_point(aes(x = Whisker, y = Weight, colour = Sex)) +
  facet_wrap(~ Population, scales = "free")

ggplot(badgers) +
  geom_point(aes(x = Paw, y = Whisker, colour = Sex)) +
  facet_grid(Food ~ Population, scales = "free")


## Task 11 : Take the first bar plot, but adjust it so it is coloured by Sex and split by Population
ggplot(badgers) +
  geom_bar(aes(x = Food, fill = Sex), position = "dodge") +
  facet_wrap(~ Population)

## Task 12 : Scatterplot of Weight against Paw width, coloured by favourite food and faceted by population
ggplot(badgers) +
  geom_point(aes(x = Paw, y = Weight, colour = Food)) +
  facet_wrap(~ Population, scales = "free")

## Adding best fit line
ggplot(badgers, aes(x = Whisker, y = Weight, colour = Sex)) +
  geom_point() +
  facet_wrap(Food ~ Population, scales = "free") +
  stat_smooth(method="lm") +
  labs(x = "Badger whisker length (cms)", y = "Badger Weight (gms)") 

## Improving aesthetics
ggplot(badgers, aes(x = Whisker, y = Weight, colour = Sex)) +
  geom_point() +
  facet_wrap(Food ~ Population, scales = "free") +
  stat_smooth(method="lm") +
  labs(x = "Badger whisker length (cms)", y = "Badger Weight (gms)") + 
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme_light()

## colour brewer
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)      # Show all color palettes

ggplot(badgers, aes(x = Whisker, y = Weight, colour = Sex)) +
  geom_point() +
  facet_wrap(Food ~ Population, scales = "free") +
  stat_smooth(method="lm") +
  labs(x = "Badger whisker length (cms)", y = "Badger Weight (gms)") + 
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme_light() +
  scale_colour_brewer(palette = "Dark2")

#########################################################################################################################

## Statistical tests

## Linear models (response first)
model1 <- lm(Weight ~ Whisker, badgersA)
summary(model1)

ggplot(badgersA, aes(x = Whisker, y = Weight)) +
  geom_point() +
  stat_smooth(method = "lm")

## T-tests to compare 2 groups

## test for Normality
hist(badgers$Whisker[badgers$Sex == "male"])
hist(badgers$Whisker[badgers$Sex == "female"])

shapiro.test(badgers$Whisker[badgers$Sex == "male"])
shapiro.test(badgers$Whisker[badgers$Sex == "female"])

# = data is normal

## test for equality of variance
var.test(badgers$Whisker ~ badgers$Sex)
 
# = variances are equal

## Students t test
badgers$Sex <- as.factor(badgers$Sex)
t.test(badgers$Whisker ~ badgers$Sex, var.equal = T )

## Mann-Whitney U test (if both samples are not normally distributed)
wilcox.test(Whisker ~ Sex, data = badgers)

## Welch's t test (if variance is not similar in both samples)
t.test(badgers$Whisker ~ badgers$Sex)


## Transforming data - sqrt or log to achieve Normality


