## TaskCode

## read in data
badgers <- readRDS("Badgers2010.rds")

glimpse(badgers)
head(badgers)

## Filter
filter(badgers, Sex == "Male")

## Arrange
arrange(badgers, BadgerTailLength)

arrange(badgers, desc(BadgerTailLength))


## Select
select(badgers, Sex, BadgerTailLength, Age)

select(badgers, Sex, starts_with("Badger"))

select(badgers, -starts_with("Badger"))

## Mutate
mutate(badgers, Age_months = Age * 12)
