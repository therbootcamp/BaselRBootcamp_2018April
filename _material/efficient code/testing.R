
require(tidyverse)

df <- read_csv('data/titanic.csv')

f0 <- function(){
  male_age <- df$Age[df$Sex == 'male']
  female_age <- df$Age[df$Sex == 'female']
  c(mean(male_age,na.rm=T),mean(female_age,na.rm=T))
  }

f1 <- function() df %>%
  group_by(Sex < 0) %>%
  summarize(mean_age = sum(Age,na.rm=T) / sum(!is.na(Age)))

microbenchmark(f0(),f1())

x = rnorm(1000000)

f0 <- function() mean(x)
f1 <- function() sum(x)/length(x)

microbenchmark(f0(),f1())



f0 <- function() titanic %>% .$Age
f1 <- function() titanic$Age
f2 <- function() titanic_df$Age
microbenchmark(f0,f1,f2,times=10000)


library(microbenchmark)

x <- runif(100)
x <- as.integer(x)
microbenchmark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1 / 2),
  exp(log(x) / 2)
)




## Reasons for R being slow
# Extreme Dynamism
# Name lookup and environments
# Evaluation overhead
# Old implementation


f0 <- function() NULL
f1 <- function(a = 1) NULL
f2 <- function(a = 1, b = 1) NULL
f3 <- function(a = 1, b = 2, c = 3) NULL
f4 <- function(a = 1, b = 2, c = 4, d = 4) NULL
f5 <- function(a = 1, b = 2, c = 4, d = 4, e = 5) NULL
microbenchmark(f0(), f1(), f2(), f3(), f4(), f5(), times = 10000)


microbenchmark(
  "[32, 11]"      = mtcars[32, 11],
  "$carb[32]"     = mtcars$carb[32],
  "[[c(11, 32)]]" = mtcars[[c(11, 32)]],
  "[[11]][32]"    = mtcars[[11]][32],
  ".subset2"      = .subset2(mtcars, 11)[32]
  )

