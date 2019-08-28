# Simple variables

a <- 10 #press cmd+enter (Apple) or ctrl+enter (Windows) to run command
b <- 20

print(a + b)

c <- a * b
print(c / b)


# Vectors
v <- c(1, 2, 3, 4, 5, 6)
print(v)

print(v * 10)


# conditions

if (a < b) {
  print ("a is smaller than b.")
}

if (b <= c) {
  print("b is smaller or equal than c.")
} else {
  print("c is greater than b.")
}


# Loops
v <- c(1, 2, 3, 4, 5, 6)
for (number in v) {
  output <- paste0("This is loop number ", number)
  print(output)
}


# Functions
repeatWords <- function (words) {
  output <- paste0(words, " ", words)
  return(output)
}

newString <- repeatWords("Hello World")
print(newString)


# Apply
wordVector <- c("Hi there", "I am learning", "R")

newVector <- lapply(wordVector, repeatWords)
print(newVector)


# Use existing functions
print(unique(sort(c(8, 6, 2, 4, 6)))) # very hard to read

install.packages("tidyverse")
library(tidyverse)

c(8, 6, 2, 4, 6) %>% #better readability
  sort() %>%
  unique() %>%
  print()
