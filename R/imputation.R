install.packages("tidyverse") # delete this line after first execution
library(tidyverse)

# set the working directory
# (you don't have to understand this line ;))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read the raw data file from MaxQuant
# the proteinGroups.txt has to be in the same folder as this script is
proteinGroups <- read_delim(file = "proteinGroups.txt",
                            delim = "\t")
proteinGroups <- as.data.frame(proteinGroups)


# select only the columns we need
data <- proteinGroups %>%
  select(starts_with("LFQ Intensity DMSO"), starts_with("LFQ Intensity NC1"))


# do a log2 transform
data.log2 <- log2(data)


# replace Inf and -Inf by NA
# the square brackets serve as selector to select only values that are Inf or -Inf in the dataframe
# these values are modified to NA, so further analysis is easier
data.log2[data.log2 == Inf | data.log2 == -Inf] <- NA


# filter out proteins with more than 3 missing values
number_missing_values_per_protein <- apply(X = data.log2,
                                           MARGIN = 1,
                                           FUN = function (protein) {
                                             protein %>%
                                               is.na() %>%
                                               sum()
                                           })

data.log2 <- data.log2[number_missing_values_per_protein <= 8, ]


# how many missing values do we have?
number_of_nas <- sum(is.na(data.log2))


# extract all finite values
finite.values <- data.log2[!is.na(data.log2)]


# draw initial distributions
finite.values %>%
  density() %>%
  plot(main = "LFQ intensity density")


# fit a normal distribution
fit <- MASS::fitdistr(finite.values, densfun = "normal")
fit.mean <- fit$estimate[1]
fit.sd <- fit$estimate[2]


# draw curve
curve(dnorm(x, fit.mean, fit.sd),
      from = min(finite.values),
      to = max(finite.values),
      col = "red",
      add = TRUE)


# change width of normal distribution
curve(0.3 * dnorm(x, fit.mean, fit.sd * 0.3),
      from = min(finite.values),
      to = max(finite.values),
      col = "purple",
      add = TRUE)


# include a down-shift
curve(0.3 * dnorm(x, fit.mean - 1.8 * fit.sd, fit.sd * 0.3),
      from = min(finite.values),
      to = max(finite.values),
      col = "blue",
      add = TRUE)


# generate 1000 semi-random numbers from resized and shifted normal distribution
semi.randoms <- rnorm(n = 1000,
                      mean = fit.mean - 1.8 * fit.sd,
                      sd = 0.3 * fit.sd)


# plot them
hist(semi.randoms, breaks = 20)


# impute the missing values in our data
data.imputed <- data.log2
data.imputed[is.na(data.imputed)] <- rnorm(n = number_of_nas,
                                           mean = fit.mean - 1.8 * fit.sd,
                                           sd = 0.3 * fit.sd)


# plot the density of the imputed data
finite.values %>%
  density() %>%
  plot(main = "LFQ intensity density", ylim = c(0, 0.3))

data.imputed %>%
  unlist() %>%
  density() %>%
  lines(lwd = 2)

