#########################
#                       #
# Author: Kyle Dixon    #
# Created: July 28 2021 #
# Title: Model Building #
#                       #
#########################

## Load libraries
library(pROC)

# Clear the environment
base::rm(list = base::ls())

## Load in processed data
base::load("raw.RData")

processed <- processed[stats::complete.cases(processed), ]

logit <- stats::glm(lawsuit ~ negative + positive + uncertainty + litigious + constraining + superfluous + sentiment + return, data = processed, family = "binomial")

processed$prob = stats::predict(logit, type = c("response"))
base::plot(pROC::roc(lawsuit ~ prob, data = processed))
