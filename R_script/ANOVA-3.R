#Laura Barraclough
#Started: 21/02/2022
#email: s1729795@ed.ac.uk

# This script aims to conduct an Analysis of Variance (ANOVA) test on hypothesis 2
# based on the script from Coding Club Tutorials and documents provided for Professional
# skills for ecologists.

# Workflow:
# 1. Load libraries and data frame
# 2. State research question and variables
# 3. State hypothesis
# 4. One-way ANOVA
# 5. Check assumptions

# 1. Libraries and data frame ----
library(tidyverse)

getwd()
setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation")
sum_data <- read.csv("sum_data_noants.csv")
str(sum_data)

#Last_felled needs to be a vector
sum_data$Last_felled <- as.factor(as.character(sum_data$Last_felled))


# 2. State research question and variables ----
# Research question: How do the mean number of saplings per plot varied with 
# year last felled?

# Response variable is the number of saplings.
# Explanatory variable is the year last felled.

# 3. State hypothesis ----
# There will be more saplings in younger forest stands.

# 4. One-way ANOVA ----
#We need to do a bit of data wrangling first as currently this is count data which is
#not continuous

sum_data <- mutate(sum_data, saplings_per_area = Number_saplings/(pi*25))

sapling_anova <- aov(saplings_per_area ~ years_since_disturbance, data = sum_data)
summary(sapling_anova)

# 5. Check assumptions ----
par(mfrow = c(1,2))
hist(sapling_anova$residuals)
plot(sapling_anova, which = 2)
# there could be some binomial distribution going on here. There are two main outliers from
# the qq plot.

plot(sapling_anova, which = 1)

shapiro.test(sum_data$saplings_per_area)
#passes shapiro wilk normality test