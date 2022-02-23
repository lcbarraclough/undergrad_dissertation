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
# 4. Data formatting
# 5. One-way ANOVA
# 6. Check assumptions

# 1. Libraries and data frame ----
library(tidyverse)

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

# 4. Data formatting ----
