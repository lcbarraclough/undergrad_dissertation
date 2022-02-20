#Laura Barraclough
#Started: 15/02/2022
#email: s1729795@ed.ac.uk

# This script aims to conduct an Analysis of Variance (ANOVA) test with lm on hypothesis 1
# based on the script from Coding Club Tutorials and documents provided for Professional
# skills for ecologists.

# Workflow:
# 1. Load libraries and data frame
# 2. State research question and variables
# 3. State hypothesis
# 4. Data formatting
# 5. lm
#   (i) Mean Abundance
#   (ii) Margalef's index
#   (iii) Menhinick's index
#   (iv) Simpson's index

# 1. Load libraries and data frame ----
library(tidyverse)

setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation")
sum_data <- read.csv("sum_data2.csv")

# 2. Research questions and variables ----
# Research question: How do the different indices for richness
# and evenness vary with felling year.

# Response variable is the various richness indices
# Explanatory variable

# 3. State hypothesis ----
# All species richness and evenness indices will be significantly higher in 1988
# than those in 2008. 

# 4. Data formatting ----
View(sum_data)
str(sum_data)
#Last_felled needs to be a vector
sum_data$Last_felled <- as.factor(as.character(sum_data$Last_felled))


# 5. lm ----

#   (i) Mean abundance
abun_lm <- lm(Total_abundance ~ Last_felled, data = sum_data)
summary(abun_lm)

#   (ii) Margalef's index
marg_lm <- lm(Margalefs_RI ~ Last_felled, data = sum_data)
summary(marg_lm)

#   (iii) Menhinick's index
menh_lm <- lm(Menhinicks_RI ~ Last_felled, data = sum_data)
summary(menh_lm)

#   (iv) Simpson's Index
simp_lm <- lm(Simpsons_EI ~ Last_felled, data = sum_data)
summary(simp_lm)
