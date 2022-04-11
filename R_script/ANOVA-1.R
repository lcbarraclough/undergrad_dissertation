#Laura Barraclough
#Started: 13/02/2022
#email: s1729795@ed.ac.uk

# This script aims to conduct an Analysis of Variance (ANOVA) test on hypothesis 1
# based on the script from Coding Club Tutorials and documents provided for Professional
# skills for ecologists.

# Workflow:
# 1. Load libraries and data frame
# 2. State research question and variables
# 3. State hypothesis
# 4. Data formatting
# 5. One-way ANOVA
#   (i) Mean Abundance
#   (ii) Margalef's index
#   (iii) Menhinick's index
#   (iv) Simpson's index
# 6. Check assumptions
# shapiro test


# 1. Load libraries and data frame ----
library(tidyverse)


setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation/species_indices")
sum_data <- read.csv("sum_data_noants3.csv")
View(sum_data)

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


# 5. One-way ANOVA ----
#   (i)
abun_anova2 <- aov(Abun_per_area ~ Last_felled, data = sum_data)
summary(abun_anova2)
# this is count data so cannot be used for ANOVA. Divide by size of plot m^2

#   (ii)
marg_anova <- aov(Margalefs_RI ~ Last_felled, data = sum_data)
summary(marg_anova)
# Significant so will do a post hoc test (Tukey test)
plot(TukeyHSD(marg_anova)) #this hows that 1988 and 1998 are similar but there
# is a difference between 1988 and 2008.

#   (iii)
mehn_anova <- aov(Menhinicks_RI ~ Last_felled, data = sum_data)
summary(mehn_anova)
# Significant so will do a post hoc test (Tukey test)
plot(TukeyHSD(mehn_anova))

#   (iv)
simp_anova <- aov(Simpsons_EI ~ Last_felled, data = sum_data)
summary(simp_anova)

# 6. Check assumptions ----
  #Assumptions are: data are normally distributes, varainces are homogenous, and observations are independant
# (i) Abundance
# Checking for normality
par(mfrow = c(1,2))
hist(abun_anova2$residuals)
#residuals aren't quite normally distributed.
plot(abun_anova2, which = 2)

# Checking for homoscedasticity  (homogeneity of variances)
plot(abun_anova2, which = 1)
#data is homoscedastic

shapiro.test(sum_data2$Abun_per_area)
# W = 0.85031, p-value = 0.03704


# (ii) Margalefs
par(mfrow = c(1,2))
hist(marg_anova$residuals)
plot(marg_anova, which = 2)
#data is normally distributed but the points don't quite lie on the q-q plot line

# Checking for homoscedasticity  (homogeneity of variances)
plot(marg_anova, which = 1)
#data is homoscedastic

shapiro.test(sum_data2$Margalefs_RI)
#W = 0.87818, p-value = 0.08307

# (iii) Menhinicks
par(mfrow = c(1,2))
hist(mehn_anova$residuals)
plot(mehn_anova, which = 2)
#distribution looks slighlty skewed to the left but the qq plot looks good
# Checking for homoscedasticity  (homogeneity of variances)
plot(mehn_anova, which = 1)
#points may not be homoscedastic

shapiro.test(sum_data2$Menhinicks_RI)
#W = 0.95371, p-value = 0.6917

# (iv) Simpsons
par(mfrow = c(1,2))
hist(simp_anova$residuals)
plot(simp_anova, which = 2)
#distribution is skewed to the left but points lie on the qq
# Checking for homoscedasticity  (homogeneity of variances)
plot(simp_anova, which = 1)

shapiro.test(sum_data2$Simpsons_EI)
#W = 0.84643, p-value = 0.03318