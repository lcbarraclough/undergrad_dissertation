#Laura Barraclough
#Started: 23/12/2021
#email: s1729795@ed.ac.uk

invert_data<- read.csv("sum_data.csv")

#Load packages
#install.packages("tidyverse")
library(tidyverse)

#Initial data exploration
str(invert_data)
head(invert_data)
tail(invert_data)

#Data manipulation
invert_data <- rename(invert_data,
                     species = X,
                     A1 = X1A, B1 = X1B, C1 = X1C, D1 = X1D,
                     A2 = X2A, B2 = X2B, C2 = X2C, D2 = X2D,
                     A3 = X3A, B3 = X3B, C3 = X3C, D3 = X3D)
  #renames data columns

invert_sum <- as.data.frame(summary(invert_data))
