#Laura Barraclough
#Started: 22/02/2022
#email: s1729795@ed.ac.uk

# Load libraries and data frame ----
bugs <- read.csv("nmds.csv")

library(tidyverse)
library(vegan)
#install.packages("viridis")
library(viridis)

# Data manipulation ----
glimpse(bugs)
#changing integers to characters 
bugs$last_felled <- as.character(bugs$last_felled)
bugs$Number_saplings <- as.character(bugs$Number_saplings)
bugs$Mean_DBH <- as.character(bugs$Mean_DBH)
bugs$Standard_deviation_DBH <- as.character(bugs$Standard_deviation_DBH)

# NMDS model ----
bugs.nmds <- metaMDS(bugs[,7:47], distance = "bray",#means bray-curtis distance
                     k = 5)
stressplot(bugs.nmds)

# Plot 1 attempt ----
plot(bugs.nmds)

ordiplot(bugs.nmds, type = "n")
orditorp(bugs.nmds, display = "species", col = "red", air = 0.01)
orditorp(bugs.nmds, display = "sites", cex = 1.25, air = 0.01)

treat= c(rep("last_felled", 5), rep("Number_saplings",5), rep("Mean_DBH",5), rep("Standard_deviation_DBH",5))
ordiplot(bugs.nmds, type = "n")
ordihull(bugs.nmds, groups = treat, draw = "polygon", col = "grey90",
         label = F)
orditorp(bugs.nmds, display = "species", col = "red", air = 0.01)

orditorp(bugs.nmds, display = "sites", col = c(rep("green", 5),
                                               rep("blue", 5)),
         air = 0.01, cex = 1.25)
#ordicluster(bugs.nmds, cluster = treat, display = "sites", col = c(rep("green",5), rep("blue",5)),
            air = 0.01, cex = 1.25)
         
# Plot 2 attempt ----

