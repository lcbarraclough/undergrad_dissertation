#Laura Barraclough
#Started: 22/02/2022
#email: s1729795@ed.ac.uk

# Load libraries and data frame
bugs <- read.csv("nmds.csv")

library(tidyverse)
library(vegan)

glimpse(bugs)
#last felled should be a character 
bugs$last_felled <- as.character(bugs$last_felled)

bugs.nmds <- metaMDS(bugs[,3:43], distance = "bray", k = 5)
stressplot(bugs.nmds)

plot(bugs.nmds)

ordiplot(bugs.nmds, type = "n")
orditorp(bugs.nmds, display = "species", col = "red", air = 0.01)
orditorp(bugs.nmds, display = "sites", cex = 1.25, air = 0.01)

treat= c(rep("1988", 5), rep("1998",5), rep("2008",5))
ordiplot(bugs.nmds, type = "n")
ordihull(bugs.nmds, groups = treat, draw = "polygon", col = "grey90",
         label = F)
orditorp(bugs.nmds, display = "species", col = "red", air = 0.01)
orditorp(bugs.nmds, display = "sites", col = c(rep("green", 5),
                                               rep("blue", 5)),
         air = 0.01, cex = 1.25)
ordicluster(bugs.nmds, cluster = treat, display = "sites", col = c(rep("green",5), rep("blue",5)),
            air = 0.01, cex = 1.25)
         