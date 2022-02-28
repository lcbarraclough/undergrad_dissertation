#Laura Barraclough
#Started: 25/02/2022
#email: s1729795@ed.ac.uk

# The aim of this script is to use the method from the practice nmds to plot
# the data from our investigation.

# Load packages and dataframe
library(vegan)
library(ggplot2)
library(viridis)

getwd()
bugs2 <- read.csv("nmds2.csv")

# nmds model
bugs2.nmds <- metaMDS(comm = bugs2[ ,7:15],
                        distance = "bray",
                        try = 100)

# plotting the nmds
data_scores2 <- as.data.frame(scores(bugs2.nmds)) #coords for results of model
data_scores2 <- cbind(data_scores2, bugs2[,6])
colnames(data_scores2)[3] <- "year_last_disturbed" 
data_scores2$year_last_disturbed <- as.character(data_scores2$year_last_disturbed)


species_scores2 <- as.data.frame(scores(bugs2.nmds, "species"))

species_scores2$species <- rownames(species_scores2)
# adds column to the row name to create species labels

nmds_plot1 <- (ggplot() +
    geom_text(data = species_scores2, aes(x = NMDS1, y = NMDS2, label = species),
              alpha = 0.5, size = 2) +
    geom_point(data = data_scores2, aes(x= NMDS1, y = NMDS2,
                                       color = year_last_disturbed), size = 3) +
    theme_bw() +
    theme(legend.position = "right",
          text = element_text(size = 12)))
#ggsave(nmds_plot1, file = "Graphs/hyp3-1.png", width = 5, height = 5)
