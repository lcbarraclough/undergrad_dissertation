#Laura Barraclough
#Started: 15/02/2022
#email: s1729795@ed.ac.uk

# the aim of this script is to practice nmds and plotting this using
# a tutorial: https://cougrstats.wordpress.com/2019/12/11/non-metric-multidimensional-scaling-nmds-in-r/

# Load packages and dataframe
library(vegan)
library(ggplot2)
library(viridis)

getwd()
orders <- read.csv("condensed_order.csv")

# nmds model
nmds_results <- metaMDS(comm = orders[ ,4:11],
                        distance = "bray",
                        try = 100)

# plotting the nmds
data_scores <- as.data.frame(scores(nmds_results)) #coords for results of model
data_scores <- cbind(data_scores, orders[,14])
colnames(data_scores)[3] <- "aquaticSiteType" #add the extra aquaticSiteType column

species_scores <- as.data.frame(scores(nmds_results, "species"))
# add the scores for species data

species_scores$species <- rownames(species_scores)
# adds column to the row name to create species labels

# buiding the plot
(ggplot() +
    geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
              alpha = 0.5, size = 10) +
    geom_point(data = data_scores, aes(x= NMDS1, y = NMDS2,
                                      color = aquaticSiteType), size = 3) +
    scale_color_manual(values = inferno(15)[c(3, 8, 11)],
                       name = "Aquatic System Type") +
    annotate(geom = "label", x = -1, y = 1.25, size = 10,
             label = paste("Stress: ", round(nmds_results$stress, digits = 3))) +
    theme_minimal() +
    theme(legend.position = "right",
          text = element_text(size = 14)))
  
  