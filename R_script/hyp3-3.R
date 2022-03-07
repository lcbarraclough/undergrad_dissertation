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
colnames(data_scores2)[3] <- "years_since_disturbance" 
data_scores2$years_since_disturbance <- as.character(data_scores2$years_since_disturbance)


species_scores2 <- as.data.frame(scores(bugs2.nmds, "species"))

species_scores2$species <- rownames(species_scores2)
# adds column to the row name to create species labels

#theme
theme_lb <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = ,"cm"),
          legend.position = "none")
}

#custom colour palette
tree.palette <- c("#f7fcb9","#addd8e", "#31a354")
names(tree.palette) <- levels(data_scores2$years_since_disturbance)

(nmds_plot1 <- ggplot() +
    geom_text(data = species_scores2,
              aes(x = NMDS1, y = NMDS2, alpha = 0.5, size = 2, label = species),
              alpha = 0.5) +
    geom_point(data = data_scores2,
               aes(x= NMDS1, y = NMDS2, color = years_since_disturbance, size = 2)) +
    theme_lb() +
    scale_x_continuous(limits = c(-1, 1.6)) +
    scale_colour_manual(values = tree.palette))

#ggsave(nmds_plot1, file = "Graphs/hyp3-1.png", width = 5, height = 5)



(nmds_plot2 <-  ggplot() +
    geom_text(data = species_scores2,
              aes(x = NMDS1, y = NMDS2, alpha = 0.5, size = 2, label = species),
              alpha = 0.5) +
    geom_point(data = data_scores2,
               aes(x= NMDS1, y = NMDS2, color = years_since_disturbance, size = 2)) +
    geom_polygon(data = data_scores2,
                 aes(x = NMDS1, y = NMDS2, fill = years_since_disturbance,
                     group = years_since_disturbance), alpha = 0.5) +
    theme_lb() +
    scale_x_continuous(limits = c(-1, 1.6)) +
    scale_colour_manual(values = tree.palette))

#ggsave(nmds_plot2, file = "Graphs/hyp3-2.png", width = 5, height = 5)
