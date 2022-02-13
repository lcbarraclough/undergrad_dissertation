#Laura Barraclough
#Started: 10/02/2022
#email: s1729795@ed.ac.uk

#The aim of this script is to work through the code required for NMDS graphs
#The website I will be using is:https://jkzorz.github.io/2019/06/06/NMDS.html

# 1. Install packages and load data frame ----
#install.packages("vegan")
library(vegan)
library(dplyr)
library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)

data("dune")
data("dune.env")

# 2. Data wrangling ----
dune.mds <- metaMDS(dune, distance = "bray", autotransform = F)
dune.envfit <- envfit(dune.mds, dune.env, permutations = 999) # this fits environmental vectors
dune.spp.fit <- envfit(dune.mds, dune, permutations = 999) # this fits species vector

site.scrs <- as.data.frame(scores(dune.mds, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Management = dune.env$Management) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Landuse = dune.env$Use) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

spp.scrs <- as.data.frame(scores(dune.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = dune.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)


env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.dune <- cbind(env.scores.dune, env.variables = rownames(env.scores.dune)) #and then gives them their names

env.scores.dune <- cbind(env.scores.dune, pval = dune.envfit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores.dune, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores.dune)


(nmds.plot.dune <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Management), shape = factor(site.scrs$Landuse)), size = 2)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Management", shape = "Landuse")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10))) # add legend at right of plot

nmds.plot.dune + labs(title = "Basic ordination plot") #displays plot
#ggsave(nmds.plot.dune, file = "Graphs/nmds_practice.png", width = 5, height = 5)


(nmds.plot.dune+
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with species vectors"))
