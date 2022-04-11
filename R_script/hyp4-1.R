#Laura Barraclough
#Started: 28/03/2022
#email: s1729795@ed.ac.uk

#This script aims to produce a scatter plot of the stocking density against the various species 
#richness indices calculated as part of hyp1. 
#index.

#Load library and dataframe
library(tidyverse)
#install.packages("rsq")
library(rsq)
library(gridExtra)

getwd()
setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation/species_indices")
bugs3 <- read.csv("sum_data_noants3.csv")
View(bugs3)

bugs3 <- mutate(bugs3, stocking_density = ((Number_stems.plot) + Number_saplings)/25*pi)
bugs3 <- slice(bugs3, 1:12,)
bugs3$years_since_disturbance <- as.character(bugs3$years_since_disturbance)

tree.palette <- c("#f7fcb9","#addd8e", "#31a354")

theme_lb <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = ,"cm"),
          legend.position = "none")
}


# Make scatter plot
(hyp4.1 <- ggplot(bugs3, aes(x = stocking_density, y = Menhinicks_RI)) +
    theme_lb() +
  geom_point(colour = c("#008B00")) +
  labs(y = "Menhinick's richness index",
       x = parse(text = "Stocking~density(stems~per~meter^2)")) +
  stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
              fill = c("#addd8e")) + 
  geom_text(geom = "text", x = 6, y = 1.5, label = "r-squared = 0.054",
              color = "black"))

ggsave(hyp4.1, file = "hyp4-1.png", width = 5, height = 5)

lm1 <- lm(Menhinicks_RI ~ stocking_density, data = bugs3)
rsq(lm1)

(hyp4.2 <- ggplot(bugs3, aes(x = stocking_density, y = Margalefs_RI)) +
    # color = years_since_disturbance)) +
    geom_point(colour = c("#008B00")) +
    theme_lb() +
    labs(x = parse(text = "Stocking~density(stems~per~meter^2)"),
         y = "Margalef's richness index") +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                fill = c("#addd8e")) + 
    geom_text(geom = "text", x = 6, y = 1.5, label = "r-squared = 0.0248",
              color = "black"))
ggsave(hyp4.2, file = "hyp4-2.png", width = 5, height = 5)

lm2 <- lm(Margalefs_RI ~ stocking_density, data = bugs3)
rsq(lm2)

(hyp4.3 <- ggplot(bugs3, aes(x= stocking_density, y = Abun_per_area)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    labs(x = parse(text = "Stocking~density(stems~per~meter^2)"),
         y = parse(text = "Abundance~per~m^2")) +
    stat_smooth(method = "lm", formula = y ~ x,  colour = c("#f7fcb9"),
                fill = c("#addd8e")) + 
    geom_text(geom = "text", x = 6, y = 0, label = "r-squared = 0.0014",
              color = "black"))
ggsave(hyp4.3, file = "hyp4-3.png", width = 5, height = 5)

lm3 <- lm(Abun_per_area ~ stocking_density, data = bugs3)
rsq(lm3)

(hyp4.4 <- ggplot(bugs3, aes(x= stocking_density, y = Simpsons_EI)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    labs(x = parse(text = "Stocking~density(stems~per~meter^2)"),
         y = "Simpson's richness index") +
    stat_smooth(method = "lm", formula = y ~ x,  colour = c("#f7fcb9"),
                fill = c("#addd8e")) + 
    geom_text(geom = "text", x = 6, y = 0.5, label = "r-squared = 0.048",
              color = "black"))


ggsave(hyp4.4, file = "hyp4-4.png", width = 5, height = 5)

lm4 <- lm(Simpsons_EI ~ stocking_density, data = bugs3)
rsq(lm4)

# Panel construction
hyp4_panel <- grid.arrange(
  hyp4.3 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  hyp4.1 + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  hyp4.2 + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  hyp4.4 + ggtitle ("(d)") +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

ggsave(hyp4_panel, file = "hyp4_panel.png", width = 10, height = 5)
