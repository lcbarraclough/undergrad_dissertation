#Laura Barraclough
#Started: 28/03/2022
#email: s1729795@ed.ac.uk

#This script aims to produce a scatter plot of the stocking density against Margalef's richness 
#index.

#Load library and dataframe
library(tidyverse)
#install.packages("rsq")
library(rsq)

getwd()
bugs3 <- read.csv("sum_data_noants3.csv")
View(bugs3)

bugs3 <- mutate(bugs3, stocking_density = ((Number_stems.plot) + Number_saplings)/25*pi)
bugs3 <- slice(bugs3, 1:12,)
bugs3$years_since_disturbance <- as.character(bugs3$years_since_disturbance)

# Make scatter plot
(hyp4.1 <- ggplot(bugs3, aes(x = stocking_density, y = Menhinicks_RI)) +
                            # color = years_since_disturbance)) +
    scale_fill_manual(values = tree.palette) +
  geom_point() +
  theme_lb() +
  labs(x = "\n Stocking density (stems per m^2)", y = "Menhinick's richness index") +
#    stat_smooth(method = "lm", formula = y ~ x) + 
    geom_text(geom = "text", x = 7, y = 1.5, label = "r-squared = 0.054",
              color = "black") +
    geom_smooth(method = lm))
#ggsave(hyp4.1, file = "Graphs/hyp4-1.png", width = 5, height = 5)

lm1 <- lm(Menhinicks_RI ~ stocking_density, data = bugs3)
rsq(lm1)

(hyp4.2 <- ggplot(bugs3, aes(x = stocking_density, y = Margalefs_RI)) +
    # color = years_since_disturbance)) +
    scale_fill_manual(values = tree.palette) +
    geom_point() +
    theme_lb() +
    labs(x = "\n Stocking density (stems per m^2)", y = "Margalef's richness index") +
    #    stat_smooth(method = "lm", formula = y ~ x) + 
    geom_text(geom = "text", x = 7, y = 1.5, label = "r-squared = 0.0248",
              color = "black") +
    geom_smooth(method = lm))
#ggsave(hyp4.2, file = "Graphs/hyp4-2.png", width = 5, height = 5)

lm2 <- lm(Margalefs_RI ~ stocking_density, data = bugs3)
rsq(lm2)
