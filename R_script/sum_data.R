#Laura Barraclough
#Started: 24/01/2022
#email: s1729795@ed.ac.uk

# Work flow
# 1. Load libraries and data frame
# 2. Make colour scheme
# 3. Make scatter plot for Hypothesis 1
#   a. invertebrate abundance vs. mean dbh
#   b. margalefs or mehinicks richness index vs. mean dbh
#   c. simpsons evenness index vs. mean dbh
#   d. make these into a panel 

  #1. Load libraries ----
library(tidyverse)
library(gridExtra)
#install.packages("gridExtra")

getwd()

  #Load csv file
sum_data <- read.csv("sum_data2.csv")

  #Check the data has loaded properly
head(sum_data)
tail(sum_data)
str(sum_data)
  #looks promising but last felled needs to be a character and not an integer
sum_data$Last_felled <- as.character(sum_data$Last_felled)

  #2. Make colour scheme ----
#install.packages("colourpicker")  #allows us to choose nice colours
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbbPalette)

  #3. Attempt to make a scatter plot ----
  #  a.invertebrate abundance vs. mean dbh
(abun_vs_mean <- ggplot(sum_data, aes(x = Mean_DBH, y = Total_abundance, colour = Last_felled)) +
   geom_point(size = 2) +
   theme_bw() + 
   scale_fill_manual(values=cbbPalette) +
   ylab("Total abundance\n") +
   xlab("\nMean DBH (cm)"))
  
#ggsave(abun_vs_mean, file= "Graphs/abun_vs_mean.png")
  
  # b. margalefs or mehinicks richness index vs. mean dbh
  #margalef's index first
(margI_vs_mean <- ggplot(sum_data, aes(x = Mean_DBH, y = Margalefs_RI, colour = Last_felled)) +
    geom_point(size = 2) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Margalef's Richness Index\n") +
    xlab("\nMean DBH (cm)"))

#ggsave(margI_vs_mean, file= "Graphs/margI_vs_mean.png")

  #menhinicks index next
(menhiI_vs_mean <- ggplot(sum_data, aes(x = Mean_DBH, y = Menhinicks_RI , colour = Last_felled)) +
    geom_point(size = 2) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Mehinick's Richness Index\n") +
    xlab("\nMean DBH (cm)"))

#ggsave(menhiI_vs_mean, file= "Graphs/menhiI_vs_mean.png")

  # c. simpsons evenness index vs. mean dbh
(simpsI_vs_mean <- ggplot(sum_data, aes(x = Mean_DBH, y = Simpsons_EI , colour = Last_felled)) +
    geom_point(size = 2) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Simpson's Evenness Index\n") +
    xlab("\nMean DBH (cm)"))

#ggsave(simpsI_vs_mean, file= "Graphs/simpsI_vs_mean.png")

  # d. make these into a panel 
hyp1_panel <- grid.arrange(
  abun_vs_mean + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  margI_vs_mean + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  menhiI_vs_mean + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  simpsI_vs_mean + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1_panel, file= "Graphs/hyp1_panel.png")
