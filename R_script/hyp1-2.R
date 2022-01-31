#Laura Barraclough
#Started: 27/01/2022
#email: s1729795@ed.ac.uk

#This script aims to eliminate the problems asscociated with the DBH<10cm which are
#currently showing as 0cm and this isn't true. In this script I will make a copy
#of the data and eliminate the measurements with DBH<10cm.

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

#Make copy of csv data file
sum_data2<- sum_data

#Check the data has loaded properly
head(sum2_data)
tail(sum2_data)
str(sum2_data)
#looks promising but last felled needs to be a character and not an integer
sum_data2$Last_felled <- as.character(sum_data$Last_felled)

#Now we will delete all the rows with DBH=0
sum_data2 <- sum_data2[- c(9, 10, 11, 12),]
#I've removed all the observations from the 3rd forest stand (3A, 3B, 3C, 3D)


#2. Make colour scheme ----
#install.packages("colourpicker")  #allows us to choose nice colours
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbbPalette)

#3. Attempt to make a scatter plot ----
#  a.invertebrate abundance vs. mean dbh
(abun_vs_mean.2 <- ggplot(sum_data2, aes(x = Mean_DBH, y = Total_abundance, colour = Last_felled)) +
   geom_point(size = 3) +
   theme_bw() + 
   scale_fill_manual(values=cbbPalette) +
   ylab("Total abundance\n") +
   xlab("\nMean DBH (cm)") +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
         legend.text = element_text(size = 10, face = "italic"),
         legend.title = element_blank()))

#ggsave(abun_vs_mean.2, file= "Graphs/abun_vs_mean2.png",width = 5, height = 5)

# b. margalefs or mehinicks richness index vs. mean dbh
#margalef's index first
(margI_vs_mean.2 <- ggplot(sum_data2, aes(x = Mean_DBH, y = Margalefs_RI, colour = Last_felled)) +
    geom_point(size = 3) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Margalef's Richness Index\n") +
    xlab("\nMean DBH (cm)") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.title = element_blank()))

#ggsave(margI_vs_mean.2, file= "Graphs/margI_vs_mean2.png", width = 5, height = 5)

#menhinicks index next
(menhiI_vs_mean.2 <- ggplot(sum_data2, aes(x = Mean_DBH, y = Menhinicks_RI , colour = Last_felled)) +
    geom_point(size = 3) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Mehinick's Richness Index\n") +
    xlab("\nMean DBH (cm)")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.title = element_blank()))

#ggsave(menhiI_vs_mean.2, file= "Graphs/menhiI_vs_mean2.png", width = 5, height = 5)

# c. simpsons evenness index vs. mean dbh
(simpsI_vs_mean.2 <- ggplot(sum_data2, aes(x = Mean_DBH, y = Simpsons_EI , colour = Last_felled)) +
    geom_point(size = 3) +
    theme_bw() + 
    scale_fill_manual(values=cbbPalette) +
    ylab("Simpson's Evenness Index\n") +
    xlab("\nMean DBH (cm)")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.title = element_blank()))

#ggsave(simpsI_vs_mean.2, file= "Graphs/simpsI_vs_mean2.png", width = 5, height = 5)

# d. make these into a panel 
hyp1.2_panel <- grid.arrange(
  abun_vs_mean.2 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  margI_vs_mean.2 + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  menhiI_vs_mean.2 + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  simpsI_vs_mean.2 + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1.2_panel, file= "Graphs/hyp1_panel2.png", width = 10, height = 10)
