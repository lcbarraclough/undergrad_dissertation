#Laura Barraclough
#Started: 31/01/2022
#email: s1729795@ed.ac.uk

# Work Flow
# 1. Load libraries and data frame
# 2. Test hypothesis 2: plots with larger DBH will have fewer saplings.
#   (a) Try plotting as a scatter plot
#   (b) Try plotting as a bar plot


  # 1. Load libraries and data frame ----
library(tidyverse)
library(gridExtra)

getwd() #check the working directory 

  #Load data frame
sum_data <- read.csv("sum_data2.csv")

head(sum_data)
tail(sum_data)
str(sum_data)
  #Check data has loaded properly
  #Last felled is an integer not a character so we need to change this
sum_data$Last_felled <- as.character(sum_data$Last_felled)
  #check this worked
str(sum_data) #it has so we can move on to testing hypothesis 2.

  # 2. (a) Try plotting as a scatter plot ----
  #plot Mean DBH measurements against sapling count
(scat_2.1 <- ggplot(sum_data, aes(x = Mean_DBH, y = Sapling_count,
                                  colour = Last_felled)) +
    geom_point() +
    theme_bw() +
   ylab("Sapling count\n") +
   xlab("\nMean DBH (cm)")+
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
         legend.text = element_text(size = 10, face = "italic"),
         legend.title = element_blank()))

#ggsave(scat_2.1, file = "Graphs/hyp2-1.png", width = 5, height = 5)

  #plot SD of DBH measurements against sapling counts
(scat_2.2 <- ggplot(sum_data, aes(x = SD_DBH, y = Sapling_count,
                                  colour = Last_felled)) +
    geom_point() +
    theme_bw() +
    ylab("Sapling count\n") +
    xlab("\nStandard deviation of DBH measurements (cm)")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.title = element_blank()))

#ggsave(scat_2.2, file = "Graphs/hyp2-2.png", width = 5, height = 5)

#     (b) Try plotting as a bar plot ----
# I tried to do the data manipulation in R but it wasn't working so I did it in excel

tree_data <- read.csv("Tree_data.csv")  #this data frame includes the mean values of the DBH measuremenst collected

tree_data$Year <- as.Date.factor(tree_data$Year)

(hist2.1 <- ggplot(tree_data, aes(x = Year, y = Mean_sapl_count, colour = Year, fill = Year)) +
   geom_histogram(stat = "identity", position = "dodge") + 
   scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
   scale_y_continuous(limits = c(0, 50)) +
   labs(title = "Sapling numbers by plot", 
        x = "\n Year", y = "Mean number of samplings per plot\n") + 
   theme_bw() +
   theme(panel.grid = element_blank(), 
         axis.text = element_text(size = 12), 
         axis.title = element_text(size = 12), 
         plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
 )
