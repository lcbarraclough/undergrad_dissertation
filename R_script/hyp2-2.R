#Laura Barraclough
#Started: 10/02/2022
#email: s1729795@ed.ac.uk

# Work Flow
# 1. Load libraries and data frame
# 2. Test hypothesis 2: older plots will have fewer saplings.
#   (a) Try plotting as a scatter plot
#   (b) Try as a box plot


# 1. Load libraries and data frame ----
library(tidyverse)
library(gridExtra)

getwd() #check the working directory 

#Load data frame
sum_data <- read.csv("sum_data2.csv")

head(sum_data)
tail(sum_data)
str(sum_data)
View(sum_data)
#Check data has loaded properly
#Last felled is an integer not a character so we need to change this
sum_data$Last_felled <- as.character(sum_data$Last_felled)
#check this worked
str(sum_data) #it has so we can move on to testing hypothesis 2.

# 2. Plotting Hypothesis 2 as a scatter plot ----
(hyp2.2 <- ggplot(sum_data, aes(x = Last_felled, y = Sapling_count, colour = Last_felled)) +
   geom_point(size = 3) +
   theme_bw() + 
   ylab("Sapling count per plot\n") +
   xlab("\nYear last felled") +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
         legend.text = element_text(size = 10, face = "italic"),
         legend.title = element_blank()) +
   geom_smooth(method = "lm") #this isn't working- i think i need to add y~x
 )

#ggsave(hyp2.2, file= "Graphs/hyp2-2.png", width = 5, height = 5)

# 3. Plotting hypothesis 2 as a box plot ----
(hyp2.3 <- ggplot(sum_data, aes(x = Last_felled, y = Sapling_count, colour = Last_felled,
                                fill = Last_felled)) +
   geom_boxplot() +
   theme_bw() +
   ylab("Sapling count per plot\n") +
   xlab("\nYear last felled") +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
         legend.text = element_text(size = 10, face = "italic"),
         legend.title = element_blank())
 )
#ggsave(hyp2.3, file = "Graphs/hyp2-3.png", width = 5, height = 5)
