#Laura Barraclough
#Started: 01/02/2022
#email: s1729795@ed.ac.uk

#This script aims to make a bar plot to answer hypothesis 1
# Work flow

# 1. Load libraries and data frame
# 2. Make colour scheme
# 3. Make bar plot for Hypothesis 1
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

View(sum_data)


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

#3. Attempt to make a bar plot ----
#  a.invertebrate abundance bar plot
# (i) original values
(abun_bar1 <- ggplot(sum_data, aes(x = Last_felled, y = Total_abundance, colour = Last_felled,
                      fill = Last_felled)) +
   geom_histogram(stat = "identity", position = "dodge") + 
   scale_y_continuous(limits = c(0, 150)) +
   labs(title = "Invertebrate abundance by year", 
        x = "\n Year", y = "Invertebrate abundance\n") + 
   theme_bw() +
   theme(panel.grid = element_blank(), 
         axis.text = element_text(size = 12), 
         axis.title = element_text(size = 12), 
         plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(abun_bar1, file= "Graphs/abun_bar1.png",width = 5, height = 5)

# (ii) mean values
summary1 <- dplyr :: select(sum_data, Last_felled, Total_abundance) %>% 
  group_by( ., Last_felled) %>% 
  summarise( ., mean.abun = mean(Total_abundance)) %>% 
  mutate( ., standard.dev = sd(mean.abun)) %>% 
  ungroup()

  #still need to add standard deviation to this
  
  # plotting this 
  (mean.abun_bar1 <- ggplot(summary1, aes(x = Last_felled, y = mean.abun, colour = Last_felled,
                                     fill = Last_felled)) +
     geom_histogram(stat = "identity", position = "dodge") + 
     scale_y_continuous(limits = c(0, 80)) +
     labs(title = "Mean invertebrate abundance by year", 
          x = "\n Year", y = "Mean invertebrate abundance\n") + 
     theme_bw() +
     theme(panel.grid = element_blank(), 
           axis.text = element_text(size = 12), 
           axis.title = element_text(size = 12), 
           plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
  )
#ggsave(mean.abun_bar1, filename = "Graphs/hyp1-3abun.png", width = 5, height = 5)
#need to add error bars 

  #Attempt to make a function and add sd to the dataframe
#install.packages("plyr")
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

summary2 <- dplyr :: select(sum_data, Last_felled, Total_abundance) %>% 
  group_by( ., Last_felled) %>% 
  data_summary( ., varname = "mean.abun", groupnames = "Last_felled")



# b. margalefs or mehinicks richness index vs. mean dbh
#margalef's index first


#menhinicks index next


#ggsave(menhiI_vs_mean.2, file= "Graphs/menhiI_vs_mean2.png", width = 5, height = 5)

# c. simpsons evenness index vs. mean dbh


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