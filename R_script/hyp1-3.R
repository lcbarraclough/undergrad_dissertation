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
#  a.invertebrate abundance bar plot ----
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
summary1 <- dplyr :: select(sum_data, Last_felled, Total_abundance)

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

  #Attempt to make a function and add sd to the dataframe ----
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



# b. margalefs or mehinicks richness index bar plots by year ----
#margalef's index first
summary2 <- dplyr :: select(sum_data, year = Last_felled, MRI = Margalefs_RI, MeRI = Menhinicks_RI)


#still need to add standard deviation to this

# sum totals plotted here because I haven't figure out how to calculate mean values with standard deviation
(MRI_sum <- ggplot(summary2, aes(x = year, y = MRI, colour = year,
                                        fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 5)) +
    labs(title = "Sum Margalef's richness index by year", 
         x = "\n Year", y = "Sum Margalef's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(MRI_sum, filename = "Graphs/MRI_sumhyp1.png", width = 5, height = 5)

#menhinicks index next
(MeRI_sum <- ggplot(summary2, aes(x = year, y = MeRI, colour = year,
                                 fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 3)) +
    labs(title = "Sum Mehninicks's richness index by year", 
         x = "\n Year", y = "Sum Mehninick's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(MeRI_sum, filename = "Graphs/MeRI_sumhyp1.png", width = 5, height = 5)


# c. simpsons evenness index bar plot ----

summary3 <- dplyr :: select(sum_data, year = Last_felled, SEI = Simpsons_EI) #%>% 
 # group_by(year) %>% 
  #summarise(SEI.mean = mean(SEI), year = year) %>% 
  #ungroup()

  #plot the sum values
(SEI_sum <- ggplot(summary3, aes(x = year, y = SEI, colour = year,
                                 fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Sum Simpson's Evenness index by year", 
         x = "\n Year", y = "Sum Simpson's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(SEI_sum, file= "Graphs/SEI_sumhyp1.png", width = 5, height = 5)

# d. make these into a panel ----
hyp1.3sum_panel <- grid.arrange(
   abun_bar1 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  MRI_sum + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  MeRI_sum + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  SEI_sum + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1.3sum_panel, file= "Graphs/hyp1_panel3sum.png", width = 10, height = 10)

