#Laura Barraclough
#Started: 02/02/2022
#email: s1729795@ed.ac.uk

# Work Flow
# 1. Load libraries and data frame
# 2. Try to make a summary table with mean values of the calculated
#     richness and evenness indices as well as abundance.
#   (i) try using filter in a pipe


# 1. Load libraries and data frame ----
library(tidyverse)

sum_data <- read.csv("sum_data2.csv")
  #check data frame loaded properly
View(sum_data)
str(sum_data)
  #last felled needs to be a character
sum_data$Last_felled <- as.character(sum_data$Last_felled)

# 2. Summary table attempts ----
#   (i) Mean abundance ----
summary1 <- dplyr :: select(sum_data, year = Last_felled, abun = Total_abundance)

mean_table1 <- group_by(summary1, year) %>% 
  summarise(abun.mean = mean(abun)) %>% 
  ungroup() 

  #plotting mean_table1 as a bar plot
(mean.abun_bar2 <- ggplot(mean_table1, aes(x = year, y = abun.mean, colour = year,
                                        fill = year)) +
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

ggsave(mean.abun_bar2, file = "Graphs/mean_abunhyp1.png", height = 5, width = 5)

#   (ii) Margalef's index ----
summary2 <- dplyr :: select(sum_data, year = Last_felled, MRI = Margalefs_RI)

mean_table2 <- group_by(summary2, year) %>% 
  summarise(MRI_mean = mean(MRI)) %>% 
  ungroup() 

#plotting mean_table1 as a bar plot
(mean.abun_bar2 <- ggplot(mean_table2, aes(x = year, y = MRI_mean, colour = year,
                                           fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 4)) +
    labs(title = "Mean Margalef's richness index by year", 
         x = "\n Year", y = "Mean Margalef's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(mean.abun_bar2, file = "Graphs/mean_MRIhyp1.png", height = 5, width = 5)

#   (iii) Menhinick's index ----
summary3 <- dplyr :: select(sum_data, year = Last_felled, MeRI = Menhinicks_RI)

mean_table3 <- group_by(summary3, year) %>% 
  summarise(MeRI_mean = mean(MeRI)) %>% 
  ungroup() 

#plotting mean_table1 as a bar plot
(mean_MeRI_bar <- ggplot(mean_table3, aes(x = year, y = MeRI_mean, colour = year,
                                           fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 3)) +
    labs(title = "Mean Menhinick's richness index by year", 
         x = "\n Year", y = "Mean Menhinick's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(mean_MeRI_bar, file = "Graphs/mean_MeRIhyp1.png", height = 5, width = 5)

#   (vi) Simpson's index ----
summary3 <- dplyr :: select(sum_data, year = Last_felled, MeRI = Menhinicks_RI)

mean_table3 <- group_by(summary3, year) %>% 
  summarise(MeRI_mean = mean(MeRI)) %>% 
  ungroup() 

#plotting mean_table1 as a bar plot
(mean_MeRI_bar <- ggplot(mean_table3, aes(x = year, y = MeRI_mean, colour = year,
                                          fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 3)) +
    labs(title = "Mean Menhinick's richness index by year", 
         x = "\n Year", y = "Mean Menhinick's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

#ggsave(mean_MeRI_bar, file = "Graphs/mean_MeRIhyp1.png", height = 5, width = 5)

