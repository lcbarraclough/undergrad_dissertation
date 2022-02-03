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
library(gridExtra)
#install.packages("plotrix")
library(plotrix)

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

  #attempt to add standard deviation
mean_table1 <- mean_table1 %>% 
  mutate(mean_table1, sd = c(8.1, 58.1, 3.0)) #entering sd values manually

  #plotting mean_table1 as a bar plot
(mean.abun_bar2 <- ggplot(mean_table1, aes(x = year, y = abun.mean, colour = year,
                                        fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 150)) +
    labs(title = "Mean invertebrate abundance by year", 
         x = "\n Year", y = "Mean invertebrate abundance\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = abun.mean-sd, ymax = abun.mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean.abun_bar2, file = "Graphs/mean_abunhyp1.png", height = 5, width = 5)

#   (ii) Margalef's index ----
summary2 <- dplyr :: select(sum_data, year = Last_felled, MRI = Margalefs_RI)

mean_table2 <- group_by(summary2, year) %>% 
  summarise(MRI_mean = mean(MRI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.27, 0.48, 0.77))


#plotting mean_table1 as a bar plot
(mean_MRI_bar <- ggplot(mean_table2, aes(x = year, y = MRI_mean, colour = year,
                                           fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 5)) +
    labs(title = "Mean Margalef's richness index by year", 
         x = "\n Year", y = "Mean Margalef's richness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = MRI_mean-sd, ymax = MRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_MRI_bar, file = "Graphs/mean_MRIhyp1.png", height = 5, width = 5)

#   (iii) Menhinick's index ----
summary3 <- dplyr :: select(sum_data, year = Last_felled, MeRI = Menhinicks_RI)

mean_table3 <- group_by(summary3, year) %>% 
  summarise(MeRI_mean = mean(MeRI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.23, 0.51, 0.55))

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
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = MeRI_mean-sd, ymax = MeRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_MeRI_bar, file = "Graphs/mean_MeRIhyp1.png", height = 5, width = 5)

#   (vi) Simpson's index ----
summary4 <- dplyr :: select(sum_data, year = Last_felled, SEI = Simpsons_EI)

mean_table4 <- group_by(summary4, year) %>% 
  summarise(SEI_mean = mean(SEI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.09, 0.19, 0.16))

#plotting mean_table1 as a bar plot
(mean_SEI_bar <- ggplot(mean_table4, aes(x = year, y = SEI_mean, colour = year,
                                          fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Mean Simpson's evenness index by year", 
         x = "\n Year", y = "Mean Simpson's evenness index\n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = SEI_mean-sd, ymax = SEI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_SEI_bar, file = "Graphs/mean_SEIIhyp1.png", height = 5, width = 5)


# 3. Panel construction ----
hyp1.4sum_panel <- grid.arrange(
  mean.abun_bar2 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MRI_bar + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MeRI_bar + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_SEI_bar + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1.4sum_panel, file= "Graphs/hyp1_panel4sum.png", width = 10, height = 10)
