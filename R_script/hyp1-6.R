#Laura Barraclough
#Started: 15/02/2022
#email: s1729795@ed.ac.uk

#The aim of this script is to add data labels with the results of the ANOVA tests.

# Work Flow
# 1. Load libraries and data frame
# 2. Plot various bar charts without ant numbers
#   (i) Abundance
#   (ii) Margalef's index
#   (iii) Menhinick's index
#   (iv) Simpson's index
# 3. Make these into a panel


# 1. Load libraries and data frame ----
library(tidyverse)
library(gridExtra)

getwd()
setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation")
sum_data2 <- read.csv("sum_data_noants.csv")
#check data frame loaded properly
View(sum_data2)
str(sum_data2)

#last felled needs to be a character
sum_data2$Last_felled <- as.character(sum_data2$Last_felled)
sum_data2 <- sum_data2 %>% 
  na.omit()

# 2. Summary table attempts ----
#   (i) Mean abundance ----
summary1.2 <- dplyr :: select(sum_data2, year = Last_felled, abun = Total_abundance)

mean_table1.2 <- group_by(summary1.2, year) %>% 
  summarise(abun.mean = mean(abun)) %>% 
  ungroup() %>% 
  na.omit()

#attempt to add standard deviation
mean_table1.2 <- mean_table1.2 %>% 
  mutate(mean_table1.2, sd = c(18.84, 14.94, 24.25)) #entering sd values manually

#plotting mean_table1 as a bar plot
(mean.abun_bar2 <- ggplot(mean_table1.2, aes(x = year, y = abun.mean, colour = year,
                                             fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = "Mean invertebrate abundance by year", 
         x = "\n Year", y = "Mean invertebrate abundance\n",
         caption = "\nP>0.01, F=3.114, DF=2") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = abun.mean-sd, ymax = abun.mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean.abun_bar2, file = "Graphs/mean_abunhyp1-2.png", height = 5, width = 5)

#   (ii) Margalef's index ----
summary2.2 <- dplyr :: select(sum_data2, year = Last_felled, MRI = Margalefs_RI)

mean_table2.2 <- group_by(summary2.2, year) %>% 
  summarise(MRI_mean = mean(MRI)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate( ., sd = c(0.19, 0.28, 0)) #there is a very large sd associated with 2008 


#plotting mean_table1 as a bar plot
(mean_MRI_bar.2 <- ggplot(mean_table2.2, aes(x = year, y = MRI_mean, colour = year,
                                             fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 2)) +
    labs(title = "Mean Margalef's richness index by year", 
         x = "\n Year", y = "Mean Margalef's richness index\n",
         caption = "\nP<0.01, F=14.16, DF=2") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = MRI_mean-sd, ymax = MRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_MRI_bar.2, file = "Graphs/mean_MRIhyp1-2.png", height = 5, width = 5)

#   (iii) Menhinick's index ----
summary3.2 <- dplyr :: select(sum_data2, year = Last_felled, MeRI = Menhinicks_RI)

mean_table3.2 <- group_by(summary3.2, year) %>% 
  summarise(MeRI_mean = mean(MeRI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.17, 0.12, 0.09))

#plotting mean_table1 as a bar plot
(mean_MeRI_bar.2 <- ggplot(mean_table3.2, aes(x = year, y = MeRI_mean, colour = year,
                                              fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1.2)) +
    labs(title = "Mean Menhinick's richness index by year", 
         x = "\n Year", y = "Mean Menhinick's richness index\n",
         caption = "\nP>0.01, F=14.16, DF=2") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = MeRI_mean-sd, ymax = MeRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_MeRI_bar.2, file = "Graphs/mean_MeRIhyp1-2.png", height = 5, width = 5)

#   (vi) Simpson's index ----
summary4.2 <- dplyr :: select(sum_data2, year = Last_felled, SEI = Simpsons_EI)

mean_table4.2 <- group_by(summary4.2, year) %>% 
  summarise(SEI_mean = mean(SEI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.04, 0.1, 0.15))

#plotting mean_table1 as a bar plot
(mean_SEI_bar.2 <- ggplot(mean_table4.2, aes(x = year, y = SEI_mean, colour = year,
                                             fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Mean Simpson's evenness index by year", 
         x = "\n Year", y = "Mean Simpson's evenness index\n",
         caption = "\nP>0.1, F=1.126, DF=2") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    geom_errorbar(aes(x = year, ymin = SEI_mean-sd, ymax = SEI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_SEI_bar.2, file = "Graphs/mean_SEIIhyp1-2.png", height = 5, width = 5)


# 3. Panel construction ----
hyp1.4sum_panel <- grid.arrange(
  mean.abun_bar2 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MRI_bar.2 + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MeRI_bar.2 + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_SEI_bar.2 + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1.4sum_panel, file= "Graphs/hyp1_panel5sum.png", width = 10, height = 10)
