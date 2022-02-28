#Laura Barraclough
#Started: 15/02/2022
#email: s1729795@ed.ac.uk

#The aim of this script is to change the year axis to number of years since last disturbance.
#I will also try to make the script more efficient by writing the common code as a theme.

# Work Flow
# 1. (i) Load libraries and data frame
#    (ii) Save common code as a theme
# 2. Plot various bar charts without ant numbers
#   (i) Abundance
#   (ii) Margalef's index
#   (iii) Menhinick's index
#   (iv) Simpson's index
# 3. Make these into a panel

# 1. (i) Load libraries and data frame ----
library(tidyverse)
library(gridExtra)

getwd()
setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation")
sum_data2 <- read.csv("sum_data_noants3.csv")
#check data frame loaded properly
View(sum_data2)
str(sum_data2)

#years since felled needs to be a character
sum_data2$years_since_disturbance <- as.character(sum_data2$years_since_disturbance)
sum_data2 <- sum_data2 %>% 
  na.omit()

#   (ii) Create theme function to reduce script length
theme_lb <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = ,"cm"),
          legend.position = "none")
}

# 2. Plot various bar charts without ant numbers ----
#   (i) Abundance
summary1.2 <- dplyr :: select(sum_data2, year = years_since_disturbance, abun = Abun_per_area)

mean_table1.2 <- group_by(summary1.2, year) %>% 
  summarise(abun.mean = mean(abun)) %>% 
  ungroup() %>% 
  na.omit()

mean_table1.2 <- mean_table1.2 %>% 
  mutate(mean_table1.2, sd = c(0.31, 0.19, 0.24))

(mean.abun_bar3 <- ggplot(mean_table1.2, aes(x = year, y = abun.mean, colour = year,
                                             fill = year)) +
   geom_histogram(stat = "identity", position = "dodge") + 
   scale_y_continuous(limits = c(0, 2)) +
   labs(x = "\n Number of years since disturbance", y = "Mean invertebrate abundance\n") +
    theme_lb() +
    geom_errorbar(aes(x = year, ymin = abun.mean-sd, ymax = abun.mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean.abun_bar3, file = "Graphs/abun_bar3.png", width = 5, height = 5)

#   (ii) Margalef's index
summary2.2 <- dplyr :: select(sum_data2, year = years_since_disturbance, MRI = Margalefs_RI)

mean_table2.2 <- group_by(summary2.2, year) %>% 
  summarise(MRI_mean = mean(MRI)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate( ., sd = c(0, 0.14, 0.09))

(mean_MRI_bar.3 <- ggplot(mean_table2.2, aes(x = year, y = MRI_mean, colour = year,
                                             fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 2)) +
    labs(x = "\n Number of years since disturbance", y = "Mean Margalef's richness index\n") + 
    theme_lb() +
    geom_errorbar(aes(x = year, ymin = MRI_mean-sd, ymax = MRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9) +
    annotate("text", x  = 1, y = 0.4, label = "A", size = 6) +
    annotate("text", x = 2, y = 1.2, label = "B", size = 6) +
    annotate("text", x = 3, y = 1.9, label = "B", size = 6)
)

#ggsave(mean_MRI_bar.3, file = "Graphs/MRI_bar3.png", width = 5, height = 5)

#   (iii) Menhinick's index
summary3.2 <- dplyr :: select(sum_data2, year = years_since_disturbance, MeRI = Menhinicks_RI)

mean_table3.2 <- group_by(summary3.2, year) %>% 
  summarise(MeRI_mean = mean(MeRI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.04, 0.06, 0.09))

(mean_MeRI_bar.3 <- ggplot(mean_table3.2, aes(x = year, y = MeRI_mean, colour = year,
                                              fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1.2)) +
    labs(x = "\n Number of years since disturbance", y = "Mean Menhinick's richness index\n") + 
    theme_lb() +
    geom_errorbar(aes(x = year, ymin = MeRI_mean-sd, ymax = MeRI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9) +
    annotate("text", x = 1, y = 0.4, label = "A", size = 6) +
    annotate("text", x = 2, y = 0.8, label = "B", size = 6) +
    annotate("text", x = 3, y = 1.1, label = "B", size = 6)
)

#ggsave(mean_MeRI_bar.3, file = "Graphs/MeRI_bar3.png", height = 5, width = 5)

#   (iv) Simpson's index
summary4.2 <- dplyr :: select(sum_data2, year = years_since_disturbance, SEI = Simpsons_EI)

mean_table4.2 <- group_by(summary4.2, year) %>% 
  summarise(SEI_mean = mean(SEI)) %>% 
  ungroup() %>% 
  mutate( ., sd = c(0.08, 0.05, 0.02))

(mean_SEI_bar.3 <- ggplot(mean_table4.2, aes(x = year, y = SEI_mean, colour = year,
                                             fill = year)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "\n Year", y = "Mean Simpson's evenness index\n") + 
    theme_lb() +
    geom_errorbar(aes(x = year, ymin = SEI_mean-sd, ymax = SEI_mean+sd),
                  width = 0.4, colour = "black", alpha = 0.9, size = 0.9)
)

#ggsave(mean_SEI_bar.3, file = "Graphs/SEI_bar3.png", height = 5, width = 5)


# 3. Plot as panel ----
hyp1.5sum_panel <- grid.arrange(
  mean.abun_bar3 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MRI_bar.3 + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_MeRI_bar.3 + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  mean_SEI_bar.3 + ggtitle("(d)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 2)

#ggsave(hyp1.5sum_panel, file= "Graphs/hyp1_panel5sum.png", width = 10, height = 12)
