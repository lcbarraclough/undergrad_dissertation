#Laura Barraclough
#Started: 09/03/2022
#email: s1729795@ed.ac.uk

# The aim of this script is to produce graphs that show the changes in insect
# communities between the three stands.

# Load libraries and data frames
library(tidyverse)
library(gridExtra)

getwd()
setwd("C:/Users/lcbar/OneDrive/Documents/Dissertation_proj/undergrad_dissertation/nmds")
counts <- read.csv("com_structure.csv")
counts$years_since_disturbance <- as.character(counts$years_since_disturbance)

# put data in long format
counts <- gather(counts, species, counts,
       c(collembola, dermaptera, hemiptera, coleoptera, diptera, hymenoptera, mollusca,
         arachnida, other))
str(counts)

# Theme
theme_lb <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = ,"cm"),
          legend.position = "none")
}

tree.palette <- c("#f7fcb9","#addd8e", "#31a354")


# 1. Bar charts ----
# One big chart
(com_structure1 <- ggplot(counts, aes(x = species, y = counts, fill = years_since_disturbance,
                                      colour = years_since_disturbance)) +
   geom_histogram(stat = "identity", position = "dodge") +
   labs(x = "\n Insect group", y = "Count\n") +
   theme_lb() +
   theme(axis.text = element_text(size = 6),
         axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_fill_manual(values = tree.palette))

#ggsave(com_structure1, file = "Graphs/com_structure1.png", width = 5, height = 5)

# Individual charts
# 14-year-old stand
sub_set1 <- filter(counts, years_since_disturbance == 14)
(hist_1 <- ggplot(sub_set1, aes(x = species, y = counts, fill = species,
                                colour = species)) +
                    geom_histogram(stat = "identity", position = "dodge") +
    theme_lb() +
  labs(x = "\n Insect Group", y = "Count\n") +
    scale_y_continuous(limits = c(0,40)) +
    theme(axis.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1))
    )


# 24-year-old stand
sub_set2 <- filter(counts, years_since_disturbance == 24)
(hist_2 <- ggplot(sub_set2, aes(x = species, y = counts, fill = species,
                                colour = species)) +
    geom_histogram(stat = "identity", position = "dodge") +
    theme_lb() +
    scale_y_continuous(limits = c(0,40)) +
    labs(x = "\n Insect Group", y = "Count\n") +
    theme(axis.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1))
)

# 34-year-old stand
sub_set3 <- filter(counts, years_since_disturbance == 34)
(hist_3 <- ggplot(sub_set3, aes(x = species, y = counts, fill = species,
                                colour = species)) +
    geom_histogram(stat = "identity", position = "dodge") +
    theme_lb() +
    scale_y_continuous(limits = c(0,40)) +
    labs(x = "\n Insect Group", y = "Count\n") +
    theme(axis.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1))
)

# panel construction
(com_structure_panel <- grid.arrange(
  hist_1 + ggtitle("(a)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  hist_2 + ggtitle("(b)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  hist_3 + ggtitle("(c)") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  ncol = 1))

#ggsave(com_structure_panel, file = "Graphs/com_structure_panel.png", width = 5, height = 10)
# 2. Pie charts ----
counts_2 <- counts
counts_2 <- mutate(counts_2, richness = count_if
                   
??tally       
