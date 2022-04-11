#Laura Barraclough
#Started: 01/04/2022
#email: s1729795@ed.ac.uk

getwd()
View(sum_data)
# make some linear models to check whether there is a 
# relationship between dbh and species richness/ abundance

theme_lb <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = ,"cm"),
          legend.position = "none")
}


# Margalef's richness and dbh ----
lm.a <- lm(Margalefs_RI ~ Mean_DBH, data = sum_data)
summary(lm.a)
plot(lm.a)
rsq(lm.a) #rsq=0.3612757
# plot this
(scat_lm.a <- ggplot(sum_data, aes(x = Mean_DBH, y = Margalefs_RI)) +
                       theme_lb() +
                       geom_point(colour = c("#008B00")) +
                       stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                                   fill = c("#addd8e")))

# Abundance and dbh ----
lm.b <- lm(Total_abundance ~ Mean_DBH, data = sum_data)
summary(lm.b)
plot(lm.b)
rsq(lm.b) #rsq= 0.1334213
(scat_lm.b <- ggplot(sum_data, aes(x = Mean_DBH, y = Total_abundance)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                fill = c("#addd8e")))

# Menhinick's richness and dbh ----
lm.c <- lm(Menhinicks_RI ~ Mean_DBH, data = sum_data)
summary(lm.c)
plot(lm.c)
rsq(lm.b) #rsq= 0.3899
(scat_lm.c <- ggplot(sum_data, aes(x = Mean_DBH, y = Menhinicks_RI)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                fill = c("#addd8e")))

# Simpson's richness and dbh ----
lm.d <- lm(Simpsons_EI ~ Mean_DBH, data = sum_data)
summary(lm.d)
plot(lm.d)
rsq(lm.d) #rsq= 0.146
(scat_lm.d <- ggplot(sum_data, aes(x = Mean_DBH, y = Simpsons_EI)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                fill = c("#addd8e")))
#Panel ----
grid.arrange(
  scat_lm.b + ggtitle("(a)"),
  scat_lm.a + ggtitle("(b)"),
  scat_lm.c + ggtitle("(c)"),
  scat_lm.d + ggtitle("(d)"),
  ncol = 2
)




# Plotting abundance versus margalef's richness index
(scat_lm.a2 <- ggplot(sum_data, aes(x = Total_abundance, y = Margalefs_RI)) +
    theme_lb() +
    geom_point(colour = c("#008B00")) +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#f7fcb9"),
                fill = c("#addd8e")))
lm_a.2 <- lm(Margalefs_RI ~ Total_abundance, data = sum_data)
summary(lm_a.2)
plot(lm_a.2)

#try omitting point 5
sum_data2 <- sum_data %>% 
  select( .,-[5,])
View(sum_data2)

