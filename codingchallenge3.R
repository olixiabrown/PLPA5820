mycotoxindata <- read.csv("MycotoxinData.csv", na.strings="na")

#load in required packages
library(ggplot2)
library(ggpubr)

#have a look at our data
str(mycotoxindata)

#add our colorblind palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

#Q -1 
#alpha = 0.6 adds transparency
#scale fill is fill color 
#scale color is border color
Q1Plot <- ggplot(mycotoxindata, aes(x = Treatment, y = DON, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.09), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("DON (ppm)") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q1Plot

#Q-2  
str(mycotoxindata)
#change treatment from chr to factor
mycotoxindata$Treatment <- as.factor(mycotoxindata$Treatment)

#reorder the levels using factor()
levels(mycotoxindata$Treatment)
mycotoxindata$Treatment <- factor(mycotoxindata$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

#Q-3

#change y variable to x15ADON
Q3Plot <- ggplot(mycotoxindata, aes(x = Treatment, y = X15ADON, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.05), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("15ADON") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q3Plot

#change y variable to mass per seed 
Q3Plot_PT2 <- ggplot(mycotoxindata, aes(x = Treatment, y = MassperSeed_mg, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.05), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("Seed Mass (mg)") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q3Plot_PT2

#Q-4
#use ggarrange to put all the plots next to each other
#common.legend = true (T) will combine legends for plots within ggarange argument
#false (F) gives you a legend for each plot
# labels = "auto" will give lowercase labels, "AUTO" makes them uppercase
figure1<- ggarrange(
  Q1Plot, Q3Plot, Q3Plot_PT2, labels = "AUTO", nrow = 1, ncol = 3, common.legend = T)
figure1

#Q-5 

#run t test and add labels 
#label = p.adj.format adds pvalues, p.adj.signif makes * the sig. identifer, {p.adj.format}{p.adj.signif} shows * and pvalue 
Q1Plot_TTEST <- Q1Plot+ 
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q1Plot_TTEST

Q3Plot_TTEST <- Q3Plot +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q3Plot_TTEST

Q3Plot_PT2_TTEST <- Q3Plot_PT2 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q3Plot_PT2_TTEST

#use ggarrange to put all the t test plots together 
figure2<- ggarrange(
  Q1Plot_TTEST, Q3Plot_TTEST, Q3Plot_PT2_TTEST, labels = "AUTO", nrow = 1, ncol = 3, common.legend = T)
figure2


