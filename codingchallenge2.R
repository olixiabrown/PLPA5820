#read in data
mycotoxindata <- read.csv("MycotoxinData.csv")

#load in ggplot
library(ggplot2)

str(mycotoxindata)

#change DON from character vector to numeric vector 
mycotoxindata$DON <- as.numeric(mycotoxindata$DON)

#make a boxplot 
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("DON (ppm)") 

#bar chart with SE bars 
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar, fill = Cultivar)) + 
  stat_summary(fun = mean, geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar") + 
  xlab("") + 
  ylab("DON (ppm)")

#add points to foreground of bar chart, set shape and color of points
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar, fill = Cultivar))+
  stat_summary(fun = mean, geom="bar",position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("DON (ppm)") + geom_point(position=position_jitterdodge(dodge.width=0.9), shape = 21, colour = "black")


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")


#change the fill color of points and boxplots to ones in the colorblind palette
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar, fill=Cultivar)) + 
  stat_summary(fun = mean, geom="bar",position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("DON (ppm)") + geom_point(position=position_jitterdodge(dodge.width=0.9), shape = 21, colour = "black")+
  theme_minimal() +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00"))

#add a facet based on the cultivar
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar, fill=Cultivar)) + 
  stat_summary(fun = mean, geom="bar",position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("DON (ppm)") + geom_point(position=position_jitterdodge(dodge.width=0.9), shape = 21, colour = "black")+
  theme_minimal() +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  facet_wrap(~Cultivar, scales = "free")

#make our points more transparent using alpha and geom jitter
ggplot(mycotoxindata, aes(x = Treatment, y = DON, color = Cultivar, fill=Cultivar)) + 
  stat_summary(fun = mean, geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar") + 
  xlab("") + 
  ylab("DON (ppm)") + geom_jitter(alpha = 0.5, shape = 21, colour = "black")+
  theme_minimal() +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  facet_wrap(~Cultivar)
