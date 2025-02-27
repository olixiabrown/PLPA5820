\#Manuscript Link [Link to
manuscript](https://doi.org/10.1094/PDIS-06-21-1253-RE)

\#Packages, Color Palette, and Data Prep

``` r
library(knitr)
library(rmarkdown)
library(pandoc)
library(tinytex)
library(ggplot2)
library(ggpubr)

#read in data
mycotoxindata <- read.csv("MycotoxinData.csv", na.strings="na")

#change treatment from chr to factor
mycotoxindata$Treatment <- as.factor(mycotoxindata$Treatment)

#reorder the levels using factor()
mycotoxindata$Treatment <- factor(mycotoxindata$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

#add colorblind palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
```

\#GGPlots

``` r
#DON GGplot
Q1Plot <- ggplot(mycotoxindata, aes(x = Treatment, y = DON, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.09), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("DON (ppm)") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q1Plot
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#X15ADON GGplot
Q3Plot <- ggplot(mycotoxindata, aes(x = Treatment, y = X15ADON, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.05), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("15ADON") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q3Plot
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#Mass per Seed GGplot
Q3Plot_PT2 <- ggplot(mycotoxindata, aes(x = Treatment, y = MassperSeed_mg, fill= Cultivar, color = Cultivar)) + 
  geom_boxplot(position = position_dodge()) +
  geom_point(position = position_jitterdodge(0.05), alpha=0.6, shape = 21, colour = "black")+
  xlab("") + ylab("Seed Mass (mg)") +
  scale_fill_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) + 
  scale_color_manual(values = c("Ambassador" = "#0072B2", "Wheaton" = "#D55E00")) +
  theme_classic()+
  facet_wrap(~Cultivar)
Q3Plot_PT2
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

\#DON T test

``` r
Q1Plot_TTEST <- Q1Plot+ 
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q1Plot_TTEST
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

\#X15ADON T test

``` r
Q3Plot_TTEST <- Q3Plot +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q3Plot_TTEST
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#Mass Per Seed T test

``` r
Q3Plot_PT2_TTEST <- Q3Plot_PT2 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format")
Q3Plot_PT2_TTEST
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\#Combine T Tests

``` r
figure2<- ggarrange(
  Q1Plot_TTEST, Q3Plot_TTEST, Q3Plot_PT2_TTEST, labels = "AUTO", nrow = 1, ncol = 3, common.legend = T)
figure2
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
