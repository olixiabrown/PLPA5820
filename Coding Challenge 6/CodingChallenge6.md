[Link to Github](https://github.com/olixiabrown/PLPA5820)

\####Q3 - Read in dataframe

``` r
getwd()
```

    ## [1] "/Users/oliviabrown/Desktop/R directories and code/PLPA5820"

``` r
setwd("/Users/oliviabrown/Desktop/R directories and code/PLPA5820/Coding Challenge 6")
cities<- read.csv("Cities.csv")
library(tidyverse)
```

    ## Warning: package 'tidyr' was built under R version 4.4.1

    ## Warning: package 'purrr' was built under R version 4.4.1

    ## Warning: package 'lubridate' was built under R version 4.4.1

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

\####Q4 - Haversine formula function

``` r
haversine <- function(lat1, lon1, lat2,lon2){
  rad.lat1 <- lat1 * pi/180
  rad.lon1 <- lon1 * pi/180
  rad.lat2 <- lat2 * pi/180
  rad.lon2 <- lon2 * pi/180

  delta_lat <- rad.lat2 - rad.lat1
  delta_lon <- rad.lon2 - rad.lon1
  
  a <- sin(delta_lat / 2)^2 + cos(rad.lat1) * cos(rad.lat2) * sin(delta_lon / 2)^2
  c <- 2 * asin(sqrt(a)) 
  earth_radius <- 6378137
  distance_km <- (earth_radius * c)/1000
  return(distance_km)
}
```

\####Q5 - Distance between Auburn and NYC

``` r
#filter df to only include cities, lat, and long columns

#cities_newdf <- cities %>%
  #filter(city == "Auburn" | city == "New York") %>%
 # select(city, lat, long)

auburn <- subset(cities, city == "Auburn")
newyork <- subset(cities, city == "New York")

lat2 <- auburn$lat
lon2 <- auburn$long
lat1 <- newyork$lat
lon1 <- newyork$long

haversine(lat1, lon1,lat2,lon2)
```

    ## [1] 1367.854

\####Q6 - Distance between Auburn and all cities

``` r
city <- cities[,1]

for(i in seq_along(city)){
  newyork <- subset(cities, city == city[[i]])

lat2 <- auburn$lat
lon2 <- auburn$long
lat1 <- newyork$lat
lon1 <- newyork$long
  result <- haversine(lat1, lon1, lat2, lon2)
  print(result)
}
```

    ## [1] 1367.854
    ## [1] 3051.838
    ## [1] 1045.521
    ## [1] 916.4138
    ## [1] 993.0298
    ## [1] 1056.022
    ## [1] 1239.973
    ## [1] 162.5121
    ## [1] 1036.99
    ## [1] 1665.699
    ## [1] 2476.255
    ## [1] 1108.229
    ## [1] 3507.959
    ## [1] 3388.366
    ## [1] 2951.382
    ## [1] 1530.2
    ## [1] 591.1181
    ## [1] 1363.207
    ## [1] 1909.79
    ## [1] 1380.138
    ## [1] 2961.12
    ## [1] 2752.814
    ## [1] 1092.259
    ## [1] 796.7541
    ## [1] 3479.538
    ## [1] 1290.549
    ## [1] 3301.992
    ## [1] 1191.666
    ## [1] 608.2035
    ## [1] 2504.631
    ## [1] 3337.278
    ## [1] 800.1452
    ## [1] 1001.088
    ## [1] 732.5906
    ## [1] 1371.163
    ## [1] 1091.897
    ## [1] 1043.273
    ## [1] 851.3423
    ## [1] 1382.372
    ## [1] 0
