##Olivia Brown and Kylie Blake
getwd()

#make a vector named z w/ values 1-200
z <- c(1:200)

#calculate mean and standard deviation
mean(z)
sd(z)

#make vector true for values > 30 and false for anything else
zlog <- z>30
zlog 

#made dataframe with z and zlog
zdf <- data.frame(z, zlog)
zdf

#renamed columns in zdf dataframe
colnames(zdf) <- c("zvec","zlogic")

#made new column with sqrt (not squared (x^2) sorry!!)
zdf$zsquared <- sqrt(zdf$zvec)

#made new dataframe with subset 
newdf <- subset(zdf, zsquared > 10 & zsquared < 100)

#same dataframe without using subset function
newdf2 <- zdf[zdf$zsquared > 10 & zdf$zsquared < 100,]

#new dataframe to only include values on row 26
newdf3 <- zdf[26,]

#new dataframe to only include values for zsquared(3rd column) on 180th row 
newdf4 <- data.frame(zdf[180,3])

getwd()
read.csv("/Users/oliviabrown/Desktop/TipsR.csv", na.strings=".")

