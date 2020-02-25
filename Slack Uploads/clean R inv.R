library("RWeka")
library("rattle")
library("rpart")
library("C50")
library("datasets")
library("mlbench")
library(partykit)
library("caret")
library('dplyr')
#install.packages('tidyverse')
library(tidyverse)
#install.packages('SnowballC')
#library(SnowballC)
#install.packages("SentimentAnalysis")
#library(SentimentAnalysis)
library(dplyr)
library("readxl")
str(plast_wl)
setwd('C:\\Users\\User\\Documents\\ds masters\\sem2\\rnd\\dsets\\plast_waste_land')
plast_wl <- read_excel("Plastic_land_ocean.xlsx")
ncol(plast_wl)
names(plast_wl)
plasticW <- plast_wl


names(plasticW)[names(plasticW) == "Mismanaged plastic waste in 2010\r\n [tonnes]7"] <- "Mismanaged_PL_2010_ton"
names(plasticW)[names(plasticW) == "Plastic waste generation [kg/day]7"] <- "Pl_waste_gen_kgPerday"
names(plasticW)[names(plasticW) == "Inadequately managed plastic waste [kg/day]7"] <- "Inad_man_pl_waste_kgPerday"
names(plasticW)[names(plasticW) == "Waste generation [kg/day]7"] <- "Waste_gen_kgPerday"
names(plasticW)[names(plasticW) == "Economic status1"] <- "Ec_status"
names(plasticW)[names(plasticW) == "Coastal population2"] <- "Coastal_pop"
names(plasticW)[names(plasticW) == "% Inadequately managed waste5"] <- "InadeqManWaste_Percent"
names(plasticW)[names(plasticW) == "% Littered waste6"] <- "LitteredWaste_Percent"
names(plasticW)[names(plasticW) == "% Plastic in waste stream4"] <- "Pl_in_waste_stream_Percent"
names(plasticW)[names(plasticW) == "Mismanaged plastic waste in 2025\r\n [tonnes]7"] <- "Mismanaged_PL_2025_ton"
names(plasticW)[names(plasticW) == "Waste generation rate [kg/person/day]3"] <- "Waste_genRate_kgPerpersonPerday"
names(plasticW)[names(plasticW) == "Plastic waste littered\r\n [kg/day]7"] <- "Pl_wast_littered_kgPerday"
names(plasticW)[names(plasticW) == "Mismanaged plastic waste [kg/person/day]7"] <-"Mismanaged_PL_kgPerpersonPerday"


str(plasticW)
head(plasticW)
plasticW <- plasticW[complete.cases(plasticW), ]
plasticW$Ec_status <- as.factor(plasticW$Ec_status)
plasticW <- plasticW[order(-plasticW$Mismanaged_PL_2010_ton),]
head(plasticW)

mydata <- c("Country",
            "Ec_status",
            "Coastal_pop",
            "Waste_gen_kgPerday",
            "Pl_waste_gen_kgPerday",
            "Inad_man_pl_waste_kgPerday",
            "Mismanaged_PL_2010_ton",
            "Mismanaged_PL_2025_ton")

df <- plasticW[mydata] 
str(df)
head(df)
nrow(df)
colnames(df)
dfN <- df[,-c(1,2)]
str(dfN)
cor(dfN)
corrplot(cor(dfN))
# High correlation between "Waste_gen_kgPerday" and "Pl_waste_gen_kgPerday" -> Plastic is main waste
# High correlation between "Inad_man_pl_waste_kgPerday" and "Mismanaged_PL_2010_ton " & "Mismanaged_PL_2025_ton"
dfN2 <- df[,-c(1)]
myModel=lm(dfN2$Mismanaged_PL_2010_ton ~., data=dfN2)
dfN2$predicted <- predict(myModel)
dfN2$residuals <- myModel$residuals
summary(myModel)
df$predicted <- NULL
df$residuals <- NULL

###
library(ggplot2)
p <- ggplot(dfN2, aes(x = dfN2$Waste_gen_kgPerday, y = dfN2$Pl_waste_gen_kgPerday))
p <- p + geom_smooth(method = "lm",
                     se = TRUE, color = "lightgrey")
p <- p + geom_segment(aes(xend = dfN2$Waste_gen_kgPerday, yend = predicted),
                      alpha = .2) # alpha to fade lines
p <- p + geom_point(aes(color='red'),show.legend = FALSE)
p <- p + geom_point(aes(y = predicted), shape = 1)
p <- p + xlab('Waste_gen_kgPerday') + ylab('Pl_waste_gen_kgPerday')
p <- p + theme_bw()
p

###
p <- ggplot(dfN2, aes(x = dfN2$Waste_gen_kgPerday, y = dfN2$Mismanaged_PL_2010_ton))
p <- p + geom_smooth(method = "lm",
                     se = TRUE, color = "lightgrey")
p <- p + geom_segment(aes(xend = dfN2$Waste_gen_kgPerday, yend = predicted),
                      alpha = .2) # alpha to fade lines
p <- p + geom_point(aes(color='red'),show.legend = FALSE)
p <- p + geom_point(aes(y = predicted), shape = 1)
p <- p + xlab('Waste_gen_kgPerday') + ylab('dfN2$Mismanaged_PL_2010_ton')
p <- p + theme_bw()
p
###

p <- ggplot(dfN2, aes(x = dfN2$Ec_status, y = dfN2$Mismanaged_PL_2010_ton))
p <- p + geom_smooth(method = "lm",
                     se = TRUE, color = "lightgrey")
p <- p + geom_segment(aes(xend = dfN2$Ec_status, yend = predicted),
                      alpha = .2) # alpha to fade lines
p <- p + geom_point(aes(color='red'),show.legend = FALSE)
p <- p + geom_point(aes(y = predicted), shape = 1)
p <- p + xlab('Ec_status') + ylab('dfN2$Mismanaged_PL_2010_ton')
p <- p + theme_bw()
p
# Low income countries seem to contribute a lot in Mismanaged_PL_2010_ton
dfTop <- df[1:20,]
plot(dfTop$Ec_status, dfTop$Mismanaged_PL_2010_ton)
plasticW$Ec_status <- as.factor(plasticW$Ec_status)
dfTop$Country2 <- as.factor(dfTop$Country)
plot(dfTop$Country2, dfTop$Mismanaged_PL_2010_ton)
