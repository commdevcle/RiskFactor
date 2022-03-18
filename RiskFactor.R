options(scipen=999)
options(knitr.table.format = "latex")
#removing every ting in R
rm(list=ls())

library(tidyverse)

#install.packages("BAMMtools")

library(BAMMtools)
library(ggplot2)

# Work directory Setup            #
#getwd()
#dd = c("c:/Rwork/")
#setwd(dd)
getwd()

mydata <- read.csv("PR02ListActivitiesbyProgramYearProject.csv", header = TRUE)

mydata1 <- subset(mydata, IDIS.Activity.ID !=15079)

mydata2 <- mydata1 %>%
  filter(Funded.Amount > 0 & IDIS.Project < 12) 


fund.mean <- mean(mydata2$Funded.Amount)
fund.median <- median(mydata2$Funded.Amount) # due to skewness
mydata2$Award.Ratio <- mydata2$Funded.Amount/fund.median

award.ratio.mean <- mean(mydata2$Award.Ratio)
summary(mydata2$Award.Ratio)

# adding value 

mydata2$Award.Ratio.cat <- 0

length(mydata2$Award.Ratio.cat)


for (i in 1: length(mydata2$Award.Ratio.cat))
{
  if (mydata2$Award.Ratio[i] <= 0.25){
    mydata2$Award.Ratio.cat [i] <- 1
  } else if (mydata2$Award.Ratio[i] > 0.25 & mydata2$Award.Ratio[i] <= 0.5){
    mydata2$Award.Ratio.cat [i] <- 2
  } else if (mydata2$Award.Ratio[i] > 0.50 & mydata2$Award.Ratio[i] <= 0.75){
    mydata2$Award.Ratio.cat [i] <- 3
  } else if (mydata2$Award.Ratio[i] > 0.75 & mydata2$Award.Ratio[i] <= 1.00){
    mydata2$Award.Ratio.cat [i] <- 4
  } else if (mydata2$Award.Ratio[i] > 1.00 & mydata2$Award.Ratio[i] <= 1.50){
    mydata2$Award.Ratio.cat [i] <- 5
  } else if (mydata2$Award.Ratio[i] > 1.50 & mydata2$Award.Ratio[i] <= 2.00){
    mydata2$Award.Ratio.cat [i] <- 6
  } else if (mydata2$Award.Ratio[i] > 2.00 & mydata2$Award.Ratio[i] <= 4.00){
    mydata2$Award.Ratio.cat [i] <- 7
  } else if (mydata2$Award.Ratio[i] > 4.00 & mydata2$Award.Ratio[i] <= 6.00){
    mydata2$Award.Ratio.cat [i] <- 8
  } else if (mydata2$Award.Ratio[i] > 6.00 & mydata2$Award.Ratio[i] <= 8.00){
    mydata2$Award.Ratio.cat [i] <- 9
  } else mydata2$Award.Ratio.cat [i] <- 10
}


table (mydata2$Award.Ratio.cat)
hist(mydata2$Award.Ratio.cat)



## draw rate

mydata2$DrawRate <- mydata2$Draw.Amount / mydata2$Funded.Amount

## draw rate categories

mydata2$DrawRate.cat <- 0

length(mydata2$Award.Ratio.cat)


for (i in 1: length(mydata2$DrawRate.cat))
{
  if (mydata2$DrawRate[i] <= 0.10) {
    mydata2$DrawRate.cat [i] <- 5
  } else if (mydata2$DrawRate[i] > 0.10 & mydata2$DrawRate[i] <= 0.25){
    mydata2$DrawRate.cat [i] <- 4
  } else if (mydata2$DrawRate[i] > 0.25 & mydata2$DrawRate[i] <= 0.50){
    mydata2$DrawRate.cat [i] <- 3
  } else if (mydata2$DrawRate[i] > 0.50 & mydata2$DrawRate[i] <= 0.75){
    mydata2$DrawRate.cat [i] <- 2
  } else if (mydata2$DrawRate[i] > 0.75 & mydata2$DrawRate[i] <= 1.00){
    mydata2$DrawRate.cat [i] <- 1
  } 
}


table (mydata2$DrawRate.cat)
hist(mydata2$DrawRate.cat)


mydata2$RiskFactor <- mydata2$Award.Ratio.cat + mydata2$DrawRate.cat

table(mydata2$RiskFactor)


mydata2$RiskFactor.cat<- 0


for (i in 1: length(mydata2$DrawRate.cat))
{
  if (mydata2$RiskFactor [i] <= 5) {
    mydata2$RiskFactor.cat [i] <- 1
  } else if (mydata2$RiskFactor[i] > 5 & mydata2$RiskFactor[i] <= 10){
    mydata2$RiskFactor.cat [i] <- 2
  } else if (mydata2$RiskFactor[i] > 10 & mydata2$RiskFactor[i] <= 15){
    mydata2$RiskFactor.cat [i] <- 3
  } 
}

mydata2$RiskFactor.cat.desc <- "NA"
mydata2$RiskFactor.cat.desc [mydata2$RiskFactor.cat ==1] <- "3. Low Risk"
mydata2$RiskFactor.cat.desc [mydata2$RiskFactor.cat ==2] <- "2. Medium Risk"
mydata2$RiskFactor.cat.desc [mydata2$RiskFactor.cat ==3] <- "1. High Risk"


table (mydata2$RiskFactor.cat)

# Random Sampling
#high risk - 5
#Medium risk - 4
#Low risk - 1
#select & mark

mydata2.low <- mydata2 %>%
  filter(RiskFactor.cat ==1) 

mydata2.medium <- mydata2 %>%
  filter(RiskFactor.cat ==2) 

mydata2.high <- mydata2 %>%
  filter(RiskFactor.cat ==3) 

mysample.low <- sample(mydata2.low$IDIS.Activity.ID, size = 1)
mysample.low

mysample.med <- sample(mydata2.medium$IDIS.Activity.ID, size = 4)
mysample.med

mysample.high <- sample(mydata2.high$IDIS.Activity.ID, size = 5)
mysample.high


#update selection on data
mydata2$Sel <- 0

# low risk selection update
for (i in 1: length(mydata2$Sel))
{
  if (mydata2$IDIS.Activity.ID [i] == mysample.low [1]) {
    mydata2$Sel [i] <- 1
  } 
}

#medium risk selection update
for (i in 1: length(mydata2$Sel))
{
  if (mydata2$IDIS.Activity.ID [i] == mysample.med [1]) {
    mydata2$Sel [i] <- 2
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.med [2]) {
    mydata2$Sel [i] <- 2
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.med [3]) {
    mydata2$Sel [i] <- 2
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.med [4]) {
    mydata2$Sel [i] <- 2
  } 
}

#high risk selection update
for (i in 1: length(mydata2$Sel))
{
  if (mydata2$IDIS.Activity.ID [i] == mysample.high [1]) {
    mydata2$Sel [i] <- 3
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.high [2]) {
    mydata2$Sel [i] <- 3
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.high [3]) {
    mydata2$Sel [i] <- 3
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.high [4]) {
    mydata2$Sel [i] <- 3
  } else if (mydata2$IDIS.Activity.ID [i] == mysample.high [5]) {
    mydata2$Sel [i] <- 3
  } 
}

table (mydata2$Sel)

write.csv(mydata2, file="PR02RiskFactor.csv", row.names = FALSE, na="")


# plot
theme_set(theme_bw())
data(mydata2, package = "ggplot2")
myplot <- ggplot (mydata2, aes(x=Award.Ratio, y=DrawRate))+
                geom_point(aes(col=RiskFactor.cat.desc, size=RiskFactor)) +
                labs(subtitle = "Award Risk vs Performance Risk", 
                 y= "Performance Risk",
                 x= "Award Risk",
               title = "Risk Factor Analysis",
               caption = "Source: HUD PR02 Report as of Oct 25, 2021")

         
plot(myplot)



