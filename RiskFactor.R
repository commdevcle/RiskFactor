options(scipen=999)
options(knitr.table.format = "latex")
#removing every ting in R
rm(list=ls())

library(tidyverse)
#install.packages("BAMMtools")
library(BAMMtools)
library(dplyr)
library(ggplot2)

install.packages("viridis")
library(viridis)

library(ggiraph)

getwd()

mydata <- read.csv("PR02ListActivitiesbyProgramYearandProject03182022.csv", header = TRUE)

#why ? CDPH Admin
mydata1 <- subset(mydata, IDIS.Activity.ID !=15380)

# why? 12 Land retutilization, 13 Fair Housing Admin, 14 General Program Admin, 15 Section 108 Loans
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

# Random Sampling for field monitoring
#high risk - 2
#Medium risk - 1
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

mysample.med <- sample(mydata2.medium$IDIS.Activity.ID, size = 1)
mysample.med

mysample.high <- sample(mydata2.high$IDIS.Activity.ID, size = 2)
mysample.high


# make a group for selected

monitoringlist <- c(mysample.high, mysample.med, mysample.low)

monitoringlist
length(monitoringlist)


#update selection on data
mydata2$Sel <- 0


# risk selection update

for (i in 1: length(monitoringlist)) {

  for (j in 1: length(mydata2$Sel))
  {
    if (mydata2$IDIS.Activity.ID [j] == monitoringlist[i]) {
        mydata2$Sel [j] <- 1
      } 
  }

}

table (mydata2$Sel)


mydata2.selected <- mydata2 %>%filter(Sel ==1) 


write.csv(mydata2.selected, file="MonitoringList.csv", row.names = FALSE, na="")



#-------------------------------------------------------------------

mydata3 <- mydata2 %>%filter(Sel ==0)


mydata3.low <- mydata3 %>%
  filter(RiskFactor.cat ==1) 

mydata3.medium <- mydata3 %>%
  filter(RiskFactor.cat ==2) 

mydata3.high <- mydata3 %>%
  filter(RiskFactor.cat ==3) 


mysample.low.desk <- sample(mydata3.low$IDIS.Activity.ID, size = 1)
mysample.low.desk

mysample.med.desk <- sample(mydata3.medium$IDIS.Activity.ID, size = 2)
mysample.med.desk

mysample.high.desk <- sample(mydata3.high$IDIS.Activity.ID, size = 4)
mysample.high.desk


# make a group for selected

deskmonitoringlist <- c(mysample.high.desk, mysample.med.desk, mysample.low.desk)

deskmonitoringlist
length(deskmonitoringlist)


#update selection on data
mydata3$deskSel <- 0


# risk selection update

for (i in 1: length(deskmonitoringlist)) {
  
  for (j in 1: length(mydata3$deskSel))
  {
    if (mydata3$IDIS.Activity.ID [j] == deskmonitoringlist[i]) {
      mydata3$deskSel [j] <- 1
    } 
  }
  
}

table (mydata3$deskSel)


mydata3.selected <- mydata3 %>%filter(deskSel ==1) 


write.csv(mydata3.selected, file="DesktopMonitoringList.csv", row.names = FALSE, na="")

#---- update mydata 2 and mydata 3
write.csv(mydata2, file="PR02MonitoringList1q2022.csv", row.names = FALSE, na="")
write.csv(mydata3, file="PR02DeskMonitoringList1q2022.csv", row.names = FALSE, na="")

# plot
myplot <- ggplot (mydata2, aes(x=Award.Ratio, y=DrawRate))+
  geom_point(aes(col=RiskFactor.cat.desc, size=RiskFactor)) +
  geom_text(data=mydata2.selected, aes(label= IDIS.Activity.ID), color="black")+
  #geom_text(data=mydata3.selected, aes(label= IDIS.Activity.ID), color="black")+
  labs(subtitle = "Field Monitoring List 4 in 1 Q 2022", 
       y= "Performance Risk",
       x= "Award Risk",
       title = "Risk Factor Analysis",
       caption = "Source: HUD PR02 Report as of March 18, 2022")+
  theme_minimal()

plot(myplot)


myplot2 <- ggplot (mydata2, aes(x=Award.Ratio, y=DrawRate))+
  geom_point(aes(col=RiskFactor.cat.desc, size=RiskFactor)) +
  #geom_text(data=mydata2.selected, aes(label= IDIS.Activity.ID), color="black")+
  geom_text(data=mydata3.selected, aes(label= IDIS.Activity.ID), color="black")+
  labs(subtitle = "Desktop Monitoring List 7 in 1 Q 2022", 
       y= "Performance Risk",
       x= "Award Risk",
       title = "Risk Factor Analysis",
       caption = "Source: HUD PR02 Report as of March 18, 2022")+
  theme_minimal()

plot(myplot2)


myplot3 <- ggplot (mydata2.selected, aes(x=Award.Ratio, y=DrawRate))+
  geom_point(aes(col=RiskFactor.cat.desc, size=RiskFactor)) +
  geom_text(aes(label= IDIS.Activity.ID), color="black")+
  labs(title = "Risk Factor Analysis", 
       subtitle = "Selected Field Monitor", 
       y= "Performance Risk",
       x= "Award Risk",
       caption = "Source: HUD PR02 Report as of March 18, 2022")+
       theme_minimal()



plot(myplot3)


myplot4 <- ggplot (mydata3.selected, aes(x=Award.Ratio, y=DrawRate))+
  geom_point(aes(col=RiskFactor.cat.desc, size=RiskFactor)) +
  geom_text(aes(label= IDIS.Activity.ID), color="black")+
  labs(title = "Risk Factor Analysis", 
       subtitle = "Selected Desktop Monitor", 
       y= "Performance Risk",
       x= "Award Risk",
       caption = "Source: HUD PR02 Report as of March 18, 2022")+
      theme_minimal()



plot(myplot4)
