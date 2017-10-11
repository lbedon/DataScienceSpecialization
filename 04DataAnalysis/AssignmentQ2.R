# Peer-graded Assignment: Course Project 2 Exploratory Data Analysis
# Luis David Bedon Gomez

setwd("~/Coursera/DataAnalysis")
library(dplyr)
####################################################################################
# Step 1: Getting the data

## If "exdata%2Fdata%2FNEI_data.zip" does not exist, download and unzip the file.

filename<-"exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")){
  urldata<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urldata,destfile = filename,method = "curl")
  unzip(filename)
}

## Get the data (and do not read the file if the Variable exists!!):
if(!exists("NEI")){
  NEI<-readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")#,stringsAsFactors = FALSE
}

####################################################################################
# Question 2: Have total emissions from PM2.5 decreased in the  Baltimore City, 
# Maryland ( 𝚏𝚒𝚙𝚜 == 𝟸𝟺𝟻𝟷𝟶) from 1999 to 2008?

## A quick boxplot showed extremely high values that could be wrong meassurements.
## Therefore a quick analysis by Percentile and Year is chosen

### Take the last 50 1000-quantiles for Baltimore fips==24510
NEIBAL<-filter(NEI,fips==24510)
tailquantile<-tail(quantile(NEIBAL$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("chartsBAL.RDS")){
  chart<-function(x) NEIBAL[NEIBAL$Emissions<=x & NEIBAL$fips==24510,] %>% group_by(year)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
  chartsBAL<-lapply(tailquantile,chart)
  saveRDS(chartsBAL,file="chartsBAL.RDS")
}else {
  chartsBAL<-readRDS("chartsBAL.RDS")
}

####################################################################################
## Making the Barplot:

###
png(filename = "plot2.png",width=600,height = 480)
### Adjust margin
par("mar"=c(5,6,5,3))
### Create initial barplot
barplot(chartsBAL[[50]][[2]],names.arg = c(chartsBAL[[10]][[1]]),ylab="Sum of the PM2.5-Emission-Data, \n[tons]",xlab="Year", col=rgb(0,0,0,0),ylim=c(0,max(chartsBAL[[50]][[2]])*1.2))
title(main="Sum of the given PM2,5-Emission-Data for the City of Baltimore \nby Percentiles and Year")

### Create function to add the next bars for the other 10 percentiles
addbar <- function(x) barplot(chartsBAL[[x]][[2]],add=TRUE,col=rgb(.5,.5,.5,.1),axes=FALSE)
sapply(seq(10,50,5),addbar)

### Label the quantiles
text(c(.7,1.9,3.1,4.3),c(2260,2160,2670,1640),c("100%","100%","100%","100%"))
text(c(.7,1.9,3.1,4.3),c(1240,1600,1840,1250),c("99.5%","99.5%","99.5%","99.5%"),cex=1)
text(c(.7,1.9,3.1,4.3),c(1060,1200,1330,970),c("99.0%","99.0%","99.0%","99.0%"),cex=.8)
text(c(.7,1.9,3.1,4.3),c(820,960,1140,800),c("98.5%","98.5%","98.5%","98.5%"),cex=.7)
text(c(.7,1.9,3.1,4.3),c(700,820,950,690),c("...","...","...","..."),cex=.8)
dev.off()




######
#Koordinaten der Markierungen:
round((chartsBAL[[30]][[2]]-chartsBAL[[25]][[2]])/2+chartsBAL[[25]][[2]],-1)