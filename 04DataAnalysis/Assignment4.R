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
# Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

## A quick boxplot showed extremely high values that could be wrong meassurements.
## Therefore a quick analysis by Percentile and Year is chosen

### Take the last 50 1000-quantiles
tailquantile<-tail(quantile(NEI$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("charts.RDS")){
chart<-function(x) NEI[NEI$Emissions<=x,]%>% group_by(year)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
charts<-lapply(tailquantile,chart)
saveRDS(charts,file="charts.RDS")
}else {
charts<-readRDS("charts.RDS")
}

####################################################################################
## Making the Barplot:

###
png(filename = "plot1.png",width=600,height = 480)
### Adjust margin
par("mar"=c(5,6,5,3))
### Create initial barplot
barplot(charts[[50]][[2]],names.arg = c(charts[[10]][[1]]),ylab="Sum of the PM2.5-Emission-Data, \n[tons]",xlab="Year", col=rgb(0,0,0,0),ylim=c(0.1,8e6))
title(main="Sum of the given PM2,5-Emission-Data \nby 1000th-quantiles and Year")

### Create function to add the next bars for the other 10 1000th-quantiles
addbar <- function(x) barplot(charts[[x]][[2]],add=TRUE,col=rgb(.5,.5,.5,.2),axes=FALSE)
sapply(40:49,addbar)

### Label the quantiles
text(c(.7,1.9,3.1,4.3),c(5.5e6,4.3e6,4.1e6,3e6),c("100%","100%","100%","100%"))
text(c(.7,1.9,3.1,4.3),c(3.4e6,2.7e6,2.8e6,2.2e6),c("99.9%","99.9%","99.9%","99.9%"),cex=1)
text(c(.7,1.9,3.1,4.3),c(2.7e6,2.2e6,2.25e6,1.85e6),c("99.8%","99.8%","99.8%","99.8%"),cex=.8)
text(c(.7,1.9,3.1,4.3),c(2.2e6,1.85e6,1.89e6,1.6e6),c("99.7%","99.7%","99.7%","99.7%"),cex=.7)
text(c(.7,1.9,3.1,4.3),c(1.9e6,1.6e6,1.65e6,1.4e6),c("...","...","...","..."),cex=.8)
dev.off()


#############################################################
Voruntersuchung

colpurp<-colorRamp(c("yellow","purple"),50)

#Logarithmic
dev.off()
newpoints<-function(x) points(charts[[x]][[2]]~charts[[x]][[1]],col=rgb(colpurp(x/50)/255))
newsegments<-function(x) segments(charts[[x]][[1]][1:3],charts[[x]][[2]][1:3],charts[[x]][[1]][2:4],charts[[x]][[2]][2:4],col=rgb(colpurp(x/50)/255))
plot(charts[[50]][[2]]~charts[[50]][[1]],ylim=c(2e5,8e6),col=rgb(colpurp(50/50)/255),type="n",log="y")
segments(charts[[50]][[1]][1:3],charts[[50]][[2]][1:3],charts[[50]][[1]][2:4],charts[[50]][[2]][2:4],col=rgb(colpurp(50/50)/255))
sapply(1:50,newpoints)
sapply(1:50,newsegments)

#Absolut
dev.off()
#newpoints<-function(x) points(charts[[x]][[2]]~charts[[x]][[1]],col=rgb(colpurp(x/50)/255))
#newsegments<-function(x) segments(charts[[x]][[1]][1:3],charts[[x]][[2]][1:3],charts[[x]][[1]][2:4],charts[[x]][[2]][2:4],col=rgb(colpurp(x/50)/255))
plot(charts[[50]][[2]]~charts[[50]][[1]],ylim=c(0,8e6),col=rgb(colpurp(50/50)/255),type="n")
segments(charts[[50]][[1]][1:3],charts[[50]][[2]][1:3],charts[[50]][[1]][2:4],charts[[50]][[2]][2:4],col=rgb(colpurp(50/50)/255))
sapply(1:50,newpoints)
sapply(1:50,newsegments)
