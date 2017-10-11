# Peer-graded Assignment: Course Project 2 Exploratory Data Analysis Question 3
# Luis David Bedon Gomez

setwd("~/Coursera/DataAnalysis")
library(dplyr)
library(ggplot2)
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
# Question 3: Of the four types of sources indicated by   
# the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable, which of these f
# our sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008?

## A quick boxplot showed extremely high values that could be wrong meassurements.
## Therefore a quick analysis by Percentile and Year is chosen

### Take the last 50 1000-quantiles for Baltimore fips==24510
NEIBAL<-filter(NEI,fips==24510)
tailquantile<-tail(quantile(NEIBAL$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("chartsBALtype.RDS")){
  chart<-function(x) NEIBAL[NEIBAL$Emissions<=x & NEIBAL$fips==24510,] %>% group_by(year,type)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
  chartsBALtype<-lapply(tailquantile,chart)
  saveRDS(chartsBALtype,file="chartsBALtype.RDS")
}else {
  chartsBALtype<-readRDS("chartsBALtype.RDS")
}

####################################################################################
## Making the ggplot:

###
png(filename = "plot3.png",width=600,height = 480)
ggplot(data=chartsBALtype[[50]], aes(x=year, y=sum, group=type, color=type))+
geom_line()+ geom_point( size=4, shape=23, fill="white")+ 
xlab("Year")+ ylab("Sum of the PM2.5-Emission-Data [tons]")+ 
ggtitle("Sum of the given PM2,5-Emission-Data for the City of Baltimore \nby Type and Year")+ 
  theme(plot.margin = unit(c(1.5,1,1,1.5), "cm"))
dev.off()


