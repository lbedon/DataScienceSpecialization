# Peer-graded Assignment: Course Project 2 Exploratory Data Analysis Question 5
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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
## Read "Source_Classification_Code.rds"
SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

####################################################################################
# Question 4: How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?



## Take all categories in SCC containing "Coal" and merge NEI with the needed categories
mv<-data.frame(mv=SCC$SCC[grep("[Mm]otor|[Vv]ehicle", SCC$EI.Sector)])
NEImv<-merge(NEI[NEI$fips==24510,],mv,by.x="SCC",by.y="mv")

### Take the last 50 1000-quantiles
tailquantile<-tail(quantile(NEImv$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("chartsNEImv.RDS")){
  chart<-function(x) NEImv[NEImv$Emissions<=x,] %>% group_by(year)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
  chartsNEImv<-lapply(tailquantile,chart)
  saveRDS(chartsNEImv,file="chartsNEImv.RDS")
}else {
  chartsNEImv<-readRDS("chartsNEImv.RDS")
}

####################################################################################
## Making the ggplot:

###
png(filename = "plot5.png",width=600,height = 480)


ggplot()+
  geom_col(data=chartsNEImv[[50]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.5))+
  geom_col(data=chartsNEImv[[49]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.5))+
  #geom_col(data=chartsNEImv[[48]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  #geom_col(data=chartsNEImv[[47]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  #geom_col(data=chartsNEImv[[46]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  #geom_col(data=chartsNEImv[[45]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  xlab("Year")+ ylab("Sum of the motor-vehicle-related\nPM2.5-Emission-Data [tons]")+ 
  ggtitle("Sum of the motor-vehicle-related PM2,5-Emission-Data\nfor Baltimore by Year, the 1000th and the 999th-quantiles")+ 
  theme(plot.margin = unit(c(1.5,1,1,1.5), "cm"))+ 
  annotate("text", x = c(1999), y = c(310), label = "100%",size = 4)+ 
  annotate("text", x = c(1999), y = c(260), label = "99.9%",size = 4)+ 
  annotate("text", x = c(2002,2005,2008), y = c(120,115,80), label = "100%, 99.9%",size = 4)+ 
  #annotate("text", x = c(1999,2002,2005,2008), y = c(390960,438850,445940,276860), label = "99.8%",size = 2.8)+
  #annotate("text", x = c(1999,2002,2005,2008), y = c(355570,411160,412260,263180), label = "...",size = 3)
  scale_x_continuous(name="Year",breaks=c(1999,2002,2005,2008))+
  theme(axis.title.y = element_text(size=14))
dev.off()


