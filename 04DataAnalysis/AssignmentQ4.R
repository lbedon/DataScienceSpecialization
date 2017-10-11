# Peer-graded Assignment: Course Project 2 Exploratory Data Analysis Question 4
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
# Question 4: Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?



## Take all categories in SCC containing "Coal" and merge NEI with the needed categories
coal<-data.frame(coal=SCC$SCC[grep("[Cc]oal", SCC$EI.Sector)])
NEIcoal<-merge(NEI,coal,by.x="SCC",by.y="coal")

### Take the last 50 1000-quantiles
tailquantile<-tail(quantile(NEIcoal$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("chartsNEIcoal.RDS")){
  chart<-function(x) NEIcoal[NEIcoal$Emissions<=x,] %>% group_by(year)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
  chartsNEIcoal<-lapply(tailquantile,chart)
  saveRDS(chartsNEIcoal,file="chartsNEIcoal.RDS")
}else {
  chartsNEIcoal<-readRDS("chartsNEIcoal.RDS")
}

####################################################################################
## Making the ggplot:

###
png(filename = "plot4.png",width=600,height = 480)


ggplot()+
  geom_col(data=chartsNEIcoal[[50]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  geom_col(data=chartsNEIcoal[[49]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  geom_col(data=chartsNEIcoal[[48]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  geom_col(data=chartsNEIcoal[[47]] ,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  geom_col(data=chartsNEIcoal[[46]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  geom_col(data=chartsNEIcoal[[45]],aes(x=year, y=sum),fill=rgb(.5,.5,.5,.3))+
  xlab("Year")+ ylab("Sum of the coal-combustion-related\nPM2.5-Emission-Data [tons]")+ 
  ggtitle("Sum of the coal-combustion-related PM2,5-Emission-Data\nfor the United States by Year and selected 1000th-quantiles")+ 
  theme(plot.margin = unit(c(1.5,1,1,1.5), "cm"))+ 
  annotate("text", x = c(1999,2002,2005,2008), y = c(524650,512820,520220,333370), label = "100%",size = 3)+ 
  annotate("text", x = c(1999,2002,2005,2008), y = c(441410,468320,476440,304390), label = "99.9%",size = 3)+ 
  annotate("text", x = c(1999,2002,2005,2008), y = c(390960,438850,445940,276860), label = "99.8%",size = 2.8)+
  annotate("text", x = c(1999,2002,2005,2008), y = c(355570,411160,412260,263180), label = "...",size = 3)+
  scale_x_continuous(name="Year",breaks=c(1999,2002,2005,2008))+
  theme(axis.title.y = element_text(size=12))

dev.off()


