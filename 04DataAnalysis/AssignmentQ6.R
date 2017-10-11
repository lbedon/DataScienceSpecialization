# Peer-graded Assignment: Course Project 2 Exploratory Data Analysis Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == 𝟶𝟼𝟶𝟹𝟽)
# Which city has seen greater changes over time in motor vehicle emissions?
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
# Question 6: How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?



## Take all categories in SCC containing "Coal" and merge NEI with the needed categories
BL<-data.frame(BL=SCC$SCC[grep("[Mm]otor|[Vv]ehicle", SCC$EI.Sector)])
NEIBL<-merge(NEI[NEI$fips=="24510"|NEI$fips=="06037",],BL,by.x="SCC",by.y="BL")

### Take the last 50 1000-quantiles
tailquantile<-tail(quantile(NEIBL$Emissions,1:1000/1000),50)

### Create function to summarize the data per quantile and year if 
### this variable does not exist. Takes time to calculate!!
if(!file.exists("chartsNEIBL.RDS")){
  chart<-function(x) NEIBL[NEIBL$Emissions<=x,] %>% group_by(year,fips)%>%summarize(sum=sum(Emissions),max(Emissions),N=length(Emissions),Mean=mean(Emissions),Avg=sum/N)
  chartsNEIBL<-lapply(tailquantile,chart)
  saveRDS(chartsNEIBL,file="chartsNEIBL.RDS")
}else {
  chartsNEIBL<-readRDS("chartsNEIBL.RDS")
}

## Create new variable to show the development per each city for all measurements
LA<-chartsNEIBL[[50]][chartsNEIBL[[50]]$fips=="06037",]
BA<-chartsNEIBL[[50]][chartsNEIBL[[50]]$fips=="24510",]


####################################################################################
## Making the ggplot:

###
png(filename = "plot6.png",width=600,height = 480)


ggplot()+
  geom_col(data=LA,aes(x=year, y=sum),fill=rgb(.5,.5,.5,.5))+
  geom_col(data=BA,aes(x=year, y=sum),fill=rgb(.25,.5,.75,.5))+
  xlab("Year")+ 
  ylab("Sum of the motor-vehicle-related\nPM2.5-Emission-Data [tons]")+ 
  ggtitle("Change in the motor-vehicle-related PM2,5-Emission-Data\nfor Baltimore and LA by Year")+ 
  annotate("text", x = c(1999,2002,2005,2008), y = c(200,100,100,100), label = "Baltimore",size = 3)+
  annotate("text", x = c(1999,2002,2005,2008), y = c(3700,4100,4400,3900), label = "LA",size = 3)+
  scale_x_continuous(name="Year",breaks=c(1999,2002,2005,2008))+
  theme(axis.title.y = element_text(size=12))

dev.off()


