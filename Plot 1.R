setwd("C:/Users/aleja/Documents/Cursos/Coursera R pratices")

library(data.table)

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url=url, destfile=paste(getwd(), "Data_For_Project2.zip", sep = "/"))

list.files(path=getwd(), pattern="Data_For_Project2")

unzip(zipfile="Data_For_Project2.zip")

SCC <- as.data.table(x=readRDS(file="Source_Classification_Code.rds"))
NEI <- as.data.table(x=readRDS(file="summarySCC_PM25.rds"))
z <- sample(1:4, 100, T)

head(SCC)
head(NEI)

#######################
# _______Question 1. 

	#Have total emissions from PM2.5 decreased in the United States from 1999 
	#to 2008? 
	#Using the base plotting system, make a plot showing the total PM2.5 
	#emission from all sources for each of the years 1999, 2002, 2005, and 2008.

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols=c("Emissions")]
NEI[, lapply(.SD, sum, na.rm=T), .SDcols=c("Emissions"), by=year]->year_Total_NEI
year_Total_NEI[,Emissions]->emissions
year_Total_NEI[,year]->year


png(filename='plot1.png')

gph_1<-barplot(emissions, names=year, xlab="Year"
		,ylab=expression("PM"[2.5]*" emissions in kilotons")
		, main=expression("PM"[2.5]*"Emissions per Year.")
		, col=gray((1:4)/6))

text(x = gph_1, y = round(emissions/1.4,2)
	, label = round(emissions/1000,2)
	, pos = 3, cex = 0.8, col = "white")
dev.off()