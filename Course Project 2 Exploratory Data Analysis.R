############################################
# ____________Project 2._________________
###########################################

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


##############################
#_____Question 2.

	# Have total emissions from PM2.5 decreased in the Baltimore City, 
	# Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") 
	# from 1999 to 2008? Use the base plotting system to make a plot 
	# answering this question.

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]

NEI[fips=='24510', lapply(.SD, sum, na.rm = TRUE)
	, .SDcols = c("Emissions")
	, by = year]->baltimore_data_NEI
baltimore_data_NEI[, Emissions]->emissions_b
baltimore_data_NEI[,year]->year_b

png(filename='plot2.png')

gph_2<-barplot(emissions_b, names=year_b, xlab="Year"
		,ylab=expression("PM"[2.5]*" emissions in kilotons")
		, main=expression("PM"[2.5]*"Emissions per Year in Baltimore.")
		, col=gray((1:4)/6))

text(x = gph_2, y = round(emissions_b/1.4,2)
	, label = round(emissions_b/1000,2)
	, pos = 3, cex = 0.8, col = "white")
dev.off()



##############################
#____________Question 3. 

library(ggplot2)
library(dplyr)
library(ggthemes)

summarise(group_by(filter(NEI, fips == "24510"), year,type), Emissions=sum(Emissions))->baltimore_data

png("plot3.png", width = 12,height = 6, units = "in", res = 600)

ggplot(baltimore_data,aes(factor(year),Emissions,fill=type, label=round(Emissions,2))) +
  geom_bar(stat="identity") +
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission.")) + 
  labs(title=expression("PM"[2.5]*" Emissions in Baltimore, 1999-2008."))+
  geom_label(aes(fill = type), colour = "white", fontface = "bold")+
  theme_solarized()

dev.off()



##############################
# ___________Question 4.

	# Across the United States, 
	# how have emissions from coal combustion-related sources changed from 1999–2008?


library(ggplot2)
library(dplyr)
library(ggthemes)

grepl("Fuel Comb.*Coal", SCC$EI.Sector)->coal;SCC[coal,]->coal
head(coal)

NEI[(NEI$SCC %in% coal$SCC), ]->coal_related
summarise(group_by(coal_related, year), Emissions=sum(Emissions))->coal_related

png("plot4.png")

ggplot(coal_related,aes(x = factor(year),y = Emissions/10^5,label = round(Emissions/1000,2))) +
  geom_bar(stat="identity", width=0.75, fill =gray((1:4)/6)) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission.")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))+
  geom_label(colour = "black", fontface = "italic")+theme_solarized()

dev.off()


##############################
#_______Question 5.

	# How have emissions from motor vehicle 
	# sources changed from 1999–2008 in Baltimore City?


library(ggplot2)
library(dplyr)
library(ggthemes)

NEI[(NEI$fips=="24510") & (NEI$type=="ON-ROAD"),]->baltimore_emissions

summarise(group_by(baltimore_emissions, year), Emissions=sum(Emissions))->baltimore_emissions

png("plot5.png")

ggplot(baltimore_emissions,aes(factor(year),Emissions, label=round(Emissions,2))) +
  geom_bar(stat="identity" ,width=0.75,fill =gray((1:4)/6)) +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission.")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore, 1999-2008"))+
  geom_label(colour = "black", fontface = "italic")+theme_solarized()

  

dev.off()



###############################
#___________Question 6.

	# Compare emissions from motor vehicle sources in Baltimore City 
	# with emissions from motor vehicle sources in Los Angeles County, 
	# California (\color{red}{\verb|fips == "06037"|}fips == "06037"). 
	# Which city has seen greater changes over time in motor vehicle emissions?

library(ggplot2)
library(dplyr)
library(ggthemes)


summarise(group_by(filter(NEI, fips == "24510"& type == 'ON-ROAD'), year)
	, Emissions=sum(Emissions))->baltimore_emissions; baltimore_emissions$County <- "Baltimore City, MD"

summarise(group_by(filter(NEI, fips == "06037"& type == 'ON-ROAD'), year)
	, Emissions=sum(Emissions))->LA_emissions; LA_emissions$County <- "Los Angeles County, CA"

emissions <- rbind( LA_emissions,baltimore_emissions)

png("plot6.png")

ggplot(emissions, aes(x=factor(year), y=Emissions,label = round(Emissions,2))) +
    geom_bar(stat="identity", fill ="firebrick") + 
    facet_grid(.~County, scales="free", space="free") +
    ylab(expression("total PM"[2.5]*" emissions in kilotons.")) + 
    xlab("year") +
    ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles."))+
    geom_label(colour = "black", fontface = "bold",vjust = 0)+
    theme_economist()

dev.off()
