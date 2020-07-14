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