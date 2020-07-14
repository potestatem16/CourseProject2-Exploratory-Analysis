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