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