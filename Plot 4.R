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