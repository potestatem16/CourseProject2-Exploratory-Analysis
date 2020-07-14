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