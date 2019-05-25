# (1) Include	the	following	libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

#(2) Load	in	the	energy	dataset,	contained	in	the	CT5102	github	account
ener <- read_excel(file.choose())
ener

#(3) Add	new	features	to	the	energy	data	set
ener$Date <- strftime(ener$DateTime, format = "%Y-%m-%d")
ener$Time <- strftime(ener$DateTime, format = "%H:%M:%s")
ener$HourOfDay <- hour(ener$DateTime)
ener$MinuteOfDay <- minute(ener$DateTime)
ener$DayOfWeek <- wday(ener$DateTime, label = T)
ener$NIFlow <- ifelse(ener$NetImports > 0, "Importing", "Exporting")

#(4) Plot	the	net	import	data	over	time,	colour	by	NIFlow,	and	facet	by	day	of	the	week
ggplot(data = ener) + geom_point(mapping = aes(x = HourOfDay, y = NetImports, colour = NIFlow)) + facet_wrap(~DayOfWeek) + xlab("Time (Hour of Day)") +ylab("Net Imports") + ggtitle("Time v Net Imports By Day of Week") + scale_color_hue(labels = c("Exports","Imports"))

#(5) Plot	the	wind	generation	vs	CO2 Emissions
ggplot(data = ener) + geom_point(mapping = aes(x = Wind, y = CO2, colour = NIFlow)) + xlab("Wind Generation") +ylab("CO2 Emissions") + ggtitle("Wind Generation v CO2 Emissions") + scale_color_hue(labels = c("Exports","Imports"))

#(6) Load	in	the	weather	data	set
weather <- read_excel(file.choose())
weather

#(7) Convert	the	Date	(dttm)	to	(date)	format
weather$Date <- as.Date(weather$Date)

#(8) Plot	the	average	wind	speed :
###Providing End Bracket ")" to the 'Knots' which is not displayed in the assignment
ggplot(data = weather) + geom_line(mapping = aes(x = Date, y = AVRWind), linetype = 2) + geom_point(mapping = aes(x = Date, y = AVRWind),colour = "blue") + ylab("Average Wind Speed (Knots)")

#(9) Generate	 the	average	daily	wind	generation	 from	 the	energy	data,	and	ensure	 that	the	Date	column	is	of	type <date>

avr_daily_wind <- ener %>% group_by(Date = date(Date)) %>% summarise(AverageWindGeneration = mean(Wind))
avr_daily_wind

#(10) Join	the	new	datasets	and	produce	the	following	plots
Joined_data_set <- inner_join(weather, avr_daily_wind, by="Date")
Joined_data_set

#Plot for Average Wind Generation and Wind Speed
ggplot(data = Joined_data_set, mapping = aes(x = AVRWind, y = AverageWindGeneration)) + geom_point() + xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + ggtitle("Wind Speed v Wind Power Generated")

#Plot for Average Wind Generation and Wind Speed with Linear Model
ggplot(data = Joined_data_set, mapping = aes(x = AVRWind, y = AverageWindGeneration)) + geom_point() + geom_smooth(method = "lm")+ xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + ggtitle("Wind Speed v Wind Power Generated, with linear model")

#Plot for Average Wind Generation and Wind Speed with Loess Model
ggplot(data = Joined_data_set, mapping = aes(x = AVRWind, y = AverageWindGeneration)) + geom_point() + geom_smooth(method = "loess")+ xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + ggtitle("Wind Speed V Wind Power Generated, with loess model")
