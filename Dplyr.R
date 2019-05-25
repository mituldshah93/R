#Task 1 : Including Libraries

library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

#Task 2 : Creating a local copy of the Flights tibble

my_flights <- flights
my_flights

#Task 3 : First Page
  #Filter	 out	 missing	 values	 for	 dep_delay	 and	 arr_delay,	 and	 select	 the	 following columns	from	the	data	set:	time_hour,	origin,	dest,	carrier,	dep_delay,	arr_delay,	air_time,	distance

my_flights <- select(filter(my_flights[complete.cases(my_flights$dep_delay) & complete.cases(my_flights$arr_delay),]), time_hour,origin,dest,carrier,dep_delay,arr_delay,air_time,distance)
my_flights

#Task 3 : Second Page

  #Add Day of Week column
my_flights$DayOfWeek <- wday(my_flights$time_hour, label = T)

  #Add Hour of the Day Column
my_flights$HourOfDay <- hour(my_flights$time_hour)

  #Add Hour of the Day Column
my_flights$Month <- month(my_flights$time_hour, label = T, abbr = T)

select(my_flights,time_hour,DayOfWeek,HourOfDay,everything())

#Task 4 : Average	departure	delay	statistics	by	hour	of	day,	ordered	by	delay
delay_hourly <- my_flights[,c("HourOfDay","dep_delay")]
delay_hourly <- delay_hourly %>% group_by(HourOfDay,add = T) %>% summarise(AvrDepDelay = mean(dep_delay), SD = sd(dep_delay), MinDelay = min(dep_delay), MaxDelay = max(dep_delay), MaxDelayHours = MaxDelay/60) %>% arrange(desc(AvrDepDelay))

#Task 5 : Average	departure	delay	statistics	by	month,	ordered	by	delay.
delay_monthly <- my_flights[,c("Month","dep_delay")]
delay_monthly <- delay_monthly %>% group_by(Month,add = T) %>% summarise(AvrDepDelay = mean(dep_delay), SD = sd(dep_delay), MinDelay = min(dep_delay), MaxDelay = max(dep_delay), MaxDelayHours = MaxDelay/60) %>% arrange(desc(AvrDepDelay))

#Task 6 : 
delay_carrier <- my_flights[,c("carrier","dep_delay")]
delay_carrier <- delay_carrier %>% group_by(carrier,add = T) %>% summarise(AvrDepDelay = mean(dep_delay), SD = sd(dep_delay), MinDelay = min(dep_delay), MaxDelay = max(dep_delay), MaxDelayHours = MaxDelay/60, NObs = n()) %>% arrange(desc(AvrDepDelay))

#Task 7 : Average	departure	delay	statistics	by	airport	by	month,	ordered	by	delay.
delay_airport_month <- my_flights[,c("origin","Month","dep_delay")]
delay_airport_month <- delay_airport_month %>% group_by(origin,Month, add = T) %>% summarise(AvrDepDelay = mean(dep_delay), SD = sd(dep_delay), MinDelay = min(dep_delay), MaxDelay = max(dep_delay), MaxDelayHours = MaxDelay/60, NObs = n()) %>% arrange(desc(AvrDepDelay))

#Task 8 : Average	departure	delay	statistics	by	airport	by	month,	ordered	by	delay.
delay_airport_time1 <- my_flights[,c("HourOfDay","origin","dep_delay")]
delay_airport_time1 <- delay_airport_time1 %>% group_by(HourOfDay,origin, add = T) %>%summarise(AvrDepDelay = mean(dep_delay), SD = sd(dep_delay), MinDelay = min(dep_delay), MaxDelay = max(dep_delay), MaxDelayHours = MaxDelay/60, NObs = n()) %>% arrange(HourOfDay)

#Task 9 : Add	a	new	category,	which	divides	each	day	into	three	sections(use case_when) : 
  # Morning 5 <= time < 12, Afternoon 12 <= time < 18, Evening >= 18 
DaySection = case_when(
  5 <= my_flights$HourOfDay & my_flights$HourOfDay < 12 ~ "Morning",
  12 <= my_flights$HourOfDay & my_flights$HourOfDay < 18 ~ "Afternoon",
  my_flights$HourOfDay >= 18 ~ "Evening"
)

my_flights <- my_flights %>% mutate(DaySection)

select(my_flights,DaySection,everything())

#Task 10 : Create	 a	 sample	 dataset (using	 sample_n()),	 and	 remove	 all	 departure	 delay values	greater	that	180	minutes.
set.seed(99)
myf_sample <- sample_n(my_flights, size = 10000) %>% filter(dep_delay <= 180)
myf_sample

#Task 11: Use	 a	 boxplot	 to	 visualise the	 departure	 delay	 by	 the	 three	 different	 time	sections.
ggplot(myf_sample) + geom_boxplot(mapping = aes(x=Month, y=dep_delay, colour = DaySection)) + xlab("Month") + ylab("Departure Delay")
