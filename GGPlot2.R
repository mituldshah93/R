install.packages("readxl")
library(readxl)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)
orig_list <- data.frame(readxl::read_excel("D:\\Games\\titanic3_assignment.xls"))
plist <- orig_list
dim(plist)
summary(plist)

#1 Convert Mode of Survived to Logical
mode(plist$survived) <- "logical"

#2 Change	class	to	string
class(plist$pclass) <- "character"
plist$pclass <- ifelse(plist$pclass == "1", "First", ifelse(plist$pclass == "2", "Second", "Third"))
unique(plist$pclass)

#3 Simple	imputation	of	age	(mean	of all ages)
plist$age[is.na(plist$age)] <- mean(plist$age,na.rm = T)

#4 Simple	imputation	of	fare (mean	of all	fares)
plist$fare[is.na(plist$fare)] <- mean(plist$fare,na.rm = T)

#5 Simple	imputation	of	place	of embarking 	(randomly	generated)	with	seed	of	99	
set.seed(99)
plist$embarked[is.na(plist$embarked)] <- sample(c("S", "C", "Q"), length(is.na(plist$embarked)), replace = T)

#6 Create	new	category	(age	cohort)	
plist$age_cohort <- ifelse(plist$age < 16, "Child", ifelse(plist$age >= 60, "Elderly", "Adult"))
summary(plist)

#7 Put	in	full	town	origin	(Queenstown(Q)	replaced	by	Cobh)
plist$embarked <- ifelse(plist$embarked == "S", "Southampton", ifelse(plist$embarked == "C", "Cherbourg", "Cobh"))
unique(plist$embarked)

#8 Double	check	dataset
head(plist)
dim(plist)
table(plist$survived, plist$sex)
table(plist$survived)
table(plist$survived, plist$pclass)
table(plist$survived, plist$age_cohort)
table(plist$survived, plist$embarked)

#Plot 1 : Survival Numbers by Travel Class
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = pclass)) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Travel Class") + theme(legend.position = "top", legend.title = element_blank())

#Plot 2 : Survival Numbers by Gender
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = sex)) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Gender") + theme(legend.position = "top", legend.title = element_blank())

#Plot 3 : Survival Numbers by Age Cohort
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = age_cohort)) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Age Cohort") + theme(legend.position = "top", legend.title = element_blank())

#Plot 4 : Survival Numbers by Embarkation Location
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = embarked)) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Embarkation Location") + theme(legend.position = "top", legend.title = element_blank())
###### Note : Here the Title of the Plot and Legend Parameters do not match, ideally it might be Survival Numbers by Class, I have done the Opration on class but keeping the title as per the Assignment Document.

#Plot 5 : Survival Proportions by Class
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = pclass), position = "fill") + xlab("Survived") + ylab("Proportion") + ggtitle("Survival Proportions by Embarkation Location") + theme(legend.position = "top", legend.title = element_blank())

#plot 6 : Survival Proportions by Gender
ggplot(data = plist) + geom_bar(mapping = aes(x = survived,fill = sex), position = "fill") + xlab("Survived") + ylab("Proportion") + ggtitle("Survival Proportions by Gender") + theme(legend.position = "top", legend.title = element_blank())

#Plot 7 : Survival Proportions by Age Cohort
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = age_cohort), position = "fill") + xlab("Survived") + ylab("Proportion") + ggtitle("Survival Proportions by Age Cohort") + theme(legend.position = "top", legend.title = element_blank())

#Plot 8 : Survival Proportions by Place of Embarkation
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = embarked), position = "fill") + xlab("Survived") + ylab("Proportion") + ggtitle("Survival Proportions by Place of Embarkation") + theme(legend.position = "top", legend.title = element_blank())
###### Note : Here the Title according to assignment suggests "p", instread of "P" for Place of Embarkation, I have Mentioned as Upper case 'P' for Camel Case

#Plot 9 : Survival Numbers by Cohort and Travel Class
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = age_cohort)) + facet_wrap(~pclass) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Cohort and Travel Class") + theme(legend.position = "top", legend.title = element_blank())

#Plot 10 : Survival Numbers by Gender and Travel Class
ggplot(data = plist) + geom_bar(mapping = aes(x = survived, fill = sex)) + facet_wrap(~pclass) + xlab("Survived") + ylab("Number") + ggtitle("Survival Numbers by Gender and Travel Class") + theme(legend.position = "top", legend.title = element_blank())

#Plot 11 : Age V Fare by Place of Embarkation
ggplot(data = plist) + geom_point(mapping = aes(x = age, y = fare, colour = embarked)) + xlab("Age") + ylab("Fare") + ggtitle("Age v Fare by Place of Embarkation") + theme(legend.position = "top", legend.title = element_blank())

#Plot 12 : Age V Fare with Linear Model
ggplot(data = plist, aes(x = age, y = fare)) + geom_point() + geom_smooth(method = 'lm')  + xlab("Age") + ylab("Fare") + ggtitle("Age v Fare with Linear Model")

#Plot 13 : Age V Fare with Survival Info
ggplot(data = plist) + geom_point(mapping = aes(x = age, y = fare, colour = survived))  + xlab("Age") + ylab("Fare") + ggtitle("Age v Fare with Survival Info") + theme(legend.position = "top", legend.title = element_blank())

#Plot 14 : Age V Fare by Travel Class and Point of Departure
ggplot(data = plist) + geom_point(mapping = aes(x = age, y = fare, colour = embarked)) + facet_wrap(~pclass) + xlab("Age") + ylab("Fare") + ggtitle("Age v Fare by Travel Class and Departure") + theme(legend.position = "top", legend.title = element_blank())