set.seed(99) 

FirstCube <- sample(1:6,1000, replace = T)
SecondCube <- sample(1:6,1000, replace = T)
combineDice <- cbind(FirstCube, SecondCube)
SumOfDices <- rowSums(combineDice)

#Challenge 1

evenDigits <- ifelse(SumOfDices%%2==0, T, F)
oddDigits <- ifelse(SumOfDices%%2!=0, T, F)
numberOdd <- length(SumOfDices[oddDigits])
numbereven <- length(SumOfDices[evenDigits])
ans <- cbind("Number Odd" = numberOdd, "Number Even" = numbereven)

#Challenege 2

frequency_2 <- length(SumOfDices[SumOfDices == 2])
frequency_3 <- length(SumOfDices[SumOfDices == 3])
frequency_4 <- length(SumOfDices[SumOfDices == 4])
frequency_5 <- length(SumOfDices[SumOfDices == 5])
frequency_6 <- length(SumOfDices[SumOfDices == 6])
frequency_7 <- length(SumOfDices[SumOfDices == 7])
frequency_8 <- length(SumOfDices[SumOfDices == 8])
frequency_9 <- length(SumOfDices[SumOfDices == 9])
frequency_10 <- length(SumOfDices[SumOfDices == 10])
frequency_11 <- length(SumOfDices[SumOfDices == 11])
frequency_12 <- length(SumOfDices[SumOfDices == 12])

ans1 <- cbind("2" = frequency_2, "3" = frequency_3, "4" = frequency_4, "5" = frequency_5, "6" = frequency_6, 
              "7" = frequency_7, "8" = frequency_8, "9" = frequency_9, "10" = frequency_10, "11" = frequency_11, "12" = frequency_12)

#Challenge 3

location <- c(rep(F, times = 99),T)
finalLocation <- rep(location, times = 10)
finalLocation[1] <- T
ans2 <- SumOfDices[finalLocation]
