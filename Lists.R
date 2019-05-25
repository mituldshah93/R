#Assignment 2:

set.seed(111)


v <- sample(c('M','F'),10,prob = c(.3,.7),replace = T)
na.rm = F
my_table <- function(v,na.rm=F){
  
  if(na.rm == F){
  
  if(any(is.na(v))) {
    stop(" Error, the input vector has NA element(s)")
                    }
  else if(!is.character(v)) {
    stop("Error, input must be a character vector")
  }
  }
  else if(na.rm == T){
    v <- v[!is.na(v)]
    fem <- length(v[v == 'F'])
    mal <- length(v[v == 'M'])
    result1 <- cbind("F" = fem, "M" = mal)
    result1
    }
  fem <- length(v[v == 'F'])
  mal <- length(v[v == 'M'])
  result1 <- cbind("F" = fem, "M" = mal)
  result1
}