set.seed(1000)
ids <- rep(as.character(1001:1005,2))
module <- c(rep("CT101",5),rep("CT102",5))
result <- c(rnorm(n = 5,mean = 70,sd = 5),rnorm(n = 5,mean = 50,sd = 8))
result<-c(NA,result[2:length(result)])
dataf <- data.frame(ids,module,result)
na.rm <- F

my_function <- function(df, group_id,data_id,f,na.rm=F){
  
  if(class(df)=="data.frame"){
    if(any(group_id==colnames(df))==T){
      if(class(df[[group_id]])=="factor"){
        if(any(data_id==colnames(df))==T){
          if(class(df[[data_id]])=="numeric"){
            if(class(f)=="function"){
              if(na.rm==T){
                tapply(df[[data_id]],df[[group_id]],f,na.rm=T)
              }else {
                tapply(df[[data_id]],df[[group_id]],f)
              }
            }else {stop("Error ",f," is not a Function")}
          }else {stop("Error ",data_id," is not a Numeric column")}
        }else {stop("Error ",data_id," is not a Valid column of the Data Frame")}
      } else {stop("Error ",group_id," is not a categorical column of the Data Frame")}
    } else {stop("Error ",group_id," is not a valid column")}
  } else {stop("First parameter is not a data frame object")}
}

#Test Cases
my_function(dataf, "module", "result", mean)
my_function(dataf, "module", "result", mean, na.rm=T)
my_function(dataf, "ids", "result", mean)
my_function(dataf, "ids", "result", mean,na.rm=T)
my_function(1:10, "module", "result", mean, na.rm=T)
my_function(dataf, "modul", "result", mean)
my_function(dataf, "module", "ids", mean)
my_function(dataf, "module", "result", 10)