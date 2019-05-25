library(pryr)

myvec <- function(v){
  if(!is.numeric(v))
    stop("Error, data type must be numeric.")
  structure(list(data=v),class="myvec")
}

x<-myvec(1:10)

# Print Function
print.myvec <- function(x){
  cat("S3 Class = ", class(x), "\n")
  cat("Number of elements = ", length(x$data), "\n")
  cat(x$data)
}

x
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Override "[" and "[<-"

`[.myvec` <- function(a,b) {a$data[b]}
x[1:2]

`[<-.myvec` <- function(a,b,value = c) {
  a$data[b] <- value
  a
  }
x[1:2] <- 0

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Override "<", "<=", ">", ">=", "!=" and "=="
`<.myvec` <- function(a,b) {
  a$data < b
}
x[x<1]
#+++++++++++++++++++++++++++++++++++++++++++++
`<=.myvec` <- function(a,b){
  a$data <= b
}
x[x<=5]
#+++++++++++++++++++++++++++++++++++++++++++++
`>.myvec` <- function(a,b) {
  a$data > b
}
x[x>5]
#+++++++++++++++++++++++++++++++++++++++++++++
`>=.myvec` <- function(a,b){
  a$data >= b
}
x[x>=5]
#+++++++++++++++++++++++++++++++++++++++++++++
`!=.myvec` <- function(a,b){
  a$data != b
}
x[x!=0]
#+++++++++++++++++++++++++++++++++++++++++++++
`==.myvec` <- function(a,b){
  a$data == b
}
x[x==0]
x[1]<-NA

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Override "Sum" and "Mean" with and Without na.rm = T

`sum.myvec` <- function(a, na.rm = F){
  if(na.rm == T){sum(a$data, na.rm = T)}
  else{
  if(is.na(a)){sum(a$data)}
  else {sum(a$data)}}
}

sum(x)

sum(x, na.rm = T)

#+++++++++++++++++++++++++++++++++++++++++++++

`mean.myvec` <- function(a, na.rm = F){
  if(na.rm == T){mean(a$data, na.rm = T)}
  else{
    if(is.na(a)){mean(a$data)}
    else {mean(a$data)}}
}

mean(x)

mean(x, na.rm = T)