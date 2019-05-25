set.seed(10)

N=10

cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)

m <- matrix(c(cs1,cs2,cs3,cs4,cs5),ncol=5)
colnames(m) <- c('cs1','cs2','cs3','cs4','cs5')
rownames(m) <- 1:10

RanksFun <- function(x){
  x1 <- x
  names(x1) <- 1:10
  mal <- sort(x,decreasing = T)
  csfirst <- as.numeric(names(mal))
  names(csfirst) <- 1:10
  nameofcol <- names(sort(csfirst))
  x1 <- as.numeric(nameofcol)
}

mrank <- apply(m,2,RanksFun)
rownames(mrank) <- c("Student#1","Student#2","Student#3","Student#4","Student#5",
                   "Student#6","Student#7","Student#8","Student#9","Student#10")

median <- apply(mrank,1,median)
answer <- cbind(mrank,median)

m
mrank
answer