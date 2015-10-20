Mean1 <- function(x){
  sum(x)*(1/length(x))
}

Mean2 <- function(x){
  sum(x)/length(x)
}

Mean3 <- function(x){
  sumx <- 0
  for(i in 1:length(x)){
    sumx <- sumx + x[i]
  }
  meanx <- sumx/length(x)
  return(meanx)
}


x  <- round(rnorm(25, 10,5))
x
mean(x)
Mean1(x)
Mean2(x)



Mean3(x)
