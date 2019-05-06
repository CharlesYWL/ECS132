#Weili Yin,912603171
#####P1
sample(0:4,prob=c(.292,.4096,.2312,.068,.064))

sample(0:4,1,prob=c(.292,.4096,.2312,.068,.064))

sample(0:2,2,prob = c(.5,.4,.1))

ls1 <- sample(0:2,10000,prob = c(.5,.4,.1),replace = TRUE)
var(ls1)

getL2 <- function(){
  L1 <- sample(0:2,1,prob = c(.5,.4,.1))
  NumofPeople <- L1
  i <- 0
  while (i<NumofPeople) {
    if(runif(1) < .2){ #quit one people
      L1 <- L1-1
    }
    i <- i+1
  }
  return(L1+sample(0:2,1,prob=c(.5,.4,.1)))
}
getL2list <- function(num){
  list <- c()
  i <- 0
  while (i < num) {
    list <- c(list ,getL2())
    i <- i+1
  }
  return(list)
}
var(getL2list(10000))

getL2L1 <-function(){
  L1 <- sample(0:2,1,prob = c(.5,.4,.1))
  NumofPeople <- L1
  initL1 <- L1
  i <- 0
  while (i<NumofPeople) {
    if(runif(1) < .2){ #quit one people
      L1 <- L1-1
    }
    i <- i+1
  }
  return(L1+sample(0:2,1,prob=c(.5,.4,.1))-initL1)
}
getL2L1list <- function(num){
  list <- c()
  i <- 0
  while (i < num) {
    list <- c(list ,getL2L1())
    i <- i+1
  }
  return(list)
}
var(getL2L1list(10000))

sim1 <- function(nreps){
  ls1 <- sample(0:2,nreps,prob = c(.5,.4,.1),replace = TRUE)
  cat("Var(L1) = ", var(ls1),'\n')
  cat("Var(L2) = ", var(getL2list(nreps)),'\n')
  cat("Var(L2-L1) = ",var(getL2L1list(nreps)),'\n') 
}

####2
toss <- function(r,s){
  headaccu <- 0
  round <- 0
  testls <- c()
  while (headaccu < r & round < s) {
    if(runif(1) <.5){ #get HEAD
      headaccu <- headaccu+1
      testls <- c('H',testls)
    }else{
      headaccu <- 0
      testls <- c('T',testls)
    }
    round <- round +1     
  }
  #print(testls)
  return(round)
}
gettosslist <- function(r,s,num){
  list <- c()
  i <- 0
  while (i < num) {
    list <- c(list ,toss(r,s))
    i <- i+1
  }
  return(list)
}
sim2 <- function(r,s,nreps) {
  return(mean(gettosslist(r,s,nreps)))
}


###P3.1
library(gtools)
origin <- function(list){return(list)}
permn <- function(x,m,FUN){
  ls <- permutations(n = length(x), r = m, x)
  rs <- NA
  count <- length(ls)/m
  for (i in 1:count) {
    rs[i] <- FUN(ls[i])
  }
  return(rs)
}
perm = function(n, x) {
  factorial(n) / factorial(n-x)
}
### Below are for P3.2
#fun <- function(list){ 
#  sum <- numeric(1)
#  sum <- 0
#  i <- 1
#  while (i < 8) {
#    sum <- sum + abs(list[i+1]-list[i])
#    i <- i+1
#  }
#  return(sum)
#}

###P5
library(ggplot2)
drawplot <-function(){
  num <- c(1:10)
  possibility <- NA
  sum <- 0
  for (i in 1:10) {
    possibility[i] <- dbinom(i,10,.97)
  }
  df <- data.frame(num,possibility)
  df$possibility <- as.factor(df$possibility)
  head(df)
  ggplot(df, aes(x=num, y=possibility)) + geom_point()
}
#drawplot()