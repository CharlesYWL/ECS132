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
mean(getL2list(10000))

####2
toss <- function(){
  headaccu <- 0
  round <- 0
  testls <- c()
  while (headaccu < 3 & round < 6) {
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
gettosslist <- function(num){
  list <- c()
  i <- 0
  while (i < num) {
    list <- c(list ,toss())
    i <- i+1
  }
  return(list)
}
mean(gettosslist(10000))

###P3
library(gtools)
fir <- function(z){z[1]}
permn <- function(x,m,FUN){
  ls <- permutations(n = length(x), r = m, x)
  rs <- c()
  count <- length(ls)/m
  for (i in 1:count) {
    rs <- c(rs,FUN(ls[i,]))
  }
#  print(ls)
  return(rs)
}
permn(7:10,2,fir)



