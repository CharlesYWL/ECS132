#Weili Yin,912603171
#####P1
pik <- function(i,k) {
  if(k==1){
    #basecase only one more step
    return((i-1)*(1/6))
  }else{
    accu <- 0
    x <- 1
      #add all possi up
      while(x<=7-i & x <=6){
        accu <- accu + (1/6)*pik(i+x,k-1)
        x <- x+1
      }
      return(accu)
  }
}
#####P2:
sim <- function(p,q,nreps){
  collision_0 <- 0
  collision_1 <- 0
  collision_2 <- 0
  countx2eq2 <- 0
  countx1eq1 <- 0
  countx1eq2 <- 0
  countx2eq2givx1eq1 <- 0
  nx1eqx2 <- 0
  for (i in 1:nreps) {
    numsend <- 0
    numcollision <- 0
    for (j in 1:2) {
      if (runif(1) < p) {
        numsend <- numsend + 1}
    }
    if (numsend==1) {
      X1 <- 1
    }else{
      if (numsend == 2) {
        numcollision <- numcollision + 1}
      X1 <- 2
    }
    if (X1==2) {
      countx1eq2 <- countx1eq2 + 1}
    numactive <- X1
    if (X1 == 1 && runif(1) < q) {
      numactive <- numactive + 1}
    if (numactive == 1) {
      if (runif(1) < p) {
        X2 <- 0
      }else{
        X2 <- 1
      }
    }else{
      numsend <- 0
      for (j in 1:2) {
        if (runif(1) < p) {
          numsend <- numsend + 1}
      }
      if (numsend == 1) {
        X2 <- 1
      }else{
        if (numsend == 2) {
          numcollision <- numcollision + 1}
        X2 <- 2
      }
    }
    if (X2==2) {
      countx2eq2 <- countx2eq2 + 1}
    if (X1==1) {
      countx1eq1 <- countx1eq1 + 1
      if (X2==2) {
        countx2eq2givx1eq1 <- countx2eq2givx1eq1 + 1}
    }
    if (X1==X2) {
      nx1eqx2 <- nx1eqx2 + 1
      if (numcollision == 2) {
        collision_2 <- collision_2 + 1
      }else if(numcollision == 1) {
        collision_1 <- collision_1 + 1
      }else{
        collision_0 <- collision_0 + 1
      }
    }
  }
  cat("P(X1 = 2):",countx1eq2/nreps,"\n")
  cat("P(X2 = 2):",countx2eq2/nreps,"\n")
  cat("P(X2 = 2 | X1 = 1):",countx2eq2givx1eq1/nreps,"\n")
  cat("P(C=0 | X1 = X2):",collision_0/nx1eqx2,"\n")
  cat("P(C=1 | X1 = X2):",collision_1/nx1eqx2,"\n")
  cat("P(C=2 | X1 = X2):",collision_2/nx1eqx2,"\n")
}

#####P3:
#used to check number
count <- function(ls,target){
  i <- 0
  for(v in ls){
    if(v==target)
      i <- i+1
  }
  return(i)
}
#draw one time and check
drawOneAce_onetime <- function(){
  ls <- c(1,1,1,1,5:52)
  #Assume ace is all 1, so there are 4 1s
  rs <- sample(ls,5)
  if(count(rs,1)==1)
    return(TRUE)
  else
    return(FALSE)
}
drawOneAce <- function(){
  i <- 1:10000
  ls <- c()
  for(x in i){
    ls <- c(ls,drawOneAce_onetime())
  }
  return(count(ls,TRUE)/10000)
}
#draw one time for 3 diomands
drawThreeDio_onetime <- function(){
  ls <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,14:52)
  #Assume diomands is all 1, so there are 13 1s
  rs <- sample(ls,5)
  if(count(rs,1)==3)
    return(TRUE)
  else
    return(FALSE)
}
drawThreeDio <- function(){
  i <- 1:10000
  ls <- c()
  for(x in i){
    ls <- c(ls,drawThreeDio_onetime())
  }
  return(count(ls,TRUE)/10000)
}