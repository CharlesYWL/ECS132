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
#####P2

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
  #Assume ace is all 1, so there are 4 1s
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