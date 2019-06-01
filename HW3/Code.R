#Weili Yin,912603171,HW3
library(ggplot2)
#####A
sim1<- function(nreps,p, k){
  count = 0
  list<-rgeom(nreps,p)
  for (variable in list) {
    if(variable<=k)
      count <- count+1
  }
  return(count/nreps)
}
sim1(100000,.2,3)
####C
mlb <- read.table("./dnc-corecipient/out.dnc-corecipient",skip = 1)
mlb <- mlb[mlb$V1<mlb$V2,] #filter

ggplot(mlb,aes(x=,y=V1))+
  geom_point(data=mlb)

####D
daccum <- function(i,k){
  if(i==1)
    return(daccum_help(k))
  else if(k<=1)
    return(1)
  else{ #recursion
    pos=1/36*daccum(i-1,k-2)+2/36*daccum(i-1,k-3)+
      3/36*daccum(i-1,k-4)+4/36*daccum(i-1,k-5)+
      5/36*daccum(i-1,k-6)+6/36*daccum(i-1,k-7)+
      5/36*daccum(i-1,k-8)+4/36*daccum(i-1,k-9)+
      3/36*daccum(i-1,k-10)+2/36*daccum(i-1,k-11)+
      1/36*daccum(i-1,k-12)
  }
  return(pos)
}
##when i=1, which is basecase
daccum_help <- function(k){
  if (k==2) {
    return(1)
  }else if(k==3){
    return(1-1/36)
  }else if(k==4){
    return(1-2/36-1/36)
  }else if(k==5){
    return(1-(3+2+1)/36)
  }else if(k==6){
    return(1-(4+3+2+1)/36)
  }else if(k==7){
    return(1-(5+4+3+2+1)/36)
  }else if(k==8){
    return(1-(6+5+4+3+2+1)/36)
  }else if(k==9){
    return(1-(5+6+5+4+3+2+1)/36)
  }else if(k==10){
    return(1-(4+5+6+5+4+3+2+1)/36)
  }else if(k==11){
    return(1-(3+4+5+6+5+4+3+2+1)/36)
  }else if(k==12){
    return(1/36)
  }else if(k<=1){
    return(1)
  }else{
    return(0)
  }
}
###paccum is accumulation of daccum
paccum <- function(i,k){
  accu <- 0
  for (variable in 1:i) {
    accu <- accu + daccum(variable,k)
  }
  return(accu)
}
###qaccum is inverse of paccum



###raccum just simulation
raccum <- function(nreps,k){
  ls <- NULL
  for (variable in 1:nreps) {
    ls[variable]=generate_once(k)
  }
  return(ls)
}
generate_once <- function(k){
  count <- 0
  sum <- 0
  flag <- TRUE
  while (flag) {
    sum <- sum + sample(1:6,1)+sample(1:6,1)
    count <- count+1
    if(sum>=k) 
      flag=FALSE
  }
  return(count)
}
count <- function(ls,target){
  count <- 0
  for (variable in ls) {
    if (variable == target) {
      count <- count+1
    }
  }
  return(count/length(ls))
}