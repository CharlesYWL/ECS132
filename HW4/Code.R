#Ecs132 Hw4
#Weili Yin,912603171
#Haoran Ding, 913203198
###P2
sim2 <- function(nreps){
  list <- c()
  for (i in 1:nreps) {
    list[i] <- gerenateOne(20,4.2)
  }
  cat("P(Bug>40)=", checkMore(list,40))
}
gerenateOne <- function(n,lamb){
  return(sum(rpois(n, lamb)))
}
  
checkMore <-function(list,target){
  count <- 0
  for (v in list) {
    if (v >40)
      count <- count+1
  }
  return(count/length(list))
}
