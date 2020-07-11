
f <- function(x) factorial(x)
liklihood <- function(w,l,p){
  f(w+l)/(f(w)*f(l))*p*(1-p)
}
update_prior <- function(new,prior){
  #'every new observation @param new
  #'update @param prior
  #if(!is.null(nrow(new))) new <- colSums(new)
  liklihood(new[1],new[2],prior[1])*prior[1]/1
  
}


update_prior(c(1,0),c(0.5,0.5))

W <- c(1,0,1,1,1,0,1,0,1)
L <- (W-1)*-1

N <- cbind(W,L)
P <- 0.5

p_W <- c(0)
for(i in 1:nrow(N)){
  P <- update_prior(N[i,],c(P,(1-P)))
  p_W <- c(p_W,P)
}
plot(p_W)

dbinom(2,size=2,prob=0.5)
