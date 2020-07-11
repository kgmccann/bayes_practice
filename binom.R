
f <- function(x) factorial(x)
liklihood <- function(w,l,p){
  (f(w+l)/(f(w)*f(l)))*p**w*(1-p)**l
}
update_prior <- function(new,prior){
  #'for all new observation @param new
  #'update @param prior
  if(!is.null(nrow(new))) new <- colSums(new)
  post <- liklihood(new[1],new[2],seq(0,1,length.out=length(prior)))*(prior)
  
  st_post <- post/sum(post)
  names(st_post) <- round(seq(0,1,length.out=length(prior)),2)
  st_post
}

W <- c(1,0,1,1,1,0,1,0,1)
L <- (W-1)*-1

N <- cbind(W,L)
P <- rep(0.5,20)

liklihood(1,0,seq(0,1,length.out=length(P)))==dbinom(1,size=1,prob=seq(0,1,length.out=length(P))) #close enough

par(mfrow=c(2,1))
first_ob <- update_prior(c(1,0),P)
plot(names(first_ob),first_ob,type='l',col='blue',xlab = '')
sum(update_prior(c(1,0),P))

plot(names(first_ob),update_prior(N,P),col='red',xlab='Proportion')
sum(update_prior(N,P))

