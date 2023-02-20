# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

qF <- function(w,v,q){
  v <- v[v>0]
  V <- w*v
  if(q==1) exp(-sum(V*(v/sum(V*v))*log(v/sum(V*v))))
  else  sum(V*(v/sum(V*v))^q)^(1/(1-q))
}

qF_tau <- function(w,v,q,tau,di){
  d_tau <- ifelse(di<tau,di,tau)
  ai <- (1-d_tau/tau)%*%v
  ai_tau <- ai[ai>0]
  v <- v[ai>0]
  V <- w*v*v/ai_tau
  if(q==1) exp(-sum(V*(ai_tau/sum(V*ai_tau))*log(ai_tau/sum(V*ai_tau))))
  else sum(V*(ai_tau/sum(V*ai_tau))^q)^(1/(1-q))
}

F_alpha <- function(w,v,v1,q,R=c(0.5,0.5),N=2){
  V <- w*(v%*%R)
  v_plus <- c()
  for (i in 1:N){
    v_plus <- cbind(v_plus,v[,i]*R[i])
  }
  if (q==1) {
    Vv <- ifelse(v_plus==0,0,(v_plus/sum(V*(v%*%R)))*log(v_plus/sum(V*(v%*%R))))
    1/N*exp(-sum(t(V)%*%Vv))
  }
  else {
    Vv <- ifelse(v1==0,0,(v_plus/sum(V*(v%*%R)))^q)
    1/N*(sum(V*rowSums(Vv)))^(1/(1-q))
  }
}

F_gamma <- function(w,v,q,R=c(0.5,0.5),N=2){
  V <- w*(v%*%R)
  if(q==1) {
    Vv <- ifelse(v%*%R==0,0,((v%*%R)/sum(V*(v%*%R)))*log((v%*%R)/sum(V*(v%*%R))))
    exp(-sum(V*Vv))
  }
  else (sum(V*((v%*%R)/sum(V*(v%*%R)))^q))^(1/(1-q))
}

F_alphaa_tau <- function(w,v,v1,q,di,tau,R=c(0.5,0.5),N=2){
  d_tau <- ifelse(di<tau,di,tau)
  ai <- (1-d_tau/tau)%*%v
  f_bar <- rowSums(v)/N
  ai_bar <- (1-d_tau/tau)%*%f_bar
  v_tau <- ifelse(f_bar==0,0,f_bar*f_bar/ai_bar)
  V <- w*v_tau

  if (q==1) {
    Vv <- ifelse(ai==0,0,(ai/sum(V%*%ai))*log(ai/sum(V%*%ai)))
    1/N*exp(-sum(t(V)%*%Vv))
  }
  else {
    Vv <- ifelse(v1==0,0,(ai/sum(V%*%ai))^q)
    1/N*(sum(V*rowSums(Vv)))^(1/(1-q))
  }
}


F_gamma_tau <- function(w,v,q,di,tau,R=c(0.5,0.5),N=2){
  d_tau <- ifelse(di<tau,di,tau)
  ai <- (1-d_tau/tau)%*%v
  f_bar <- rowSums(v)/N
  ai_bar <- (1-d_tau/tau)%*%f_bar
  v_tau <- ifelse(f_bar==0,0,f_bar*f_bar/ai_bar)
  V <- w*v_tau

  if(q==1) {
    Vv <- ifelse(ai_bar==0,0,((ai_bar)/sum(V*ai_bar))*log((ai_bar)/sum(V*ai_bar)))
    exp(-sum(V*Vv))
  }
  else (sum(V*((ai_bar)/sum(V*ai_bar))^q))^(1/(1-q))
}

