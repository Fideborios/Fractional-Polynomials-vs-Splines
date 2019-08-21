# Inverse logit function
#rs is the risk score (or linear predictor or predicted log-odds)
expit<-function(rs) {1/(1+exp(-rs))}

# Integrate over the distribution of u for a given patient and 
# output population average predictions
papred<-function(x,beta,sigmau){
  #x is an Nxp design matrix 
  #beta is px1 vector of estimated regression coefficients  
  #sigmau the standard deviation of the random effects 
  #Sample from the prior distribution of the random effects
  u <- qnorm(seq(0.001, 0.999, 0.001))*sigmau
  predu<-function(x,beta,u) { mean(expit(x%*%beta+u)) }
  pa<-apply(x,1,predu,"beta"=beta,"u"=u)  
  return(pa)
}

