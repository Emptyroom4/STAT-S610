#Q2

rejection_sampling <- function(n, targ_pdf, prop_pdf, prop_ran, env_const ){
  if(n %% 1 !=0) stop("n is not integer") 
  if(n < 0) stop("n is required to be an integer")
  if(missing(env_const)){
    env_const <- def_env_const(targ_pdf, prop_pdf)
  }
  i = 1
  accept = rep(NA, n)
  while(TRUE){
    prop_value = prop_ran(1)
    u = runif(1)
    if((u*(env_const*prop_pdf(prop_value))) < (targ_pdf(prop_value))){
      accept[i] = prop_value
      i = i + 1
    }
    if (i > n) break
  }
  return(accept)
}

rejection_sampling(100, dnorm, function(x) 1/2*exp(-abs(x)), function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1), 1.32)

                   