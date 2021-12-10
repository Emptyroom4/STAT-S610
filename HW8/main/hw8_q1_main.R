#Q1 main
#Hyeseon Seo


set.seed(1234567890)

inverse_sampling <- function(n, cdf, cdf_inv, ...){
  if (n <0) stop("n is required to be positive")
  if (n%%1 !=0) stop(" n should be integer") 
  if (missing(cdf)) stop("cdf is required") 
  if (missing(cdf_inv)) {cdf_inv = def_cdf_inv(cdf, ...)} 
  
  v_cdf_inv = Vectorize(cdf_inv, vectorize.args = names(formals(cdf_inv))[1])
  return(v_cdf_inv(runif(n),...))
 }


