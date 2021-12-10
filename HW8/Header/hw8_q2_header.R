
# Q2 header
#Hyeseon Seo

def_env_const <- function(targ_pdf, prop_pdf){
  h <- function(x){
    if(targ_pdf(x) < .Machine$double.eps)
  retrun(0)
    else (return(targ_pdf(x) / prop_pdf(x)))
  }
  opt = optimise(h, interval = c(-1,1)*10, 
                 maximum = TRUE, tol =.Machine$double.eps^0.5)
  return(opt$objective)
}



