source("/Users/hyeseonseo/Desktop/Intro_Computing/Final/final_q3.R")

## Test that 
eigen_fun(X2,tol=.Machine$double.eps^0.75)
set.seed(1234567890)
z2 <- matrix(rnorm(2*2),2,2)
z3 <- matrix(rnorm(3*3),3,3)
z4 <- matrix(rnorm(4*4),4,4)
z5 <- matrix(rnorm(5*5),5,5)

X2 <- z2 + t(z2)
X3 <- z3 + t(z3)
X4 <- z4 + t(z4)
X5 <- z5 + t(z5)

t2 <- eigen_fun(X2,tol=.Machine$double.eps^0.75)
t3 <- eigen_fun(X3,tol=.Machine$double.eps^0.75)
t4 <- eigen_fun(X4,tol=.Machine$double.eps^0.75)
t5 <- eigen_fun(X5,tol=.Machine$double.eps^0.75)

e_t2 <- list(sort(t2$values), sort(t2$vectors))
e_t3 <- list(sort(t2$values), sort(t2$vectors))
e_t4 <- list(sort(t2$values), sort(t2$vectors))
e_t5 <- list(sort(t2$values), sort(t2$vectors)) 

e_X2 <- list(sort(eigen(X2)$values), sort(eigen(X2)$vectors))
e_X3 <- list(sort(eigen(X3)$values), sort(eigen(X3)$vectors))
e_X4 <- list(sort(eigen(X4)$values), sort(eigen(X4)$vectors))
e_X5 <- list(sort(eigen(X5)$values), sort(eigen(X5)$vectors))


testthat("eigen decomposition output", {
  expect_equal(e_t2, e_X2)
  expect_equal(e_t3, e_X3)
  expect_equal(e_t4, e_X4)
  expect_equal(e_t5, e_X5)
})


