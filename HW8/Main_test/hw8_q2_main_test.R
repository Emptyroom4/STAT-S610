
#Q2_main test
#Hyeseon Seo
source("Desktop/Intro_Computing/HW8/main/hw8_q2_main.R")
set.seed(1234567890)

test_that('ks_test', {
  set.seed(1234567890)
  n1 = rejection_sampling(100, dnorm, function(x) dt(x,1), function(n) rt(n,1), 1.52)
  n2 = rejection_sampling(100, dnorm, function(x) 1/2*exp(-abs(x)), function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1), 1.32)
  
  ks_test1 = ks.test(n1, pnorm)
  ks_test2 = ks.test(n2, pnorm)
  expect_gt(ks_test1$p.value, 0.01)
  expect_gt(ks_test2$p.value, 0.01)
})
