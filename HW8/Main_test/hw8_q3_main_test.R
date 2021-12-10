
#Q3. main test
# Hyeseon Seo

source("Desktop/Intro_Computing/HW8/main/hw8_q3_main.R")
source("Desktop/Intro_Computing/HW8/header/hw8_q3_header.R")

library(testthat)

test_that('ks_test', {
  h = function(x,mu,sigma) -(x-mu)^2/(2*sigma^2)-0.5*log(2*pi*sigma^2)
  interval = c(-Inf,Inf)
  mu = 3
  sigma = 2
  x = mu+c(-1,1)*sigma
  n = 1000
  out = adaptive_rejection_sampling(n, h, interval, x, mu=mu, sigma=sigma) 
  expect_gt(ks.test(out, pnorm, mean=mu, sd=sigma)$p.value, 0.01)
})
