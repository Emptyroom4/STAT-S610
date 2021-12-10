source("Desktop/Intro_Computing/HW8/header/hw8_q1_header.R")
#Q1 header test
#Hyeseon Seo

library(testthat)

test_that('define cdf inverse test', {
  p = c(seq(0.1, 0.9, 0.1))
  expected = c(qnorm(p, mean=100, sd=25))
  for ( i in 1:length(p)){
    f <- def_cdf_inv(pnorm, mean=100, sd=25)
    exp_return_value = f(p[i], mean=100, sd=25)
    expect_equal(exp_return_value, expected[i])
  }
}) 






