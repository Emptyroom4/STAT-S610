source("Desktop/Intro_Computing/HW8/main/hw8_q1_main.R")
source("Desktop/Intro_Computing/HW8/header/hw8_q1_header.R")

library(testthat)

#Q1.
test_that("ks_test", {
  x = inverse_sampling(100,pnorm,qnorm,mean = 100, sd = 25)
  ks_test_out = ks.test(x, pnorm, mean = 100, sd = 25)
  expect_gt(ks_test_out$p.value, 0.01)
})


