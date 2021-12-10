#Q2 header_test
#Hyeseon Seo

test_that('when envelope_constant is missing', {
  n3 = rejection_sampling(100, dnorm, function(x) 1/2*exp(-abs(x)), function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1))
  n4 = rejection_sampling(100, dnorm, function(x) 1/2*exp(-abs(x)), function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1))
  ks_test3 = ks.test(n3, pnorm)
  ks_test4 = ks.test(n4, pnorm)
  expect_gt(ks_test3$p.value, 0.01)
  expect_gt(ks_test4$p.value, 0.01)
})
