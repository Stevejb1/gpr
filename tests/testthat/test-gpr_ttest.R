test_that("gpr_ttest a_priori two sample works", {
  result <- gpr_ttest(test = "two.sample", analysis = "a_priori",
                      effect_size = 0.5, alpha = 0.05, power = 0.80,
                      plot = FALSE)
  expect_equal(result$total_n, 128)
  expect_equal(result$n1, 64)
  expect_equal(round(result$power, 4), 0.8015)
})

test_that("gpr_ftest a_priori one way anova works", {
  result <- gpr_ftest(test = "anova.one.way", analysis = "a_priori",
                      effect_size = 0.25, alpha = 0.05, power = 0.80,
                      groups = 3, plot = FALSE)
  expect_equal(result$n, 159)
  expect_equal(round(result$power, 7), 0.8048873)
})

test_that("gpr_chisq a_priori gof works", {
  result <- gpr_chisq(test = "gof", analysis = "a_priori",
                      effect_size = 0.30, alpha = 0.05, power = 0.80,
                      df = 5, plot = FALSE)
  expect_equal(result$n, 143)
  expect_equal(round(result$power, 7), 0.8015133)
})

test_that("gpr_exact proportion one sample works", {
  result <- gpr_exact(test = "proportion.one.sample", analysis = "a_priori",
                      p0 = 0.50, p1 = 0.65, alpha = 0.05, power = 0.80,
                      plot = FALSE)
  expect_equal(result$total_n, 90)
  expect_equal(round(result$power, 7), 0.8122881)
})
