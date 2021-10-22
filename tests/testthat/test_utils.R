library(testthat)
library(dateutils)

context("test aggregations")

test_that("agg_to_freq works as expected", {
  library(data.table)
  sq1 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2021-06-30"), by = "day")
  dt1 <- data.table(ref_date = sq1, series_name = "a", value = seq(length(sq1)))
  sq2 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2021-06-30"), by = "week")
  dt2 <- data.table(ref_date = sq2, series_name = "b", value = seq(length(sq2)))
  dt <- rbind(dt1,dt2)
  out <- agg_to_freq(dt)
  expect(NROW(out) == 36, "Monthly aggregation did not run as expected")
  expect(all(out$ref_date%in%end_of_period(out$ref_date)), "Monthly dates are not end of month")
})

test_that("stack_obs functions correctly", {
  X <- matrix(rnorm(100), 50,2)
  Z <- stack_obs(X, 3)
  expect(all(X[50,] == Z[48, 1:2]), "Stack obs does not return DFM format data")
  expect(all(X[48,] == Z[48, 5:6]), "Stack obs does not return DFM format data")
})

test_that("VAR functions work correctly", {
  B <- matrix(c(.4, .1, .1, -.2, .2, .3, .1, 0), 2, 4, byrow = TRUE)
  A <- comp_form(B)
  Q <- matrix(0, 4, 4)
  Q[1:2, 1:2] <- diag(2)
  P <- long_run_var(A, Q, 2, 2)
  expect(all(P[1:2, 1:2] == P[3:4, 3:4]), "Long run variance incorrect")
  expect(round(P[1,1], digits = 2) == 1.3, "Stack obs does not return DFM format data")
})

test_that("weekly aggregation is indexed to Friday", {
  library(data.table)
  fred_small <- fred[ref_date <= as.Date("2021-01-01")]
  out <- agg_to_freq(fred, frq = "week")
  expect(all(weekdays(out$ref_date)=="Friday"), "Weekly aggregation does not index to Friday")
})

test_that("seasonal adjustment is working", {
  fred_sa <- seas_df_long(fred, sa_names = c("gdp constant prices", "advance retail sales"),
                          x11 = TRUE, transfunc = 'auto')
  expect(all(c("gdp constant prices sa", "advance retail sales sa")%in%unique(fred_sa$values_sa$series_name)), "SA series are missing")
})