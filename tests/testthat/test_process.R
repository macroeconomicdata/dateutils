library(testthat)
library(dateutils)

context("test process functions")

test_that("process_MF() works as expected", {
  MF <- process_MF(fred[series_name == "gdp constant prices"], fred[series_name != "gdp constant prices"],
                   LHS_lags = 1, RHS_lags = 0) 
  expect(all(c("gdp constant prices 0",
         "gdp constant prices 1",
         "advance retail sales 0",
         "initial jobless claims 0",
         "t bill spread 10y 3m 0")%in%unique(MF$series_name)), "Mixed frequency series are missing")
})

test_that("process() functions correctly", {
  MF <- process_MF(fred[series_name == "gdp constant prices"], fred[series_name != "gdp constant prices"],
                   LHS_lags = 0, RHS_lags = 0) 
  out <- process(MF, fredlib)
  expect(max(abs(out$value), na.rm = TRUE)<7, "process() values too large")
})