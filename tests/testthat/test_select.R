library(testthat)
result<-select(data=mtcars,method = "onepropselection",p=2,
       mutation_rate = 0.01,regression_target = "mpg",
       scheme = "re-rank",max_iter = 500)

test_that("test select.R works",{
          expect_true(is.list(result))

}
)



