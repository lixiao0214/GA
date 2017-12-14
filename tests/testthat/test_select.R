library(testthat)

test_that("test crossover_p_splt works",
          expect_true(
            is.list(select(data=mtcars,method = "onepropselection",p=2,
                           mutation_rate = 0.01,regression_target = "mpg",
                           scheme = "re-rank",max_iter = 500))
          )
)



