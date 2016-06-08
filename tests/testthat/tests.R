test_that("compute visitors from actions", {
    actions <- data.frame()
    expect_that(1 + 1, equals(2))
    expect_that(compute_visitors(actions), throws_error())
})
