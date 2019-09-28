test_that("multiplication works", {
    x <- c(1:8, NA, NA)
    perc = 0.9
    expect_equal(sum_perc(x, perc), NA_real_)
    expect_equal(mean_perc(x, perc), NA_real_)

    expect_equal(sum_perc(x) , 36)
    expect_equal(mean_perc(x), 4.5)
})
