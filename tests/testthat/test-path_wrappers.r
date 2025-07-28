test_that("`ELLIPSE()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_true(CIRCLE(5, 5, 2) |> inherits("dee"))
    # plot(CIRCLE(5, 5, 4), height = 10, width = 10, fill = "red")
    expect_true(ELLIPSE(5, 5, 2, 3) |> inherits("dee"))
    # plot(ELLIPSE(5, 5, 2, 3), height = 10, width = 10, fill = "red")
    # plot(ELLIPSE(5, 5, 3, 2), height = 10, width = 10, fill = "red")
})

test_that("`POLYGON()`", {
    skip_if_not_installed("polyclip")
    skip_on_cran()
    do.call(rlang::local_options,
            dee_options(dee.digits = 0, default = TRUE))
    l <- list(x = c(4, 4, 8, 8), y = c(4, 8, 8, 4))
    expect_equal(POLYGON(l) |> format(),
                 "M 4,4 4,8 8,8 8,4 Z")
    expect_equal(POLYGON(l, offset = 1) |> format(),
                 "M 9,9 3,9 3,3 9,3 Z")
    expect_equal(POLYGON(l, offset = -1) |> format(),
                 "M 7,7 5,7 5,5 7,5 Z")
    expect_error(POLYGON(l, offset = -10), "did not return a polygon")
})

test_that("`RECT()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(RECT(x = 10, y = 10, w = 6, h = 4) |> format(),
                 "M 7,8 7,12 13,12 13,8 Z")
})
