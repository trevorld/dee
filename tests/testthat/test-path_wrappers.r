test_that("`d_ellipse()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_true(d_circle(5, 5, 2) |> inherits("dee"))
    # plot(d_circle(5, 5, 4), height = 10, width = 10, fill = "red")
    expect_true(d_ellipse(5, 5, 2, 3) |> inherits("dee"))
    # plot(d_ellipse(5, 5, 2, 3), height = 10, width = 10, fill = "red")
    # plot(d_ellipse(5, 5, 3, 2), height = 10, width = 10, fill = "red")
})

test_that("`d_polygon()`", {
    skip_if_not_installed("polyclip")
    skip_on_cran()
    do.call(rlang::local_options,
            dee_options(dee.digits = 0, default = TRUE))
    l <- list(x = c(4, 4, 8, 8), y = c(4, 8, 8, 4))
    expect_equal(d_polygon(l) |> format(),
                 "M 4,4 4,8 8,8 8,4 Z")
    expect_equal(d_polygon(l, offset = 1) |> format(),
                 "M 9,9 3,9 3,3 9,3 Z")
    expect_equal(d_polygon(l, offset = -1) |> format(),
                 "M 7,7 5,7 5,5 7,5 Z")
    expect_error(d_polygon(l, offset = -10), "did not return a polygon")
})

test_that("`d_rect()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(d_rect(x = 10, y = 10, w = 6, h = 4) |> format(),
                 "M 7,8 7,12 13,12 13,8 Z")
})
