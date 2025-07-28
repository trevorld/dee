test_that("`ISOTOXAL_2NGON()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- ISOTOXAL_2NGON(5, 5, 3, 2/3, 5)
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")

    rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
    d <- ISOTOXAL_2NGON(5, 5, 3, 2/3, 5)
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`REGULAR_NGON()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- REGULAR_NGON(x = 5, y = 5, r = 4, n = 5)
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")

    rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
    d <- REGULAR_NGON(x = 5, y = 5, r = 4, n = 5)
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})
