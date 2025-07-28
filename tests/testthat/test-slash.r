test_that("`d_fslash()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_fslash(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_fslash(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    expect_error(d_fslash(2, 8, 8, 2, 1,
                 left = "horizontal", right = "square"),
                 "not supported")
    d <- d_fslash(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
    expect_true(inherits(d, "dee"))
    expect_error(d_fslash(2, 8, 8, 2, 1,
                 left = "square", right = "horizontal"),
                 "not supported")
    d <- d_fslash(2, 8, 8, 2, 1, nib = "square")
    expect_true(inherits(d, "dee"))
    expect_error(d_fslash(2, 8, 8, 2, 1,
                 left = "square", right = "vertical"),
                 "not supported")
    d <- d_fslash(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
    expect_true(inherits(d, "dee"))
    expect_error(d_fslash(2, 8, 8, 2, 1,
                 left = "vertical", right = "square"),
                 "not supported")
    d <- d_fslash(2, 8, 8, 2, 1, nib = "vertical")
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_bslash()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_bslash(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_bslash(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    expect_error(d_bslash(2, 8, 8, 2, 1,
                 left = "horizontal", right = "square"),
                 "not supported")
    d <- d_bslash(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
    expect_true(inherits(d, "dee"))
    expect_error(d_bslash(2, 8, 8, 2, 1,
                 left = "square", right = "horizontal"),
                 "not supported")
    d <- d_bslash(2, 8, 8, 2, 1, nib = "square")
    expect_true(inherits(d, "dee"))
    expect_error(d_bslash(2, 8, 8, 2, 1,
                 left = "square", right = "vertical"),
                 "not supported")
    d <- d_bslash(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
    expect_true(inherits(d, "dee"))
    expect_error(d_bslash(2, 8, 8, 2, 1,
                 left = "vertical", right = "square"),
                 "not supported")
    d <- d_bslash(2, 8, 8, 2, 1, nib = "vertical")
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})
