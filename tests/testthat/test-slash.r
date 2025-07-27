test_that("`FSLASH()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- FSLASH(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- FSLASH(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    expect_error(FSLASH(2, 8, 8, 2, 1,
                 left = "horizontal", right = "square"),
                 "not supported")
    d <- FSLASH(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
    expect_true(inherits(d, "dee"))
    expect_error(FSLASH(2, 8, 8, 2, 1,
                 left = "square", right = "horizontal"),
                 "not supported")
    d <- FSLASH(2, 8, 8, 2, 1, nib = "square")
    expect_true(inherits(d, "dee"))
    expect_error(FSLASH(2, 8, 8, 2, 1,
                 left = "square", right = "vertical"),
                 "not supported")
    d <- FSLASH(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
    expect_true(inherits(d, "dee"))
    expect_error(FSLASH(2, 8, 8, 2, 1,
                 left = "vertical", right = "square"),
                 "not supported")
    d <- FSLASH(2, 8, 8, 2, 1, nib = "vertical")
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`BSLASH()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- BSLASH(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- BSLASH(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    expect_error(BSLASH(2, 8, 8, 2, 1,
                 left = "horizontal", right = "square"),
                 "not supported")
    d <- BSLASH(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
    expect_true(inherits(d, "dee"))
    expect_error(BSLASH(2, 8, 8, 2, 1,
                 left = "square", right = "horizontal"),
                 "not supported")
    d <- BSLASH(2, 8, 8, 2, 1, nib = "square")
    expect_true(inherits(d, "dee"))
    expect_error(BSLASH(2, 8, 8, 2, 1,
                 left = "square", right = "vertical"),
                 "not supported")
    d <- BSLASH(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
    expect_true(inherits(d, "dee"))
    expect_error(BSLASH(2, 8, 8, 2, 1,
                 left = "vertical", right = "square"),
                 "not supported")
    d <- BSLASH(2, 8, 8, 2, 1, nib = "vertical")
    expect_true(inherits(d, "dee"))
    # plot(d, height = 10, width = 10, fill = "red")
})
