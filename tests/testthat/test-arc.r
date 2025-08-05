test_that("`d_arc1()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc1(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc1(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc2()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc2(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc2(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc3()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc3(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc3(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc4()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc4(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc4(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc12()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc12(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc12(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc23()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc23(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc23(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc34()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc34(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc34(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc41()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc41(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc41(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc123()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc123(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc123(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc234()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc234(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc234(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc341()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc341(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc341(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_arc412()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    d <- d_arc412(2, 8, 8, 2, 1)
    expect_true(inherits(d, "dee"))
    dob <- d_arc412(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
    expect_equal(d, dob)
    # plot(d, height = 10, width = 10, fill = "red")
})
