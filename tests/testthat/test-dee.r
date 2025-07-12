test_that("`dee()`", {
    d <- dee("M 10,30") + dee("V 30")
    expect_true(inherits(d, "dee"))
    expect_true(inherits(format(d), "character"))
})
