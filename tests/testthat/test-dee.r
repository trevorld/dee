test_that("`dee()`", {
    d <- dee("M 10,30") + dee("V 30")
    expect_true(inherits(d, "dee"))
    expect_true(inherits(format(d), "character"))
    s <- capture.output({d2 <- print(d)})
    expect_equal(d, d2)
    expect_equal(s, "M 10,30 V 30")
})
