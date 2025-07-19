test_that("`dee_options()`", {
    dot <- dee_options(default = TRUE)
    do.call(rlang::local_options, dot)

    dof <- dee_options(dee.sep = ",", default = FALSE)
    expect_equal(dot, dof)
})
