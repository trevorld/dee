test_that("`d_isotoxal_2ngon()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	d <- d_isotoxal_2ngon(5, 5, 3, 2 / 3, 5)
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	d <- d_isotoxal_2ngon(5, 5, 3, 2 / 3, 5)
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_regular_ngon()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	d <- d_regular_ngon(x = 5, y = 5, r = 4, n = 5)
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	d <- d_regular_ngon(x = 5, y = 5, r = 4, n = 5)
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")
})
