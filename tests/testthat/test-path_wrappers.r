test_that("`d_ellipse()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	expect_true(d_circle(5, 5, 2) |> inherits("dee"))
	# plot(d_circle(5, 5, 4), height = 10, width = 10, fill = "red")
	expect_true(d_ellipse(5, 5, 2, 3) |> inherits("dee"))
	# plot(d_ellipse(5, 5, 2, 3), height = 10, width = 10, fill = "red")
	# plot(d_ellipse(5, 5, 3, 2), height = 10, width = 10, fill = "red")
	expect_true(d_ellipse(5, 5, 2, 3, a = 45) |> inherits("dee"))
	# plot(d_ellipse(5, 5, 3, 2, a = 45), height = 10, width = 10, fill = "red")

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	expect_true(d_ellipse(5, 5, 2, 3, a = 45) |> inherits("dee"))
})

test_that("`d_polygon()`", {
	skip_if_not_installed("polyclip")
	skip_on_cran()
	do.call(rlang::local_options, dee_options(dee.digits = 0, default = TRUE))
	l <- list(x = c(4, 4, 8, 8), y = c(4, 8, 8, 4))
	expect_equal(d_polygon(l) |> format(), "M 4,4 4,8 8,8 8,4 Z")
	expect_equal(d_polygon(l, offset = 1) |> format(), "M 9,9 3,9 3,3 9,3 Z")
	expect_equal(d_polygon(l, offset = -1) |> format(), "M 7,7 5,7 5,5 7,5 Z")
	expect_error(d_polygon(l, offset = -10), "did not return a polygon")
})

test_that("`d_rect()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	expect_equal(d_rect(x = 10, y = 10, w = 6, h = 4) |> format(), "M 7,8 7,12 13,12 13,8 Z")

	expect_true(d_rect(10, 10, 6, 4, a = 45) |> inherits("dee"))
	# plot(d_rect(10, 10, 6, 4, a = 45), height = 20, width = 20, fill = "red")

	skip_if_not_installed("polyclip")
	skip_on_cran()
	expect_equal(
		d_rect(x = 10, y = 10, w = 6, h = 4, offset = 1, digits = 0) |> format(),
		"M 14,13 6,13 6,7 14,7 Z"
	)

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	expect_equal(d_rect(5, 5, 4, 2, a = 90) |> format(), "M 4,7 6,7 6,3 4,3 Z")
})
