test_that("`d_isotoxal_2ngon()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	d <- d_isotoxal_2ngon(5, 5, 3, 5, s = 2 / 3)
	expect_s3_class(d, "dee")
	# plot(d, height = 10, width = 10, fill = "red")

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	d <- d_isotoxal_2ngon(5, 5, 3, 5, s = 2 / 3)
	expect_s3_class(d, "dee")
	# plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_isotoxal_2ngon()` `s` shortcuts", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	s <- affiner::isotoxal_2ngon_inner_radius(5, d = 2)
	expect_equal(
		d_isotoxal_2ngon(5, 5, 3, n = 5, d = 2),
		d_isotoxal_2ngon(5, 5, 3, s = s, n = 5)
	)
	s <- affiner::isotoxal_2ngon_inner_radius(8, alpha = 45)
	expect_equal(
		d_isotoxal_2ngon(5, 5, 3, n = 8, alpha = 45),
		d_isotoxal_2ngon(5, 5, 3, s = s, n = 8)
	)
	s <- affiner::isotoxal_2ngon_inner_radius(8, beta_ext = 90)
	expect_equal(
		d_isotoxal_2ngon(5, 5, 3, n = 8, beta_ext = 90),
		d_isotoxal_2ngon(5, 5, 3, s = s, n = 8)
	)
	expect_snapshot(error = TRUE, d_isotoxal_2ngon(5, 5, 3, n = 8))
	expect_snapshot(error = TRUE, d_isotoxal_2ngon(5, 5, 3, n = 8, d = 2, alpha = 45))
})

test_that("`d_regular_ngon()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	d <- d_regular_ngon(x = 5, y = 5, r = 4, n = 5)
	expect_s3_class(d, "dee")
	# plot(d, height = 10, width = 10, fill = "red")

	rlang::local_options(dee.origin_at_bottom = TRUE, dee.height = 10)
	d <- d_regular_ngon(x = 5, y = 5, r = 4, n = 5)
	expect_s3_class(d, "dee")
	# plot(d, height = 10, width = 10, fill = "red")
})
