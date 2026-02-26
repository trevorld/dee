test_that("`d_fslash()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	# Default `nib = "horizontal"`
	d <- d_fslash(2, 8, 8, 2, 1)
	expect_true(inherits(d, "dee"))
	dob <- d_fslash(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
	expect_equal(d, dob)
	d <- d_fslash(2, 8, 8, 2, 1, left = "horizontal", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "horizontal", right = "square")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, nib = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "diagonal", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "diagonal", right = "square")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "diagonal", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, nib = "square")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "square", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "square", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "square", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, nib = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "vertical", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_fslash(2, 8, 8, 2, 1, left = "vertical", right = "square")
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")
})

test_that("`d_bslash()`", {
	do.call(rlang::local_options, dee_options(default = TRUE))
	# Default `nib = "horizontal"`
	d <- d_bslash(2, 8, 8, 2, 1)
	expect_true(inherits(d, "dee"))
	dob <- d_bslash(8, 8, 2, 2, 1, origin_at_bottom = TRUE, height = 10)
	expect_equal(d, dob)
	d <- d_bslash(2, 8, 8, 2, 1, left = "horizontal", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "horizontal", right = "square")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "horizontal", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, nib = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "diagonal", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "diagonal", right = "square")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "diagonal", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, nib = "square")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "square", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "square", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "square", right = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, nib = "vertical")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "vertical", right = "diagonal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "vertical", right = "horizontal")
	expect_true(inherits(d, "dee"))
	d <- d_bslash(2, 8, 8, 2, 1, left = "vertical", right = "square")
	expect_true(inherits(d, "dee"))
	# plot(d, height = 10, width = 10, fill = "red")
})

test_that("`height_slash_left()` and `height_slash_right()`", {
	# horizontal nib always gives height 0
	nibs <- c("horizontal", "diagonal", "square", "vertical")
	for (right in nibs) {
		expect_equal(height_slash_left(6, 6, 1, left = "horizontal", right = right), 0)
	}
	for (left in nibs) {
		expect_equal(height_slash_right(6, 6, 1, left = left, right = "horizontal"), 0)
	}
	# vertical/vertical: height is same on both ends
	expect_equal(
		height_slash_left(6, 6, 1, nib = "vertical"),
		height_slash_right(6, 6, 1, nib = "vertical")
	)
	# diagonal/diagonal: height = v_fslash_vh(dx, dy, 0.5*w) = sqrt(2)/2
	expect_equal(height_slash_left(6, 6, 1, nib = "diagonal"), sqrt(2) / 2)
	expect_equal(height_slash_right(6, 6, 1, nib = "diagonal"), sqrt(2) / 2)
	# symmetry: height_slash_left(dx, dy, w, A, B) == height_slash_right(dx, dy, w, B, A)
	for (left in nibs) {
		for (right in nibs) {
			expect_equal(
				height_slash_left(6, 6, 1, left = left, right = right),
				height_slash_right(6, 6, 1, left = right, right = left)
			)
		}
	}
})

test_that("`width_slash_left()` and `width_slash_right()`", {
	# vertical nib always gives width 0
	nibs <- c("horizontal", "diagonal", "square", "vertical")
	for (right in nibs) {
		expect_equal(width_slash_left(6, 6, 1, left = "vertical", right = right), 0)
	}
	for (left in nibs) {
		expect_equal(width_slash_right(6, 6, 1, left = left, right = "vertical"), 0)
	}
	# horizontal/vertical: width_left = h_fslash_vh(dx, dy, w) = sqrt(2) for dx=dy=6, w=1
	expect_equal(width_slash_left(6, 6, 1, left = "horizontal", right = "vertical"), sqrt(2))
	# diagonal/diagonal: width = h_fslash_vh(dx, dy, 0.5*w) = sqrt(2)/2
	expect_equal(width_slash_left(6, 6, 1, nib = "diagonal"), sqrt(2) / 2)
	expect_equal(width_slash_right(6, 6, 1, nib = "diagonal"), sqrt(2) / 2)
	# symmetry: width_slash_left(dx, dy, w, A, B) == width_slash_right(dx, dy, w, B, A)
	for (left in nibs) {
		for (right in nibs) {
			expect_equal(
				width_slash_left(6, 6, 1, left = left, right = right),
				width_slash_right(6, 6, 1, left = right, right = left)
			)
		}
	}
})
