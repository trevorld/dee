test_that("`x_ellipse_left()` and `x_ellipse_right()`", {
	# Unrotated: at y = yc, left = xc - rx, right = xc + rx
	expect_equal(x_ellipse_left(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), 5 - 3)
	expect_equal(x_ellipse_right(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), 5 + 3)
	# Rotated 90°: rx/ry swap roles, so at yc: left = xc - ry, right = xc + ry
	expect_equal(x_ellipse_left(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90), 5 - 4)
	expect_equal(x_ellipse_right(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90), 5 + 4)
	# y outside the ellipse returns NaN
	suppressWarnings({
		expect_equal(x_ellipse_left(10, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), NaN)
		expect_equal(x_ellipse_right(10, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), NaN)
	})
	# left <= right
	expect_lte(
		x_ellipse_left(6, xc = 5, yc = 5, rx = 3, ry = 4, a = 30),
		x_ellipse_right(6, xc = 5, yc = 5, rx = 3, ry = 4, a = 30)
	)
})

test_that("`y_ellipse_top()` and `y_ellipse_bottom()`", {
	# Unrotated: at x = xc, top = yc - ry, bottom = yc + ry
	expect_equal(y_ellipse_top(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), 5 - 4)
	expect_equal(y_ellipse_bottom(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), 5 + 4)
	# Rotated 90°: rx/ry swap roles, so at xc: top = yc - rx, bottom = yc + rx
	expect_equal(y_ellipse_top(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90), 5 - 3)
	expect_equal(y_ellipse_bottom(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90), 5 + 3)
	# x outside the ellipse returns NaN
	suppressWarnings({
		expect_equal(y_ellipse_top(10, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), NaN)
		expect_equal(y_ellipse_bottom(10, xc = 5, yc = 5, rx = 3, ry = 4, a = 0), NaN)
	})
	# top <= bottom
	expect_lte(
		y_ellipse_top(6, xc = 5, yc = 5, rx = 3, ry = 4, a = 30),
		y_ellipse_bottom(6, xc = 5, yc = 5, rx = 3, ry = 4, a = 30)
	)
	# origin_at_bottom: top > bottom (top is larger y value)
	expect_equal(
		y_ellipse_top(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0, origin_at_bottom = TRUE),
		5 + 4
	)
	expect_equal(
		y_ellipse_bottom(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0, origin_at_bottom = TRUE),
		5 - 4
	)
})

test_that("More tests for rotated ellipses", {
	xy <- as_coord2d(degrees(135), radius = 2)
	xo <- 5
	yo <- 4

	# angle = 45
	expect_equal(y_ellipse_top(xy$x + xo, xc = xo, yc = yo, rx = 1, ry = 2, a = 45), yo - xy$y)
	expect_equal(y_ellipse_bottom(-xy$x + xo, xc = xo, yc = yo, rx = 1, ry = 2, a = 45), yo + xy$y)
	expect_equal(x_ellipse_left(-xy$y + yo, xc = xo, yc = yo, rx = 1, ry = 2, a = 45), xo + xy$x)
	expect_equal(x_ellipse_right(xy$y + yo, xc = xo, yc = yo, rx = 1, ry = 2, a = 45), xo - xy$x)
	expect_equal(
		y_ellipse_top(xy$x + xo, xc = xo, yc = yo, rx = 1, ry = 2, a = 45, origin_at_bottom = TRUE),
		yo + xy$y
	)
	expect_equal(
		x_ellipse_left(
			xy$y + yo,
			xc = xo,
			yc = yo,
			rx = 1,
			ry = 2,
			a = 45,
			origin_at_bottom = TRUE
		),
		xo + xy$x
	)
	expect_equal(
		x_ellipse_right(
			-xy$y + yo,
			xc = xo,
			yc = yo,
			rx = 1,
			ry = 2,
			a = 45,
			origin_at_bottom = TRUE
		),
		xo - xy$x
	)

	# angle = 135
	expect_equal(y_ellipse_bottom(xy$x + xo, xc = xo, yc = yo, rx = 1, ry = 2, a = 135), yo + xy$y)
	expect_equal(
		y_ellipse_bottom(
			xy$x + xo,
			xc = xo,
			yc = yo,
			rx = 1,
			ry = 2,
			a = 135,
			origin_at_bottom = TRUE
		),
		yo - xy$y
	)

	# angle = -45
	expect_equal(y_ellipse_top(-xy$x + xo, xc = xo, yc = yo, rx = 1, ry = 2, a = -45), yo - xy$y)
	expect_equal(x_ellipse_left(xy$y + yo, xc = xo, yc = yo, rx = 1, ry = 2, a = -45), xo + xy$x)
	expect_equal(x_ellipse_right(-xy$y + yo, xc = xo, yc = yo, rx = 1, ry = 2, a = -45), xo - xy$x)
	expect_equal(
		y_ellipse_top(
			-xy$x + xo,
			xc = xo,
			yc = yo,
			rx = 1,
			ry = 2,
			a = -45,
			origin_at_bottom = TRUE
		),
		yo + xy$y
	)
	expect_equal(
		x_ellipse_right(
			xy$y + yo,
			xc = xo,
			yc = yo,
			rx = 1,
			ry = 2,
			a = -45,
			origin_at_bottom = TRUE
		),
		xo - xy$x
	)
})
