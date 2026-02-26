test_that("`dee()`", {
	d <- dee("M 10,30") + dee("v 30")
	expect_true(inherits(d, "dee"))
	expect_false(inherits(as.character(d), "dee"))
	expect_true(inherits(format(d), "character"))
	s <- capture.output({
		d2 <- print(d)
	})
	expect_equal(d, d2)
	expect_equal(s, c("<dee[1]>", "M 10,30 v 30"))

	ds <- c(d, d2)
	expect_true(inherits(ds, "dee"))
	expect_equal(length(ds), 2L)

	expect_true(inherits(ds[2L], "dee"))
	expect_equal(length(ds[2L]), 1L)
	expect_true(inherits(ds[[2L]], "dee"))
	expect_equal(length(ds[[2L]]), 1L)

	ds[3L] <- d2
	expect_true(inherits(ds, "dee"))
	ds[[4L]] <- d2
	expect_true(inherits(ds, "dee"))
	expect_error({
		ds[5L] <- "boo"
	})
	expect_error({
		ds[[5L]] <- "boo"
	})

	skip_if_not_installed("omsvg")
	skip_if_not_installed("svgparser")
	grDevices::pdf(NULL)
	plot(d, height = 100, width = 100, stroke = "black", stroke_width = 4)
	invisible(grDevices::dev.off())
})

test_that("`as_coord2d.dee()`", {
	d <- d_rect(5, 5, 2, 2)
	xy <- as_coord2d(d)
	expect_equal(xy$x, c(4, 4, 6, 6))
	expect_equal(xy$y, c(4, 6, 6, 4))
	xy <- as_coord2d(d, origin_at_bottom = TRUE, height = 10)
	expect_equal(xy$x, c(4, 4, 6, 6))
	expect_equal(xy$y, c(6, 4, 4, 6))
})
