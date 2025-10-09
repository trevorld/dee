test_that("`dee()`", {
	d <- dee("M 10,30") + dee("v 30")
	expect_true(inherits(d, "dee"))
	expect_true(inherits(format(d), "character"))
	s <- capture.output({
		d2 <- print(d)
	})
	expect_equal(d, d2)
	expect_equal(s, c("<dee[1]>", "M 10,30 v 30"))

	expect_true(inherits(c(d, d2), "dee"))

	skip_if_not_installed("omsvg")
	skip_if_not_installed("svgparser")
	grDevices::pdf(NULL)
	plot(d, height = 100, width = 100, stroke = "black", stroke_width = 4)
	invisible(grDevices::dev.off())
})
