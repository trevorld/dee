arc_setup <- function(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height) {
	y_top <- as.numeric(y_top)
	x_right <- as.numeric(x_right)
	y_bottom <- as.numeric(y_bottom)
	x_left <- as.numeric(x_left)
	w <- as.numeric(w)
	if (isTRUE(origin_at_bottom)) {
		y_top <- height - y_top
		y_bottom <- height - y_bottom
	}
	dx <- x_right - x_left
	dy <- y_bottom - y_top
	stopifnot(all(w < dx), all(w < dy))
	list(
		y_top = y_top,
		x_right = x_right,
		y_bottom = y_bottom,
		x_left = x_left,
		w = w,
		dx = dx,
		dy = dy
	)
}

#' Elliptical arc path convenience wrapper
#'
#' `d_arc1()` `d_arc2()`, `d_arc3()`, `d_arc4()`,
#' `d_arc12()`, `d_arc23()`, `d_arc34()`, `d_arc41()`,
#' `d_arc123()`, `d_arc234()`, `d_arc341()`, and `d_arc412()`
#' are wrappers to create elliptical arc paths.
#' They are vectorized in their `y_top`, `x_right`, `y_bottom`, `x_left`,
#' and `w` arguments.
#' The numbers after `d_arc` refer to the quadrants of a unit circle (in counterclockwise order).
#' and give an indication of what the shape of the arc looks like.
#' @inheritParams M
#' @param y_top,x_right,y_bottom,x_left The most extreme x and y values of the quarter circular arc shape.
#' @param w The (stroke) width of the arc "line".
#' @param ... Passed to [M()], [A()], and [L()].
#' @seealso [d_fslash()] and [d_bslash()]
#' @examples
#' d_1 <- d_arc1(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_1, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_2 <- d_arc2(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_2, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_34 <- d_arc34(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_34, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_412 <- d_arc412(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_412, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @return A [dee()] object.
#' @rdname d_arc
#' @export
d_arc1 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_top, ...) +
		A(rx = p$dx, ry = p$dy, x = p$x_right, y = p$y_bottom, sweep_flag = TRUE) +
		H(x = p$x_right - p$w, ...) +
		AZ(rx = p$dx - p$w, ry = p$dy - p$w, x = p$x_left, y = p$y_top + p$w, ...)
}

#' @rdname d_arc
#' @export
d_arc2 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_bottom, ...) +
		A(rx = p$dx, ry = p$dy, x = p$x_right, y = p$y_top, sweep_flag = TRUE) +
		V(y = p$y_top + p$w, ...) +
		AZ(rx = p$dx - p$w, ry = p$dy - p$w, x = p$x_left + p$w, y = p$y_bottom, ...)
}

#' @rdname d_arc
#' @export
d_arc3 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left + p$w, p$y_top, ...) +
		A(rx = p$dx - p$w, ry = p$dy - p$w, x = p$x_right, y = p$y_bottom - p$w, ...) +
		V(y = p$y_bottom) +
		AZ(rx = p$dx, ry = p$dy, x = p$x_left, y = p$y_top, sweep_flag = TRUE, ...)
}

#' @rdname d_arc
#' @export
d_arc4 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_bottom - p$w, ...) +
		A(rx = p$dx - p$w, ry = p$dy - p$w, x = p$x_right - p$w, y = p$y_top, ...) +
		H(x = p$x_right, ...) +
		AZ(rx = p$dx, ry = p$dy, x = p$x_left, y = p$y_bottom, sweep_flag = TRUE, ...)
}

#' @rdname d_arc
#' @export
d_arc12 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_bottom, ...) +
		A(rx = p$dx / 2, ry = p$dy, x = p$x_right, y = p$y_bottom, ..., sweep_flag = TRUE) +
		H(x = p$x_right - p$w, ...) +
		AZ(rx = p$dx / 2 - p$w, ry = p$dy - p$w, x = p$x_left + p$w, y = p$y_bottom, ...)
}

#' @rdname d_arc
#' @export
d_arc23 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_right, p$y_bottom, ...) +
		A(rx = p$dx, ry = p$dy / 2, x = p$x_right, y = p$y_top, ..., sweep_flag = TRUE) +
		V(y = p$y_top + p$w, ...) +
		AZ(rx = p$dx - p$w, ry = p$dy / 2 - p$w, x = p$x_right, y = p$y_bottom - p$w, ...)
}

#' @rdname d_arc
#' @export
d_arc34 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left + p$w, p$y_top, ...) +
		A(rx = p$dx / 2 - p$w, ry = p$dy - p$w, x = p$x_right - p$w, y = p$y_top, ...) +
		H(x = p$x_right, ...) +
		AZ(rx = p$dx / 2, ry = p$dy, x = p$x_left, y = p$y_top, ..., sweep_flag = TRUE)
}

#' @rdname d_arc
#' @export
d_arc41 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_top, ...) +
		A(rx = p$dx, ry = p$dy / 2, x = p$x_left, y = p$y_bottom, ..., sweep_flag = TRUE) +
		V(y = p$y_bottom - p$w, ...) +
		AZ(rx = p$dx - p$w, ry = p$dy / 2 - p$w, x = p$x_left, y = p$y_top + p$w, ...)
}

#' @rdname d_arc
#' @export
d_arc123 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left + p$dx / 2, p$y_bottom, ...) +
		A(
			rx = p$dx / 2,
			ry = p$dy / 2,
			x = p$x_right,
			y = p$y_top + p$dy / 2,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		H(x = p$x_right - p$w, ...) +
		AZ(
			rx = p$dx / 2 - p$w,
			ry = p$dy / 2 - p$w,
			x = p$x_left + p$dx / 2,
			y = p$y_bottom - p$w,
			...,
			large_arc_flag = TRUE
		)
}

#' @rdname d_arc
#' @export
d_arc234 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_right, p$y_top + p$dy / 2, ...) +
		A(
			rx = p$dx / 2,
			ry = p$dy / 2,
			x = p$x_left + p$dx / 2,
			y = p$y_top,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		V(y = p$y_top + p$w, ...) +
		AZ(
			rx = p$dx / 2 - p$w,
			ry = p$dy / 2 - p$w,
			x = p$x_right - p$w,
			y = p$y_top + p$dy / 2,
			...,
			large_arc_flag = TRUE
		)
}

#' @rdname d_arc
#' @export
d_arc341 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left + p$dx / 2, p$y_top, ...) +
		A(
			rx = p$dx / 2,
			ry = p$dy / 2,
			x = p$x_left,
			y = p$y_top + p$dy / 2,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		H(x = p$x_left + p$w, ...) +
		AZ(
			rx = p$dx / 2 - p$w,
			ry = p$dy / 2 - p$w,
			x = p$x_left + p$dx / 2,
			y = p$y_top + p$w,
			...,
			large_arc_flag = TRUE
		)
}

#' @rdname d_arc
#' @export
d_arc412 <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL)
) {
	p <- arc_setup(y_top, x_right, y_bottom, x_left, w, origin_at_bottom, height)
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(p$x_left, p$y_top + p$dy / 2, ...) +
		A(
			rx = p$dx / 2,
			ry = p$dy / 2,
			x = p$x_left + p$dx / 2,
			y = p$y_bottom,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		V(y = p$y_bottom - p$w, ...) +
		AZ(
			rx = p$dx / 2 - p$w,
			ry = p$dy / 2 - p$w,
			x = p$x_left + p$w,
			y = p$y_top + p$dy / 2,
			...,
			large_arc_flag = TRUE
		)
}
