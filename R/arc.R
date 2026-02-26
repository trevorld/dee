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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_top, ...) +
		A(rx = dx, ry = dy, x = x_right, y = y_bottom, sweep_flag = TRUE) +
		H(x = x_right - w, ...) +
		AZ(rx = dx - w, ry = dy - w, x = x_left, y = y_top + w, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_bottom, ...) +
		A(rx = dx, ry = dy, x = x_right, y = y_top, sweep_flag = TRUE) +
		V(y = y_top + w, ...) +
		AZ(rx = dx - w, ry = dy - w, x = x_left + w, y = y_bottom, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left + w, y_top, ...) +
		A(rx = dx - w, ry = dy - w, x = x_right, y = y_bottom - w, ...) +
		V(y = y_bottom) +
		AZ(rx = dx, ry = dy, x = x_left, y = y_top, sweep_flag = TRUE, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_bottom - w, ...) +
		A(rx = dx - w, ry = dy - w, x = x_right - w, y = y_top, ...) +
		H(x = x_right, ...) +
		AZ(rx = dx, ry = dy, x = x_left, y = y_bottom, sweep_flag = TRUE, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_bottom, ...) +
		A(rx = dx / 2, ry = dy, x = x_right, y = y_bottom, ..., sweep_flag = TRUE) +
		H(x = x_right - w, ...) +
		AZ(rx = dx / 2 - w, ry = dy - w, x = x_left + w, y = y_bottom, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_right, y_bottom, ...) +
		A(rx = dx, ry = dy / 2, x = x_right, y = y_top, ..., sweep_flag = TRUE) +
		V(y = y_top + w, ...) +
		AZ(rx = dx - w, ry = dy / 2 - w, x = x_right, y = y_bottom - w, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left + w, y_top, ...) +
		A(rx = dx / 2 - w, ry = dy - w, x = x_right - w, y = y_top, ...) +
		H(x = x_right, ...) +
		AZ(rx = dx / 2, ry = dy, x = x_left, y = y_top, ..., sweep_flag = TRUE)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_top, ...) +
		A(rx = dx, ry = dy / 2, x = x_left, y = y_bottom, ..., sweep_flag = TRUE) +
		V(y = y_bottom - w, ...) +
		AZ(rx = dx - w, ry = dy / 2 - w, x = x_left, y = y_top + w, ...)
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left + dx / 2, y_bottom, ...) +
		A(
			rx = dx / 2,
			ry = dy / 2,
			x = x_right,
			y = y_top + dy / 2,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		H(x = x_right - w, ...) +
		AZ(
			rx = dx / 2 - w,
			ry = dy / 2 - w,
			x = x_left + dx / 2,
			y = y_bottom - w,
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_right, y_top + dy / 2, ...) +
		A(
			rx = dx / 2,
			ry = dy / 2,
			x = x_left + dx / 2,
			y = y_top,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		V(y = y_top + w, ...) +
		AZ(
			rx = dx / 2 - w,
			ry = dy / 2 - w,
			x = x_right - w,
			y = y_top + dy / 2,
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left + dx / 2, y_top, ...) +
		A(
			rx = dx / 2,
			ry = dy / 2,
			x = x_left,
			y = y_top + dy / 2,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		H(x = x_left + w, ...) +
		AZ(
			rx = dx / 2 - w,
			ry = dy / 2 - w,
			x = x_left + dx / 2,
			y = y_top + w,
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
	rlang::local_options(dee.origin_at_bottom = FALSE)
	M(x_left, y_top + dy / 2, ...) +
		A(
			rx = dx / 2,
			ry = dy / 2,
			x = x_left + dx / 2,
			y = y_bottom,
			...,
			large_arc_flag = TRUE,
			sweep_flag = TRUE
		) +
		V(y = y_bottom - w, ...) +
		AZ(
			rx = dx / 2 - w,
			ry = dy / 2 - w,
			x = x_left + w,
			y = y_top + dy / 2,
			...,
			large_arc_flag = TRUE
		)
}
