#' Find ellipse coordinate
#'
#' `x_ellipse_left()` returns the x-coordinate of the leftmost point and
#' `x_ellipse_right()` returns the x-coordinate of the rightmost point on a
#' (possibly rotated) ellipse at a given y value.
#' `y_ellipse_top()` returns the y-coordinate of the topmost point and
#' `y_ellipse_bottom()` returns the y-coordinate of the bottommost point on a
#' (possibly rotated) ellipse at a given x value.
#'   `x_ellipse_left()` and `x_ellipse_right()` return `NaN` if `y` is
#'   outside the vertical extent of the ellipse.
#'   `y_ellipse_top()` and `y_ellipse_bottom()` return `NaN` if `x` is
#'   outside the horizontal extent of the ellipse.
#' @param y The y value at which to evaluate the ellipse.
#' @param x The x value at which to evaluate the ellipse.
#' @param xc,yc The center of the ellipse.
#' @param rx,ry The x and y radii of the (unrotated) ellipse.
#' @param a The counter-clockwise angle of rotation of the ellipse (will be coerced by [affiner::degrees()]).
#' @param ... Should be empty.
#' @param origin_at_bottom If `TRUE`, the origin is at the bottom of the
#'   coordinate system (i.e. y increases upward).
#' @return A numeric value.
#' @examples
#' # Unrotated ellipse centered at (5, 5) with rx=3, ry=4
#' x_ellipse_left(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0)
#' x_ellipse_right(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0)
#' # Rotated 90 degrees: rx and ry swap roles
#' x_ellipse_left(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90)
#' x_ellipse_right(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90)
#' # Unrotated ellipse centered at (5, 5) with rx=3, ry=4
#' y_ellipse_top(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0)
#' y_ellipse_bottom(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 0)
#' # Rotated 90 degrees: rx and ry swap roles
#' y_ellipse_top(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90)
#' y_ellipse_bottom(5, xc = 5, yc = 5, rx = 3, ry = 4, a = 90)
#' @seealso [A()] and [d_ellipse()].
#' @export
x_ellipse_left <- function(
	y,
	xc,
	yc,
	rx,
	ry = rx,
	a = 0,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE)
) {
	check_dots_empty()
	if (isTRUE(origin_at_bottom)) {
		y <- yc - (y - yc)
		return(x_ellipse_left(y, xc, yc, rx, ry, a, origin_at_bottom = FALSE))
	}
	b <- x_ellipse_both(y, xc, yc, rx, ry, a)
	pmin(b$x1, b$x2)
}

#' @rdname x_ellipse_left
#' @export
x_ellipse_right <- function(
	y,
	xc,
	yc,
	rx,
	ry = rx,
	a = 0,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE)
) {
	check_dots_empty()
	if (isTRUE(origin_at_bottom)) {
		y <- yc - (y - yc)
		return(x_ellipse_right(y, xc, yc, rx, ry, a, origin_at_bottom = FALSE))
	}
	b <- x_ellipse_both(y, xc, yc, rx, ry, a)
	pmax(b$x1, b$x2)
}

x_ellipse_both <- function(y, xc, yc, rx, ry, a) {
	a <- -degrees(a)
	dy <- y - yc
	A <- rx * sin(a)
	B <- ry * cos(a)
	R <- sqrt(A^2 + B^2)
	phi <- atan2(B, A)
	da <- acos(dy / R)
	t1 <- phi + da
	t2 <- phi - da
	ca <- cos(a)
	sa <- sin(a)
	list(
		x1 = xc + rx * cos(t1) * ca - ry * sin(t1) * sa,
		x2 = xc + rx * cos(t2) * ca - ry * sin(t2) * sa
	)
}

#' @rdname x_ellipse_left
#' @export
y_ellipse_top <- function(
	x,
	xc,
	yc,
	rx,
	ry = rx,
	a = 0,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE)
) {
	check_dots_empty()
	if (isTRUE(origin_at_bottom)) {
		y <- y_ellipse_top(x, xc, yc, rx, ry, a, origin_at_bottom = FALSE)
		return(yc - (y - yc))
	}
	b <- y_ellipse_both(x, xc, yc, rx, ry, a)
	pmin(b$y1, b$y2)
}

#' @rdname x_ellipse_left
#' @export
y_ellipse_bottom <- function(
	x,
	xc,
	yc,
	rx,
	ry = rx,
	a = 0,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE)
) {
	check_dots_empty()
	if (isTRUE(origin_at_bottom)) {
		y <- y_ellipse_bottom(x, xc, yc, rx, ry, a, origin_at_bottom = FALSE)
		return(yc - (y - yc))
	}
	b <- y_ellipse_both(x, xc, yc, rx, ry, a)
	pmax(b$y1, b$y2)
}

y_ellipse_both <- function(x, xc, yc, rx, ry, a) {
	a <- -degrees(a)
	dx <- x - xc
	A <- rx * cos(a)
	B <- -ry * sin(a)
	R <- sqrt(A^2 + B^2)
	phi <- atan2(B, A)
	da <- acos(dx / R)
	t1 <- phi + da
	t2 <- phi - da
	ca <- cos(a)
	sa <- sin(a)
	list(
		y1 = yc + rx * cos(t1) * sa + ry * sin(t1) * ca,
		y2 = yc + rx * cos(t2) * sa + ry * sin(t2) * ca
	)
}
