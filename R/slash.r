#' Forward/backward slash path convenience wrapper
#'
#' `d_fslash()` is a wrapper around [MZ()] to create
#' a forward slash path convenience wrapper.
#' `d_bslash()` is a wrapper around [MZ()] to create
#' a backward slash path convenience wrapper.
#' They are vectorized in their `y_top`, `x_right`, `y_bottom`, `x_left`,
#' and `w` arguments.
#' @inheritParams M
#' @param y_top,x_right,y_bottom,x_left The most extreme x and y values of the slash shape.
#' @param w The (stroke) width of the slash "line".
#' @param nib,left,right The shape of the "nib" tracing the line.
#'                       This only affects the left and right ends.
#' @param ... Passed to [MZ()].
#' @return A [dee()] object.
#' @examples
#' d_h <- d_fslash(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_h, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_s <- d_bslash(2, 8, 8, 2, 1, nib = "square")
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_s, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_v <- d_fslash(2, 8, 8, 2, 1, nib = "vertical")
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_v, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_d <- d_bslash(2, 8, 8, 2, 1, nib = "diagonal")
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_d, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @seealso [d_arc1()], [d_arc2()], [d_arc3()], and [d_arc4()] for curved (quarter-elliptical arc) lines.
#' @export
d_fslash <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL),
	nib = c("horizontal", "diagonal", "square", "vertical"),
	left = nib,
	right = nib
) {
	nibs <- c("horizontal", "diagonal", "square", "vertical")
	left <- match.arg(left[[1L]], nibs)
	right <- match.arg(right[[1L]], nibs)
	y_top <- as.numeric(y_top)
	x_right <- as.numeric(x_right)
	y_bottom <- as.numeric(y_bottom)
	x_left <- as.numeric(x_left)
	w <- as.numeric(w)
	if (isTRUE(origin_at_bottom)) {
		y_top <- height - y_top
		y_bottom <- height - y_bottom
	}
	stopifnot(all(y_top < y_bottom), all(x_left < x_right))
	fn <- switch(
		left,
		diagonal = switch(
			right,
			diagonal = fslash_dd,
			horizontal = fslash_dh,
			square = fslash_ds,
			vertical = fslash_dv
		),
		horizontal = switch(
			right,
			diagonal = fslash_hd,
			horizontal = fslash_hh,
			square = fslash_hs,
			vertical = fslash_hv
		),
		square = switch(
			right,
			diagonal = fslash_sd,
			horizontal = fslash_sh,
			square = fslash_ss,
			vertical = fslash_sv
		),
		vertical = switch(
			right,
			diagonal = fslash_vd,
			horizontal = fslash_vh,
			square = fslash_vs,
			vertical = fslash_vv
		)
	)
	dots <- list(y_top = y_top, x_right = x_right, y_bottom = y_bottom, x_left = x_left, w = w)
	.mapply(fn, dots, list(...)) |> Reduce(`+.dee`, x = _)
}

h_fslash_hh <- function(dx, dy, w) {
	stopifnot(dy > w)
	a <- (dy^2 - w^2) / w^2 # multiplied by a 2 which cancels out
	h <- (sqrt(dx^2 + a * (dy^2 + dx^2)) - dx) / a
	stopifnot(h < dx)
	h
}
v_fslash_vv <- function(dx, dy, w) {
	h_fslash_hh(dy, dx, w)
}

h_fslash_vh <- function(dx, dy, w) {
	m <- dy / dx
	h <- sqrt(1 + m^2) * w / m
	stopifnot(h < dx)
	h
}
v_fslash_vh <- function(dx, dy, w) {
	m <- dy / dx
	v <- sqrt(1 + m^2) * w
	stopifnot(v < dy)
	v
}

fslash_hh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_hh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_right - h, x_right, x_left + h)
	y <- c(y_bottom, y_top, y_top, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_hs <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_right - h1, x_right, x_right, x_left + h1 + h2)
	y <- c(y_bottom, y_top, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_hd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_right - h1, x_right, x_left + h1 + h2)
	y <- c(y_bottom, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_sh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_right - h1 - h2, x_right, x_left + h1)
	y <- c(y_bottom, y_bottom - v, y_top, y_top, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_dv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_right, x_right, x_left + h)
	y <- c(y_bottom - v1, y_top, y_top + v1 + v2, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_vd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_right - h, x_right)
	y <- c(y_bottom, y_bottom - v1 - v2, y_top, y_top + v1)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_sv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_right, x_right, x_left + h)
	y <- c(y_bottom, y_bottom - v1, y_top, y_top + v1 + v2, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_vs <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_right - h, x_right, x_right)
	y <- c(y_bottom, y_bottom - v1 - v2, y_top, y_top, y_top + v1)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_dh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_right - h1 - h2, x_right, x_left + h1)
	y <- c(y_bottom - v, y_top, y_top, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_hv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_right, x_right, x_left + h)
	y <- c(y_bottom, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_ss <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_right - h, x_right, x_right, x_left + h)
	y <- c(y_bottom, y_bottom - v, y_top, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_sd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_right - h, x_right, x_left + h)
	y <- c(y_bottom, y_bottom - v, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_dd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_right - h, x_right, x_left + h)
	y <- c(y_bottom - v, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_ds <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_right - h, x_right, x_right, x_left + h)
	y <- c(y_bottom - v, y_top, y_top, y_top + v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_vh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left, x_right - h, x_right)
	y <- c(y_bottom, y_bottom - v, y_top, y_top)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_vv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v <- v_fslash_vv(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left, x_right, x_right)
	y <- c(y_bottom, y_bottom - v, y_top, y_top + v)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}

#' @rdname d_fslash
#' @export
d_bslash <- function(
	y_top,
	x_right,
	y_bottom,
	x_left,
	w,
	...,
	origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
	height = getOption("dee.height", NULL),
	nib = c("horizontal", "diagonal", "square", "vertical"),
	left = nib,
	right = nib
) {
	nibs <- c("horizontal", "diagonal", "square", "vertical")
	left <- match.arg(left[[1L]], nibs)
	right <- match.arg(right[[1L]], nibs)
	y_top <- as.numeric(y_top)
	x_right <- as.numeric(x_right)
	y_bottom <- as.numeric(y_bottom)
	x_left <- as.numeric(x_left)
	w <- as.numeric(w)
	if (isTRUE(origin_at_bottom)) {
		y_top <- height - y_top
		y_bottom <- height - y_bottom
	}
	stopifnot(all(y_top < y_bottom), all(x_left < x_right))
	fn <- switch(
		left,
		diagonal = switch(
			right,
			diagonal = bslash_dd,
			horizontal = bslash_dh,
			square = bslash_ds,
			vertical = bslash_dv
		),
		horizontal = switch(
			right,
			diagonal = bslash_hd,
			horizontal = bslash_hh,
			square = bslash_hs,
			vertical = bslash_hv
		),
		square = switch(
			right,
			diagonal = bslash_sd,
			horizontal = bslash_sh,
			square = bslash_ss,
			vertical = bslash_sv
		),
		vertical = switch(
			right,
			diagonal = bslash_vd,
			horizontal = bslash_vh,
			square = bslash_vs,
			vertical = bslash_vv
		)
	)
	dots <- list(y_top = y_top, x_right = x_right, y_bottom = y_bottom, x_left = x_left, w = w)
	.mapply(fn, dots, list(...)) |> Reduce(`+.dee`, x = _)
}

bslash_hh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_hh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left + h, x_right, x_right - h)
	y <- c(y_top, y_top, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_hs <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left + h1 + h2, x_right, x_right, x_right - h1)
	y <- c(y_top, y_top, y_bottom - v, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_hd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left + h1 + h2, x_right, x_right - h1)
	y <- c(y_top, y_top, y_bottom - v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_sh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_left + h1, x_right, x_right - h1 - h2)
	y <- c(y_top + v, y_top, y_top, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_dh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h1 <- h_fslash_hh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	h2 <- h_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left - h1, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left + h1, x_right, x_right - h1 - h2)
	y <- c(y_top + v, y_top, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_hv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left + h, x_right, x_right)
	y <- c(y_top, y_top, y_bottom - v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_ss <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_left + h, x_right, x_right, x_right - h)
	y <- c(y_top + v, y_top, y_top, y_bottom - v, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_sd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left, x_left + h, x_right, x_right - h)
	y <- c(y_top + v, y_top, y_top, y_bottom - v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_sv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_left + h, x_right, x_right)
	y <- c(y_top + v1, y_top, y_top, y_bottom - v1 - v2, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_dv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left + h, x_right, x_right)
	y <- c(y_top + v1, y_top, y_bottom - v1 - v2, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_vd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_right, x_right - h)
	y <- c(y_top + v1 + v2, y_top, y_bottom - v1, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_vs <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v1 <- v_fslash_vv(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v2 <- v_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top - v1, 0.5 * w)
	x <- c(x_left, x_left, x_right, x_right, x_right - h)
	y <- c(y_top + v1 + v2, y_top, y_bottom - v1, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_dd <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left + h, x_right, x_right - h)
	y <- c(y_top + v, y_top, y_bottom - v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_ds <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, 0.5 * w)
	x <- c(x_left, x_left + h, x_right, x_right, x_right - h)
	y <- c(y_top + v, y_top, y_bottom - v, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}

bslash_vh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left, x_right, x_right - h)
	y <- c(y_top + v, y_top, y_bottom, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_vv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
	v <- v_fslash_vv(x_right - x_left, y_bottom - y_top, w)
	x <- c(x_left, x_left, x_right, x_right)
	y <- c(y_top + v, y_top, y_bottom - v, y_bottom)
	MZ(x, y, ..., origin_at_bottom = FALSE)
}
