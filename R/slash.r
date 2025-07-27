#' Forward/backward slash path convenience wrapper
#'
#' `FSLASH()` is a wrapper around `MZ()` to create
#' a forward slash path convenience wrapper.
#' `BSLASH()` is a wrapper around `MZ()` to create
#' a backward slash path convenience wrapper.
#' They are vectorized in their `y_top`, `x_right`, `y_bottom`, `x_left`,
#' and `w` arguments. 
#' @inheritParams M
#' @param y_top,x_right,y_bottom,x_left The most extreme x and y values of the slash shape.
#' @param w The (stroke) width of the slash "line".
#' @param nib,left,right The shape of the "nib" tracing the line.
#'                       This only affects the left and right ends.
#' @param ... Passed to [MZ()].
#' @examples
#' d_h <- FSLASH(2, 8, 8, 2, 1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_h, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_sq <- BSLASH(2, 8, 8, 2, 1, nib = "square")
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_sq, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' d_v <- FSLASH(2, 8, 8, 2, 1, nib = "vertical")
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_v, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
FSLASH <- function(y_top, x_right, y_bottom, x_left, w, ...,
                   origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                   height = getOption("dee.height", NULL),
                   nib = c("horizontal", "square", "vertical"),
                   left = nib, right = nib) {
    left <- match.arg(left, c("horizontal", "square", "vertical"))
    right <- match.arg(right, c("horizontal", "square", "vertical"))
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
    fn <- switch(left,
           horizontal = switch(right,
               horizontal = fslash_hh,
               square = nib_stop(left, right),
               vertical = fslash_hv),
           square = switch(right,
               horizontal = nib_stop(left, right),
               square = fslash_ss,
               vertical = nib_stop(left, right)),
           vertical = switch(right,
               horizontal = fslash_vh,
               square = nib_stop(left, right),
               vertical = fslash_vv))
    dots <- list(y_top = y_top, x_right = x_right, y_bottom = y_bottom, x_left = x_left, w = w)
    .mapply(fn, dots, list(...)) |> Reduce(`+.dee`, x = _)
}

# can use for `vv` if swap `dx` for `dy`
h_fslash_hh <- function(dx, dy, w) {
    stopifnot(dy > w)
    a <- (dy^2 - w^2) / w^2 # multiplied by a 2 which cancels out
    h <- (sqrt(dx^2 + a * (dy^2 + dx^2)) - dx) / a
    stopifnot(h < dx)
    h
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
fslash_vh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
    h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
    v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
    x <- c(x_left, x_left, x_right - h, x_right)
    y <- c(y_bottom, y_bottom - v, y_top, y_top)
    MZ(x, y, ..., origin_at_bottom = FALSE)
}
fslash_vv <- function(y_top, x_right, y_bottom, x_left, w, ...) {
    v <- h_fslash_hh(y_bottom - y_top, x_right - x_left, w)
    x <- c(x_left, x_left, x_right, x_right)
    y <- c(y_bottom, y_bottom - v, y_top, y_top + v)
    MZ(x, y, ..., origin_at_bottom = FALSE)
}

#' @rdname FSLASH
#' @export
BSLASH <- function(y_top, x_right, y_bottom, x_left, w, ...,
                   origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                   height = getOption("dee.height", NULL),
                   nib = c("horizontal", "square", "vertical"),
                   left = nib, right = nib) {
    left <- match.arg(left, c("horizontal", "square", "vertical"))
    right <- match.arg(right, c("horizontal", "square", "vertical"))
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
    fn <- switch(left,
           horizontal = switch(right,
               horizontal = bslash_hh,
               square = nib_stop(left, right),
               vertical = bslash_hv),
           square = switch(right,
               horizontal = nib_stop(left, right),
               square = bslash_ss,
               vertical = nib_stop(left, right)),
           vertical = switch(right,
               horizontal = bslash_vh,
               square = nib_stop(left, right),
               vertical = bslash_vv))
    dots <- list(y_top = y_top, x_right = x_right, y_bottom = y_bottom, x_left = x_left, w = w)
    .mapply(fn, dots, list(...)) |> Reduce(`+.dee`, x = _)
}

bslash_hh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
    h <- h_fslash_hh(x_right - x_left, y_bottom - y_top, w)
    x <- c(x_left, x_left + h, x_right, x_right - h)
    y <- c(y_top, y_top, y_bottom, y_bottom)
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

bslash_vh <- function(y_top, x_right, y_bottom, x_left, w, ...) {
    h <- h_fslash_vh(x_right - x_left, y_bottom - y_top, w)
    v <- v_fslash_vh(x_right - x_left, y_bottom - y_top, w)
    x <- c(x_left, x_left, x_right, x_right - h)
    y <- c(y_top + v, y_top, y_bottom, y_bottom)
    MZ(x, y, ..., origin_at_bottom = FALSE)
}
bslash_vv <- function(y_top, x_right, y_bottom, x_left, w, ...)  {
    v <- h_fslash_hh(y_bottom - y_top, x_right - x_left, w)
    x <- c(x_left, x_left, x_right, x_right)
    y <- c(y_top + v, y_top, y_bottom - v, y_bottom)
    MZ(x, y, ..., origin_at_bottom = FALSE)
}

nib_stop <- function(left, right) {
    stop(paste0("`left = ", dQuote(left), 
                "` and `right = ", dQuote(right), " not supported"))
}
