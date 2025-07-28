#' Rectangle path convenience wrapper
#'
#' `d_rect()` is a wrapper around `MZ()` to create
#' rectangle shaped paths.
#' It's vectorized in its `x`, `y`, `w`, and `h` arguments.
#' @inheritParams M
#' @param w,h The width and height of the rectangle.
#' @param ... Passed to [MZ()].
#' @return A [dee()] object.
#' @examples
#' d_rect(x = 5, y = 5, w = 4, h = 6)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_rect(x = 5, y = 5, w = 4, h = 6),
#'        height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
d_rect <- function(x, y = NULL, w, h, ...) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    .mapply(d_rect_helper, list(x = x, y = y, w = w, h = h), list(...)) |>
        Reduce(`+.dee`, x = _)
}

d_rect_helper <- function(x, y, w, h, ...) {
    xl <- x - 0.5 * w
    xr <- x + 0.5 * w
    yb <- y - 0.5 * h
    yt <- y + 0.5 * h
    x <- c(xl, xl, xr, xr)
    y <- c(yb, yt, yt, yb)
    MZ(x, y, ...)
}

#' Ellipse path convenience wrapper
#'
#' `d_ellipse()` is a wrapper around `M()` and `AZ()` to create
#' ellipse shaped paths.
#' It's vectorized in its `x`, `y`, `rx`, and `ry` arguments.
#' `d_circle()` is a special case to create circle shaped paths.
#' It's vectorized in its `x`, `y`, and `r` arguments.
#' @inheritParams M
#' @param r The radius of the circle.
#' @param rx,ry The radii of the ellipse.
#' @param ... Passed to [M()] and [AZ()].
#' @return A [dee()] object.
#' @examples
#' d_circle(x = 5, y = 5, r = 2)
#' d_ellipse(x = 5, y = 5, rx = 2, ry = 3)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d_ellipse(x = 5, y = 5, rx = 2, ry = 3),
#'        height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
d_ellipse <- function(x, y = NULL, rx, ry = rx, ...) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    .mapply(d_ellipse_helper,
            list(x = x, y = y, rx = rx, ry = ry),
            list(...)) |>
        Reduce(`+.dee`, x = _)
}

#' @rdname d_ellipse
#' @export
d_circle <- function(x, y = NULL, r, ...) {
    d_ellipse(x, y, r, r, ...)
}

d_ellipse_helper <- function(x, y, rx, ry, ...) {
    M(x, y + ry, ...) +
        AZ(rx, ry, 0, 0, 0, x, c(y - ry, y + ry), ...)
}

#' Polygon path convenience wrapper
#'
#' `d_polygon()` is a wrapper around `MZ()` to create
#' polygon shaped paths.
#' If the argument `offset` is nonzero
#' will use [polyclip::polyoffset()] to compute an offset region.
#' It's vectorized in its `offset` argument.
#' @inheritParams M
#' @param ... Passed to [MZ()].
#' @param offset If a positive number the distance for *outward* polygon offsetting.  If a negative number the distance for *inward* polygon offsetting.
#' @param linejoin If `offset` is nonzero the type of join operation
#'                 to use at each vertex when computing the offset region.
#' @param miterlimit Tolerance parameter if `offset` is nonzero
#'                   and `linejoin = "miter"`.  See [polyclip::polyoffset()].
#' @return A [dee()] object.
#' @examples
#' l <- list(x = c(2, 5, 8, 5), y = c(5, 8, 5, 2))
#' d <- d_polygon(l, offset = c(1, 0, -1))
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        attrs = list(fill_rule = "evenodd"),
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @seealso [polyclip::polyoffset()] for details on computing the offset region when `offset` is non-zero.
#' @export
d_polygon <- function(x, y = NULL, ...,
                    offset = 0,
                    linejoin = c("miter", "round"),
                    miterlimit = 4) {
    if (length(offset) == 1L && offset == 0)
        return (MZ(x, y, ...))
    stopifnot(requireNamespace("polyclip", quietly = TRUE))
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    jointype <- match.arg(linejoin)
    lapply(offset, d_polygon_helper, ..., x = x, y = y, jointype = jointype, miterlim = miterlimit) |>
        Reduce(`+.dee`, x = _)
}

d_polygon_helper <- function(offset, ..., x, y, jointype, miterlim) {
    xy <- polyclip::polyoffset(list(x = x, y = y), offset,
                               jointype = jointype, miterlim = miterlim)
    if (length(xy) == 0L) {
        rlang::abort(paste0("`polyoffset()` did not return a polygon for `offset =`", offset, "`"))
    }
    MZ(xy, ...)
}
