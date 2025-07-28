#' Isotoxal `2n`-gon path convenience wrapper
#'
#' `ISOTOXAL_2NGON()` is a wrapper around `POLYGON()`
#' to create isotoxal `2n`-gon polygon shaped paths.
#' Its vectorized in its `x`, `y`, `r`, `s`, `n`, `a`, and `offset` arguments.
#' @inheritParams POLYGON
#' @inheritParams M
#' @param r The outer radius of the isotoxal `2n`-gon polygon.
#' @param s The inner radius of the isotoxal `2n`-gon polygon as a **fraction** of the outer radius i.e. the inner radius = `s * r`.  [affiner::isotoxal_2ngon_inner_radius()] may be of help computing this.
#' @param n The number of outer vertices.
#' @param a The angle of the first outer vertex (will be coerced by [affiner::degrees()]).
#' @param ... Passed to [POLYGON()].
#' @seealso [POLYGON()] and [REGULAR_NGON()].  See <https://en.wikipedia.org/wiki/Isotoxal_figure#Isotoxal_polygons> and <https://en.wikipedia.org/wiki/Star_polygon#Isotoxal_star_simple_polygons> for more information on isotoxal polygons.
#' @return A [dee()] object.
#' @examples
#' # A |5/2| star e.g. the *verda stelo*
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 5, d = 2)
#' d <- ISOTOXAL_2NGON(x = 5, y = 5, r = 3, s = s, n = 5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "green", stroke = "none")
#' }
#' # "lozenge" shape most often has acute angles of 45 degrees
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 2, alpha = 45)
#' d <- ISOTOXAL_2NGON(x = 5, y = 5, r = 4, s = s, n = 2)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "cyan", stroke = "black", stroke_width = 4)
#' }
#' @export
ISOTOXAL_2NGON <- function(x, y, r, s, n, a = 90, ...,
                           offset = 0,
                           origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                           height = getOption("dee.height")) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(origin_at_bottom)) {
        y <- height - y
        return (ISOTOXAL_2NGON(x, y, r, s, n, a, ...,
                               offset = offset, origin_at_bottom = FALSE))
    } 
    r <- as.numeric(r)
    s <- as.numeric(s)
    stopifnot(all(s < r))
    a <- -as.numeric(degrees(a), "degrees")
    MoreArgs <- list(...)
    MoreArgs$origin_at_bottom <- FALSE
    .mapply(ISOTOXAL_2NGON_HELPER,
            list(x = x, y = y, r = r, s = s, n = n, a = a, offset = offset),
            MoreArgs) |>
        Reduce(`+.dee`, x = _)
}

ISOTOXAL_2NGON_HELPER <- function(x, y, r, s, n, a, offset, ...) {

    xyo <- as_coord2d(degrees(seq(a, by = 360 / n, length.out = n)),
                      radius = r)$
        translate(x = x, y = y)
    xyi <- as_coord2d(degrees(seq(a + 360 / n / 2, by = 360 / n, length.out = n)),
                      radius = s * r)$
        translate(x = x, y = y)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    POLYGON(x, y, ..., offset = offset)
}

#' Regular `n`-gon path convenience wrapper
#'
#' `REGULAR_NGON()` is a wrapper around `POLYGON()`
#' to create regular polygon shaped paths.
#' Its vectorized in its `x`, `y`, `r`, `n`, `a`, and `offset` arguments.
#' @inheritParams POLYGON
#' @inheritParams M
#' @param r The radius of the regular polygon (where the vertices lay on).
#' @param n The number of vertices.
#' @param a The angle of the first vertex (will be coerced by [affiner::degrees()]).
#' @param ... Passed to [POLYGON()].
#' @seealso [POLYGON()] and [ISOTOXAL_2NGON()].
#' @return A [dee()] object.
#' @examples
#' # A pentagon
#' d5 <- REGULAR_NGON(x = 5, y = 5, r = 4, n = 5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d5, height = 10, width = 10,
#'        fill = "magenta", stroke = "black", stroke_width = 4)
#' }
#' # A hexagon
#' d6 <- REGULAR_NGON(x = 5, y = 5, r = 4, n = 6, offset = c(0, -1))
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d6, height = 10, width = 10,
#'        attrs = list(fill_rule = "evenodd"),
#'        fill = "magenta", stroke = "black", stroke_width = 4)
#' }
#' @export
REGULAR_NGON <- function(x, y, r, n, a = 90, ...,
                         offset = 0,
                         origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                         height = getOption("dee.height")) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(origin_at_bottom)) {
        y <- height - y
        return (REGULAR_NGON(x, y, r, n, a, ...,
                             offset = offset, origin_at_bottom = FALSE))
    } 
    r <- as.numeric(r)
    a <- -as.numeric(degrees(a), "degrees")
    MoreArgs <- list(...)
    MoreArgs$origin_at_bottom <- FALSE
    .mapply(REGULAR_NGON_HELPER,
            list(x = x, y = y, r = r, n = n, a = a, offset = offset),
            MoreArgs) |>
        Reduce(`+.dee`, x = _)
}

REGULAR_NGON_HELPER <- function(x, y, r, n, a, offset, ...) {

    xy <- as_coord2d(degrees(seq(a, by = 360 / n, length.out = n)),
                     radius = r)$
        translate(x = x, y = y)
    POLYGON(xy$x, xy$y, ..., offset = offset)
}
