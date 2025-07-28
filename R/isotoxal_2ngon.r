#' Isotoxal `2n`-gon path convenience wrapper
#'
#' `d_isotoxal_2ngon()` is a wrapper around `d_polygon()`
#' to create isotoxal `2n`-gon polygon shaped paths.
#' Its vectorized in its `x`, `y`, `r`, `s`, `n`, `a`, and `offset` arguments.
#' @inheritParams d_polygon
#' @inheritParams M
#' @param r The outer radius of the isotoxal `2n`-gon polygon.
#' @param s The inner radius of the isotoxal `2n`-gon polygon as a **fraction** of the outer radius i.e. the inner radius = `s * r`.  [affiner::isotoxal_2ngon_inner_radius()] may be of help computing this.
#' @param n The number of outer vertices.
#' @param a The angle of the first outer vertex (will be coerced by [affiner::degrees()]).
#' @param ... Passed to [d_polygon()].
#' @seealso [d_polygon()] and [d_regular_ngon()].  See <https://en.wikipedia.org/wiki/Isotoxal_figure#Isotoxal_polygons> and <https://en.wikipedia.org/wiki/Star_polygon#Isotoxal_star_simple_polygons> for more information on isotoxal polygons.
#' @return A [dee()] object.
#' @examples
#' # A |5/2| star e.g. the *verda stelo*
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 5, d = 2)
#' d <- d_isotoxal_2ngon(x = 5, y = 5, r = 3, s = s, n = 5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "green", stroke = "none")
#' }
#' # "lozenge" shape most often has acute angles of 45 degrees
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 2, alpha = 45)
#' d <- d_isotoxal_2ngon(x = 5, y = 5, r = 4, s = s, n = 2)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "cyan", stroke = "black", stroke_width = 4)
#' }
#' # `d_star()` is an alias for `d_isotoxal_2ngon()`
#' # Inner exterior angle of |8/3| star is 90 degrees
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 8, beta_ext = 90)
#' d <- d_star(x = 5, y = 5, r = 4, s = s, n = 8, a = 22.5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "yellow", stroke = "black", stroke_width = 4)
#' }
#'
#' # Degenerate case of inner vertex with exterior angle
#' # of 180 degrees creates a regular `n`-gon.
#' s <- affiner::isotoxal_2ngon_inner_radius(n = 8, beta_ext = 180)
#' d <- d_star(x = 5, y = 5, r = 4, s = s, n = 8, a = 22.5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
d_isotoxal_2ngon <- function(x, y, r, s, n, a = 90, ...,
                           offset = 0,
                           origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                           height = getOption("dee.height")) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(origin_at_bottom)) {
        y <- height - y
        return (d_isotoxal_2ngon(x, y, r, s, n, a, ...,
                               offset = offset, origin_at_bottom = FALSE))
    } 
    r <- as.numeric(r)
    s <- as.numeric(s)
    stopifnot(all(s < r))
    a <- -as.numeric(degrees(a), "degrees")
    MoreArgs <- list(...)
    MoreArgs$origin_at_bottom <- FALSE
    .mapply(d_isotoxal_2ngon_helper,
            list(x = x, y = y, r = r, s = s, n = n, a = a, offset = offset),
            MoreArgs) |>
        Reduce(`+.dee`, x = _)
}

#' @rdname d_isotoxal_2ngon
#' @export
d_star <- d_isotoxal_2ngon

d_isotoxal_2ngon_helper <- function(x, y, r, s, n, a, offset, ...) {

    xyo <- as_coord2d(degrees(seq(a, by = 360 / n, length.out = n)),
                      radius = r)$
        translate(x = x, y = y)
    xyi <- as_coord2d(degrees(seq(a + 360 / n / 2, by = 360 / n, length.out = n)),
                      radius = s * r)$
        translate(x = x, y = y)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_polygon(x, y, ..., offset = offset)
}

#' Regular `n`-gon path convenience wrapper
#'
#' `d_regular_ngon()` is a wrapper around `d_polygon()`
#' to create regular polygon shaped paths.
#' Its vectorized in its `x`, `y`, `r`, `n`, `a`, and `offset` arguments.
#' @inheritParams d_polygon
#' @inheritParams M
#' @param r The radius of the regular polygon (where the vertices lay on).
#' @param n The number of vertices.
#' @param a The angle of the first vertex (will be coerced by [affiner::degrees()]).
#' @param ... Passed to [d_polygon()].
#' @seealso [d_polygon()] and [d_isotoxal_2ngon()].
#' @return A [dee()] object.
#' @examples
#' # A pentagon
#' d5 <- d_regular_ngon(x = 5, y = 5, r = 4, n = 5)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d5, height = 10, width = 10,
#'        fill = "magenta", stroke = "black", stroke_width = 4)
#' }
#' # A hexagon
#' d6 <- d_regular_ngon(x = 5, y = 5, r = 4, n = 6, offset = c(0, -1))
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d6, height = 10, width = 10,
#'        attrs = list(fill_rule = "evenodd"),
#'        fill = "magenta", stroke = "black", stroke_width = 4)
#' }
#' @export
d_regular_ngon <- function(x, y, r, n, a = 90, ...,
                         offset = 0,
                         origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
                         height = getOption("dee.height")) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(origin_at_bottom)) {
        y <- height - y
        return (d_regular_ngon(x, y, r, n, a, ...,
                             offset = offset, origin_at_bottom = FALSE))
    } 
    r <- as.numeric(r)
    a <- -as.numeric(degrees(a), "degrees")
    MoreArgs <- list(...)
    MoreArgs$origin_at_bottom <- FALSE
    .mapply(d_regular_ngon_helper,
            list(x = x, y = y, r = r, n = n, a = a, offset = offset),
            MoreArgs) |>
        Reduce(`+.dee`, x = _)
}

d_regular_ngon_helper <- function(x, y, r, n, a, offset, ...) {

    xy <- as_coord2d(degrees(seq(a, by = 360 / n, length.out = n)),
                     radius = r)$
        translate(x = x, y = y)
    d_polygon(xy$x, xy$y, ..., offset = offset)
}
