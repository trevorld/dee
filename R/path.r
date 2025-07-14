#' The "closepath" commands
#'
#' `Z()` and `zz()` connects the initial point of the current subpath with the last point (with a straight line if these are different).  There is no difference between `Z()` and `zz()`.
#'
#' @return A [dee()] object.
#' @examples
#' M(1, 1) + L(2, 2) + Z()
#' M(1, 1) + ll(2, 2) + zz()
#' @export
Z <- function() dee("Z")

#' @rdname Z
#' @export
zz <- function() dee("z")

#' The "moveto" commands
#'
#' `M()` and `mm()` move the "pen" to a new point.
# `MZ()` and `mz()` are variants that automatically add a "closepath".
#' @param x If `y` is `NULL` will be coerced by [affiner::as_coord2d()].
#'          Else a numeric vector.
#' @param y Either `NULL` or a numeric vector.
#' @param sep Either `","` or `" "`.
#' @param origin_at_bottom,height If `origin_at_bottom` is `TRUE` then
#'                                `y` (and any `y1` and `y2`) coordinates is transformed by `height - y`.
#' @return A [dee()] object.
#' @examples
#' M(1, 1) + L(2, 2) + Z()
#' M(1, 1) + ll(1, 1) + zz()
#' @export
M <- function(x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("M", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname M
#' @export
mm <- function(x, y = NULL, ...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("m", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname M
#' @param ... Passed to related function.
#' @export
MZ <- function(...) {
    M(...) + Z()
}

#' @rdname M
#' @export
mz <- function(...) {
    mm(...) + zz()
}

#' The "lineto" commands
#'
#' `L()` and `ll()` draw straight lines,
#' `H()` and `hh()` draw horizontal lines, and
#' `V()` and `vv()` draw vertical lines.
# `LZ()`, `lz()`, `HZ()`, `hz()`, `VZ()`, and `vz()` are variants that automatically add a "closepath".
#'
#' @inheritParams M
#' @examples
#' M(1, 1) + L(2, 2) + Z()
#' M(1, 1) + ll(1, 1) + zz()
#' @return A [dee()] object.
#' @export
L <- function(x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("L", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname L
#' @export
ll <- function(x, y = NULL, ...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("l", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname L
#' @export
LZ <- function(...) {
    L(...) + Z()
}

#' @rdname L
#' @export
lz <- function(...) {
    ll(...) + zz()
}

#' @rdname L
#' @export
H <- function(x) {
    stopifnot(is.numeric(x))
    paste(c("H", x), collapse = " ") |> dee()
}

#' @rdname L
#' @export
hh <- function(x) {
    stopifnot(is.numeric(x))
    paste(c("h", x), collapse = " ") |> dee()
}

#' @rdname L
#' @export
HZ <- function(...) {
    H(...) + Z()
}

#' @rdname L
#' @export
hz <- function(...) {
    hh(...) + zz()
}

#' @rdname L
#' @export
V <- function(y, ...,
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    stopifnot(is.numeric(y))
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("V", y), collapse = " ") |> dee()
}

#' @rdname L
#' @export
vv <- function(y, ...,
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    stopifnot(is.numeric(y))
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("v", y), collapse = " ") |> dee()
}

#' @rdname L
#' @export
VZ <- function(...) {
    V(...) + Z()
}

#' @rdname L
#' @export
vz <- function(...) {
    vv(...) + zz()
}

#' The quadratic Bezier curve commands
#'
#' `Q()` and `qq()` draw quadratic Bezier curves
#' `T()` and `tt()` draw quadratic Bezier curves assuming the control point is the reflection of the previous Bezier curve command.
# `QZ()`, `qz()`, `TZ()`, and `tz()` are variants that automatically add a "closepath".
#'
#' @inheritParams M
#' @param x1 If `y1` is `NULL` will be coerced by [affiner::as_coord2d()].
#'          Else a numeric vector.
#' @param y1 Either `NULL` or a numeric vector.
#' @return A [dee()] object.
#' @examples
#' M(1, 1) + Q(2, 2, 3, 3) + T(4, 4) + Z()
#' M(1, 1) + qq(1, 1, 2, 2) + tt(1, 1) + zz()
#' @export
Q <- function(x1, y1 = NULL, x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p1 <- as_coords(x1, y1)
    p <- as_coords(x, y)
    x1 <- p1$x
    y1 <- p1$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y1 <- getOption("dee.height") - y1
        y <- getOption("dee.height") - y
    }
    xy1 <- paste(x1, y1, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("Q", paste(xy1, xy)), collapse = " ") |> dee()
}

#' @rdname Q
#' @export
qq <- function(x1, y1 = NULL, x, y = NULL, ...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p1 <- as_coords(x1, y1)
    p <- as_coords(x, y)
    x1 <- p1$x
    y1 <- p1$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y1 <- getOption("dee.height") - y1
        y <- getOption("dee.height") - y
    }
    xy1 <- paste(x1, y1, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("q", paste(xy1, xy)), collapse = " ") |> dee()
}

#' @rdname Q
#' @export
QZ <- function(...) {
    Q(...) + Z()
}

#' @rdname Q
#' @export
qz <- function(...) {
    qq(...) + zz()
}

#' @rdname Q
#' @export
T <- function(x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("T", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname Q
#' @export
tt <- function(x, y = NULL, ...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    paste(c("t", paste(x, y, sep = sep)), collapse = " ") |> dee()
}

#' @rdname Q
#' @export
TZ <- function(...) {
    T(...) + Z()
}

#' @rdname Q
#' @export
tz <- function(...) {
    tt(...) + zz()
}

#' The cubic Bezier curve commands
#'
#' `C()` and `cc()` draw cubic Bezier curves
#' `S()` and `ss()` draw cubic Bezier curves assuming the first control point is the reflection of the previous Bezier curve command.
# `CZ()`, `cz()`, `SZ()`, and `sz()` are variants that automatically add a "closepath".
#'
#' @inheritParams Q
#' @param x2 If `y2` is `NULL` will be coerced by [affiner::as_coord2d()].
#'          Else a numeric vector.
#' @param y2 Either `NULL` or a numeric vector.
#' @examples
#' M(1, 1) + C(2, 2, 3, 3, 4, 4) + Z()
#' M(1, 1) + cc(1, 1, 2, 2, 3, 3) + zz()
#' @return A [dee()] object.
#' @export
C <- function(x1, y1 = NULL, x2, y2 = NULL, x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p1 <- as_coords(x1, y1)
    p2 <- as_coords(x2, y2)
    p <- as_coords(x, y)
    x1 <- p1$x
    y1 <- p1$y
    x2 <- p2$x
    y2 <- p2$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y2 <- getOption("dee.height") - y2
        y1 <- getOption("dee.height") - y1
        y <- getOption("dee.height") - y
    }
    xy1 <- paste(x1, y1, sep = sep)
    xy2 <- paste(x2, y2, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("C", paste(xy1, xy2, xy)), collapse = " ") |> dee()
}

#' @rdname C
#' @export
cc <- function(x1, y1 = NULL, x2, y2 = NULL, x, y = NULL,...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p1 <- as_coords(x1, y1)
    p2 <- as_coords(x2, y2)
    p <- as_coords(x, y)
    x1 <- p1$x
    y1 <- p1$y
    x2 <- p2$x
    y2 <- p2$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y2 <- getOption("dee.height") - y2
        y1 <- getOption("dee.height") - y1
        y <- getOption("dee.height") - y
    }
    xy1 <- paste(x1, y1, sep = sep)
    xy2 <- paste(x2, y2, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("c", paste(xy1, xy2, xy)), collapse = " ") |> dee()
}

#' @rdname C
#' @export
CZ <- function(...) {
    C(...) + Z()
}

#' @rdname C
#' @export
cz <- function(...) {
    cc(...) + zz()
}

#' @rdname C
#' @export
S <- function(x2, y2 = NULL, x, y = NULL,...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p2 <- as_coords(x2, y2)
    p <- as_coords(x, y)
    x2 <- p2$x
    y2 <- p2$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y2 <- getOption("dee.height") - y2
        y <- getOption("dee.height") - y
    }
    xy2 <- paste(x2, y2, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("S", paste(xy2, xy)), collapse = " ") |> dee()
}

#' @rdname C
#' @export
ss <- function(x2, y2 = NULL, x, y = NULL, ...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    p2 <- as_coords(x2, y2)
    p <- as_coords(x, y)
    x2 <- p2$x
    y2 <- p2$y
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y2 <- getOption("dee.height") - y2
        y <- getOption("dee.height") - y
    }
    xy2 <- paste(x2, y2, sep = sep)
    xy <- paste(x, y, sep = sep)
    paste(c("s", paste(xy2, xy)), collapse = " ") |> dee()
}

#' @rdname C
#' @export
SZ <- function(...) {
    S(...) + Z()
}

#' @rdname C
#' @export
sz <- function(...) {
    ss(...) + zz()
}

#' The elliptical arc curve commands
#'
#' `A()` and `aa()` draw elliptical arc curve commands.
# `AZ()` and `az()` are variants that automatically add a "closepath".
#'
#' @inheritParams M
#' @param rx,ry Radius of ellipse.
#' @param x_axis_rotation Angle (in degrees) from x-axis of ellipse.
#' @param large_arc_flag If `TRUE` then one of two larger arc sweeps chosen else
#'                       one of the two smaller arc sweeps.
#' @param sweep_flag If `TRUE` then arc will be drawn in "positive-angle" direction.
#'                   else drawn in "negative-angle" direction.
#' @return A [dee()] object.
#' @examples
#' M(1, 1) + A(rx = 1, x = 2, y = 2) + Z()
#' M(1, 1) + aa(rx = 1, x = 1, y = 1) + zz()
#' @export
A <- function(rx, ry = rx, x_axis_rotation = 0, large_arc_flag = FALSE, sweep_flag = FALSE, x, y = NULL, ...,
              sep = getOption("dee.sep", ","),
              origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
              height = getOption("dee.height", NULL)) {
    check_dots_empty()
    stopifnot(is.numeric(rx), is.numeric(rx))
    rxy <- paste(rx, ry, sep = sep)

    large_arc <- as.integer(as.logical(large_arc_flag))
    sweep <- as.integer(as.logical(sweep_flag))
    rls <- paste(x_axis_rotation, large_arc, sweep, sep = sep)

    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    xy <- paste(x, y, sep = sep)
    paste(c("A", paste(rxy, rls, xy)), collapse = " ") |> dee()
}

#' @rdname A
#' @export
aa <- function(rx, ry = rx, x_axis_rotation = 0, large_arc_flag = FALSE, sweep_flag = FALSE, x, y = NULL,...,
               sep = getOption("dee.sep", ","),
               origin_at_bottom = getOption("dee.origin_at_bottom", FALSE),
               height = getOption("dee.height", NULL)) {
    check_dots_empty()
    stopifnot(is.numeric(rx), is.numeric(rx))
    rxy <- paste(rx, ry, sep = sep)

    large_arc <- as.integer(as.logical(large_arc_flag))
    sweep <- as.integer(as.logical(sweep_flag))
    rls <- paste(x_axis_rotation, large_arc, sweep, sep = sep)

    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    if (isTRUE(getOption("dee.origin_at_bottom"))) {
        y <- getOption("dee.height") - y
    }
    xy <- paste(x, y, sep = sep)
    paste(c("a", paste(rxy, rls, xy)), collapse = " ") |> dee()
}

#' @rdname A
#' @export
AZ <- function(...) {
    A(...) + Z()
}

#' @rdname A
#' @export
az <- function(...) {
    aa(...) + zz()
}
