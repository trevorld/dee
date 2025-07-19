#' @section Package options:
#'  The following `dee` option may be set globally via [base::options()]:
#'   \describe{
#'     \item{dee.attrs}{[plot.dee()] passes this to [omsvg::svg_path()]'s `attrs` argument.}
#'     \item{dee.background_color}{Passed to [plot.dee()]'s `background_color` argument.}
#'     \item{dee.digits}{Passed to `digits` argument of svg d path command functions.}
#'     \item{dee.fill}{[plot.dee()] passes this to [omsvg::svg_path()]'s `fill` argument.}
#'     \item{dee.height}{Passed to `height` arguments; the height of svg image (in pixels).}
#'     \item{dee.origin_at_bottom}{Passed to `origin_at_bottom` argument of svg d path command functions.}
#'     \item{dee.sep}{Passed to `sep` argument of svg d path command functions.}
#'     \item{dee.stroke}{[plot.dee()] passes this to [omsvg::svg_path()]'s `stroke` argument.}
#'     \item{dee.stroke_width}{[plot.dee()] passes this to [omsvg::svg_path()]'s `stroke_width` argument.}
#'     \item{dee.width}{Passed to `width` arguments; the width of svg image (in pixels).}
#'   }
#' @keywords internal
"_PACKAGE"
