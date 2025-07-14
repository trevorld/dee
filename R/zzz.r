#' @importFrom rlang check_dots_empty
NULL

as_coords <- function(x, y = NULL) {
    if (is.null(y)) {
        p <- affiner::as_coord2d(x)
    } else {
        stopifnot(is.numeric(x), is.numeric(y))
        p <- vctrs::vec_recycle_common(x = x, y = y)
    }
    p
}

default_options <- list(dee.sep = ",",
                        dee.origin_at_bottom = FALSE,
                        dee.height = NULL)
