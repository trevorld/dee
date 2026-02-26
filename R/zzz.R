#' @importFrom affiner as_coord2d degrees
#' @importFrom rlang check_dots_empty
NULL

as_coords <- function(x, y = NULL) {
	if (is.null(y)) {
		p <- as_coord2d(x)
	} else {
		stopifnot(is.numeric(x), is.numeric(y))
		p <- data.frame(x = x, y = y)
	}
	p
}
