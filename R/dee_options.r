default_options <- list(dee.sep = ",",
                        dee.origin_at_bottom = FALSE,
                        dee.height = NULL)

#' Get dee options
#'
#' `dee_options()` returns the `dee` package's global options.
#'
#' @param ... `dee` package options using `name = value`.
#'            The return list will use any of these instead of the current/default values.
#' @param default If `TRUE` return the default values instead of current values.
#' @return A list of option values.
#'         Note this function **does not** set option values itself but
#'         this list can be passed to [options()], [withr::local_options()], or [withr::with_options()].
#' @examples
#'   dee_options()
#'
#'   dee_options(default = TRUE)
#'
#'   dee_options(dee.height = 100, dee.width = 100, dee.origin_at_bottom = TRUE)
#' @seealso [dee-package] for a high-level description of relevant global options.
#' @export
dee_options <- function (..., default = FALSE) {
    dee_op <- list(dee.attrs = NULL,
                   dee.background_color = NULL,
                   dee.digits = Inf,
                   dee.fill = NULL,
                   dee.height = NULL,
                   dee.origin_at_bottom = FALSE,
                   dee.sep = ",",
                   dee.stroke = NULL,
                   dee.stroke_width = NULL,
                   dee.width = NULL)
    l <- list(...)
    stopifnot(all(names(l) %in% names(dee_op)))
    if (isFALSE(default)) {
        for (n in names(dee_op)) {
            dee_op[n] <- list(getOption(n, dee_op[[n]]))
        }
    }
    if (length(names(l))) {
        for (n in names(l)) {
            dee_op[n] <- l[n]
        }
    }
    dee_op
}
