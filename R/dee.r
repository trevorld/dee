#' Build an object of class "dee"
#'
#' `dee()` casts a string to an object of class "dee".
#'
#' @param x A character vector of length 1.
#' @return An object of class "dee".
#' @examples dee("M 10,30") + dee("V 30")
#' @export
dee <- function(x) {
    x <- as.character(x)[[1L]]
    class(x) <- c("dee", "character")
    x
}

#' @export
`+.dee` <- function(e1, e2) {
    dee(paste(e1, e2))
}

#' @export
format.dee <- function(x, ...) {
    x <- as.character(x)
    x <- gsub("([Z|z] )", "\\1\n", x)
    x
}

#' @export
print.dee <- function(x, ...) {
    check_dots_empty()
    x <- format.dee(x, ...)
    cat(x, "\n", sep = "")
    invisible(NULL)
}
