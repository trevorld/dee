#' Build an object of class "dee"
#'
#' `dee()` casts a string to an object of class "dee".
#'
#' @param x A character vector.
#' @return An object of class "dee".
#' @examples dee("M 10,30") + dee("V 30")
#' @export
dee <- function(x) {
    x <- as.character(x)
    class(x) <- c("dee", "character")
    x
}

#' @export
`+.dee` <- function(e1, e2) {
    dee(paste(e1, e2))
}

#' @export
c.dee <- function(...) {
    dee(NextMethod())
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
    s <- format.dee(x, ...) |> paste(collapse = "\n")
    header <- paste0("<dee[", length(x), "]>\n")
    cat(header, s, "\n", sep = "")
    invisible(x)
}
