#' Build an object of class "dee"
#'
#' `dee()` casts a string to an object of class "dee".
#'
#' @param x A character vector.
#' @return An object of class "dee".
#' @examples
#' d <- dee("M 10,30") + dee("V 30")
#' c(d, d)
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

#' Convert to an `omsvg` "svg" class object
#'
#' `as_omsvg()` converts a `dee` object to an `omsvg` "svg" class object
#' using [omsvg::SVG()] and [omsvg::svg_path()].
#'
#' @param x An object of class "x".
#' @param ... Passed to [omsvg::svg_path()].
#' @param height,width svg width and height.
#' @param background_color Background color.
#' @param stroke,stroke_width,fill,attrs Passed to [omsvg::svg_path()].
#' @examples
#' s <- affiner::star_inner_radius(n = 5, d = 2)
#' d <- d_star(x = 50, y = 50, r = 30, s = s, n = 5, digits = 0)
#' if (requireNamespace("omsvg", quietly = TRUE)) {
#'   svg <- as_omsvg(d, height = 100, width = 100,
#'                 fill = "fill", stroke = "none")
#'   paste(svg, collapse = "\n") |> cat()
#' }
#' @export
as_omsvg <- function(
	x,
	...,
	height = getOption("dee.height"),
	width = getOption("dee.width"),
	background_color = getOption("dee.background_color", "none"),
	stroke = getOption("dee.stroke"),
	stroke_width = getOption("dee.stroke_width"),
	fill = getOption("dee.fill"),
	attrs = getOption("dee.attrs")
) {
	stopifnot(
		is.numeric(height),
		is.numeric(width)
	)
	rlang::check_installed("omsvg")
	bg <- MZ(x = c(0, 0, width, width), y = c(0, height, height, 0))
	svg <- omsvg::SVG(width = width, height = height, viewbox = TRUE) |>
		omsvg::svg_path(bg, fill = background_color, stroke = "none")
	n <- length(x)
	stroke <- rep_len_null(stroke, n)
	stroke_width <- rep_len_null(stroke_width, n)
	fill <- rep_len_null(fill, n)
	for (i in seq.int(n)) {
		d <- as.character(x[[i]])
		svg <- svg |>
			omsvg::svg_path(
				d,
				stroke = stroke[[i]],
				stroke_width = stroke_width[[i]],
				fill = fill[[i]],
				attrs = attrs,
				...
			)
	}
	svg
}


#' Plot an object of class "dee"
#'
#' `plot()` an object of class "dee" using [omsvg::SVG()], [omsvg::svg_path()], and `svgparser::read_svg()`.
#'
#' This function requires the package `svgparser` which (as of January 2026) is not available on CRAN.  You can install it with `remotes::install_github('coolbutuseless/svgparser')` or `utils::install.packages('svgparser', repos = c('https://trevorld.r-universe.dev', 'https://cloud.r-project.org'))`.
#'
#' @inheritParams as_omsvg
#' @return `invisible(NULL)`
# # https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#example
#' @examples
#' d <- M(10, 30) +
#'      A(20, 20, 0, 0, 1, 50, 30) +
#'      A(20, 20, 0, 0, 1, 90, 30) +
#'      Q(90, 60, 50, 90) +
#'      Q(10, 60, 10, 30) +
#'      Z()
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 100, width = 100, background_color = "cyan",
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
plot.dee <- function(
	x,
	...,
	height = getOption("dee.height"),
	width = getOption("dee.width"),
	background_color = getOption("dee.background_color", "none"),
	stroke = getOption("dee.stroke"),
	stroke_width = getOption("dee.stroke_width"),
	fill = getOption("dee.fill"),
	attrs = getOption("dee.attrs")
) {
	rlang::check_installed("svgparser", action = function(...) {
		utils::install.packages(
			'svgparser',
			repos = c('https://trevorld.r-universe.dev', 'https://cloud.r-project.org')
		)
	})
	svg <- as_omsvg(
		x,
		...,
		height = height,
		width = width,
		background_color = background_color,
		stroke = stroke,
		stroke_width = stroke_width,
		fill = fill,
		attrs = attrs
	)
	g <- as.character(svg) |>
		paste(collapse = "\n") |>
		svgparser::read_svg()
	grid::grid.newpage()
	grid::grid.draw(g)
	invisible(NULL)
}

rep_len_null <- function(x, length.out) {
	if (is.null(x)) {
		vector("list", length.out)
	} else {
		rep_len(x, length.out)
	}
}
