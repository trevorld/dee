# dee

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/dee)](https://cran.r-project.org/package=dee)
[![R-CMD-check](https://github.com/trevorld/dee/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/dee/actions)
[![Coverage Status](https://codecov.io/gh/trevorld/dee/branch/main/graph/badge.svg)](https://app.codecov.io/gh/trevorld/dee)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Related links](#related)



## <a name="overview">Overview</a>

The `{dee}` package provides helper functions to construct a string of the [`d` attribute of svg paths](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d).  Such svg path strings can be used with `omsvg::svg_path()` or in a bespoke svg creation function.

In particular I want to generate some svg images of font glyphs to import into [FontForge](https://fontforge.org/docs/index.html) which doesn't seem to import svg glyphs created by `svglite::svglite()` very well and `omsvg::svg_path()` provides no help in constructing an svg path `d` attribute.

## <a name="installation">Installation</a>

```r
remotes::install_github("trevorld/dee")
```

## <a name="examples">Examples</a>

**Note:** the `<svg>` tags below are not rendered by Github but they are rendered at the [package website](https://trevorldavis.com/R/dee/dev/#examples).


``` r
library("dee", warn.conflicts = FALSE) # masks `stats::C()`
# https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#example
d <- M(10, 30) +
     A(20, 20, 0, 0, 1, 50, 30) +
     A(20, 20, 0, 0, 1, 90, 30) +
     Q(90, 60, 50, 90) +
     Q(10, 60, 10, 30) +
     Z()

print(d)
```

```
## <dee[1]>
## M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 Z
```


``` r
library("omsvg")
SVG(width = 100, height = 100, viewbox = TRUE) |>
    svg_path(d, fill="red", stroke="black", stroke_width=2)
```

<!--html_preserve--><svg width="100" height="100" viewBox="0 0 100 100">
  <path d="M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 Z" stroke="black" stroke-width="2" fill="red"/>
</svg><!--/html_preserve-->

Instead of explicitly indicating each `x` and `y` coordinate one
can also use objects that contain both `x` and `y` coordinates
if they can be coerced by `affiner::as_coord2d()`.


``` r
library("affiner")
library("dee", warn.conflicts = FALSE) # masks `stats::C()`
po <- as_coord2d(x = c(10, 10, 90, 90),
                 y = c(10, 90, 90, 10))
pi <- po$clone()$
    translate(-mean(po))$
    scale(0.5)$
    rotate(45)$
    translate(mean(po)) |>
    round()

d <- MZ(po) + MZ(pi)
print(d)
```

```
## <dee[1]>
## M 10,10 10,90 90,90 90,10 Z 
## M 50,22 22,50 50,78 78,50 Z
```


``` r
library("omsvg")
attrs <- svg_attrs_pres(fill_rule = "evenodd")
SVG(width = 100, height = 100, viewbox = TRUE) |>
    svg_path(d, fill = "cyan", stroke = "black", stroke_width = 2, attrs = attrs)
```

<!--html_preserve--><svg width="100" height="100" viewBox="0 0 100 100">
  <path d="M 10,10 10,90 90,90 90,10 Z M 50,22 22,50 50,78 78,50 Z" stroke="black" stroke-width="2" fill="cyan" fill-rule="evenodd"/>
</svg><!--/html_preserve-->

One can use the `dee.origin_at_bottom` and `dee.height` options if one
prefers to think of the origin being at the bottom left corner (as is typical
with R graphics) instead of the top left corner (as is typical with the svg format).


``` r
library("affiner")
library("dee", warn.conflicts = FALSE) # masks `stats::C()`
p <- as_coord2d(x = c(10, 40, 70),
                y = c(10, 40, 10))
t1 <- MZ(p)
t2 <- rlang::with_options(MZ(p),
                          dee.origin_at_bottom = TRUE,
                          dee.height = 100)
print(t1)
```

```
## <dee[1]>
## M 10,10 40,40 70,10 Z
```

``` r
print(t2)
```

```
## <dee[1]>
## M 10,90 40,60 70,90 Z
```


``` r
library("omsvg")
SVG(width = 100, height = 100, viewbox = TRUE) |>
    svg_path(t1, fill = "cyan", stroke = "black", stroke_width = 2) |>
    svg_path(t2, fill = "orange", stroke = "black", stroke_width = 2)
```

<!--html_preserve--><svg width="100" height="100" viewBox="0 0 100 100">
  <path d="M 10,10 40,40 70,10 Z" stroke="black" stroke-width="2" fill="cyan"/>
  <path d="M 10,90 40,60 70,90 Z" stroke="black" stroke-width="2" fill="orange"/>
</svg><!--/html_preserve-->

We also contain vectorized convenience wrappers for some common shapes:


``` r
# U+2742 circled open center eight pointed black star
s <- 0.6
ro <- 50
ri <- s * ro
d <- d_circle(50, 50, c(ro, ri, ri - 3)) + d_star(50, 50, ro, s, 8, digits = 0)
print(d)
```

```
## <dee[1]>
## M 50,100 A 50,50 0,0,0 50,0 50,50 0,0,0 50,100 Z 
## M 50,80 A 30,30 0,0,0 50,20 30,30 0,0,0 50,80 Z 
## M 50,77 A 27,27 0,0,0 50,23 27,27 0,0,0 50,77 Z 
## M 50,0 61,22 85,15 78,39 100,50 78,61 85,85 61,78 50,100 39,78 15,85 22,61 0,50 22,39 15,15 39,22 Z
```


``` r
library("omsvg")
attrs <- svg_attrs_pres(fill_rule = "evenodd")
SVG(width = 100, height = 100, viewbox = TRUE) |>
    svg_path(d, fill = "red", stroke = "none", attrs = attrs)
```

<!--html_preserve--><svg width="100" height="100" viewBox="0 0 100 100">
  <path d="M 50,100 A 50,50 0,0,0 50,0 50,50 0,0,0 50,100 Z M 50,80 A 30,30 0,0,0 50,20 30,30 0,0,0 50,80 Z M 50,77 A 27,27 0,0,0 50,23 27,27 0,0,0 50,77 Z M 50,0 61,22 85,15 78,39 100,50 78,61 85,85 61,78 50,100 39,78 15,85 22,61 0,50 22,39 15,15 39,22 Z" stroke="none" fill="red" fill-rule="evenodd"/>
</svg><!--/html_preserve-->

## <a name="related">Related links</a>

The `d` attribute of svg paths:

* [mdn web docs](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d)
* [w3.org SVG 1.1 specification](https://www.w3.org/TR/2011/REC-SVG11-20110816/paths.html#PathData)

R packages:

* [omsvg](https://github.com/rich-iannone/omsvg)
* [svglite](https://github.com/r-lib/svglite)
* [svgparser](https://github.com/coolbutuseless/svgparser)
