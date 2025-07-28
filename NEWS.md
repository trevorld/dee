dee 0.1.0 (development)
=======================

Initial features
----------------

* `dee()` creates "dee" class objects.

  + Can be plotted with `plot.dee()` if suggested packages `{omsvg}` and `{svgparser}` are installed.

* `dee_options()` returns a list of (current or default) `{dee}` package option values.
* Absolute coordinate svg d path command helpers:

  + `A()`, `AZ()` are the elliptical arc curve commands.
  + `C()`, `CZ()`, `S()`, `SZ()` are the cubic Bézier curve commands.
  + `L()`, `LZ()`, `H()`, `HZ()`, `V()`, `VZ()` are the "lineto" commands.
  + `M()`, `MZ()` are the "moveto" commands.
  + `Q()`, `QZ()`, `T()`, `TZ()` are the quadratic Bézier curve commands.
  + `Z()` is the "closepath" command.

* Relative coordinate svg d path command helpers:

  + `aa()`, `az()` are the elliptical arc curve commands.
  + `cc()`, `cz()`, `ss()`, `sz()` are the cubic Bézier curve commands.
  + `ll()`, `lz()`, `hh()`, `hz()`, `vv()`, `vz()` are the "lineto" commands.
  + `mm()`, `mz()` are the "moveto" commands.
  + `qq()`, `qz()`, `tt()`, `tz()` are the quadratic Bézier curve commands.
  + `zz()` is the "closepath" command.

* svg d path convenience wrappers:

  + `d_fslash()` and `d_bslash()` are wrappers around `MZ()` to build
    forward/backward slash paths.
  + `d_ellipse()` is a wrapper around `M()` and `AZ()` to build ellipse paths.

    + `d_circle()` is a wrapper around `d_ellipse()` to build circle paths.

  + `d_polygon()` is a wrapper around `MZ()` to build polygon paths.
    If its argument `offset` is nonzero will compute an offset region.

    + `d_isotoxal_2ngon()` is a wrapper around `d_polygon()` to create isotoxal `2n`-gon polygons (e.g. stars).
    + `d_regular_ngon()` is a wrapper around `d_polygon()` to create regular polygons.

  + `d_rect()` is a wrapper around `MZ()` to build rectangle paths.
