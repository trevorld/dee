dee 0.1.0 (development)
=======================

Initial features
----------------

* `dee()` creates "dee" class objects.

  + Can be plotted with `plot.dee()` if suggested packages `{omsvg}` and `{svgparser}` are installed.

* `dee_options()` returns a list of (current or default) `{dee}` package option values.
* Absolute coordinate path command helpers:

  + `A()`, `AZ()` are the elliptical arc curve commands.
  + `C()`, `CZ()`, `S()`, `SZ()` are the cubic Bézier curve commands.
  + `L()`, `LZ()`, `H()`, `HZ()`, `V()`, `VZ()` are the "lineto" commands.
  + `M()`, `MZ()` are the "moveto" commands.
  + `Q()`, `QZ()`, `T()`, `TZ()` are the quadratic Bézier curve commands.
  + `Z()` is the "closepath" command.

* Relative coordinate path command helpers:

  + `aa()`, `az()` are the elliptical arc curve commands.
  + `cc()`, `cz()`, `ss()`, `sz()` are the cubic Bézier curve commands.
  + `ll()`, `lz()`, `hh()`, `hz()`, `vv()`, `vz()` are the "lineto" commands.
  + `mm()`, `mz()` are the "moveto" commands.
  + `qq()`, `qz()`, `tt()`, `tz()` are the quadratic Bézier curve commands.
  + `zz()` is the "closepath" command.

* Convenience wrappers:

  + `FSLASH()` and `BSLASH()` are wrappers around `MZ()` to build
    forward/backward slash paths.
  + `ELLIPSE()` is a wrapper around `M()` and `AZ()` to build ellipse paths.

    + `CIRCLE()` is a wrapper around `ELLIPSE()` to build circle paths.

  + `POLYGON()` is a wrapper around `MZ()` to build polygon paths.
    If its argument `offset` is nonzero will compute an offset region.

    + `ISOTOXAL_2NGON()` is a wrapper around `POLYGON()` to create isotoxal `2n`-gon polygons (e.g. stars).
    + `REGULAR_NGON()` is a wrapper around `POLYGON()` to create regular polygons.

  + `RECT()` is a wrapper around `MZ()` to build rectangle paths.
