# Changelog

## ggdensity 1.0.1

### Fixes

- Package startup message no longer effects the sessions RNG (Reported
  by [@TimTaylor](https://github.com/TimTaylor)
  [\#34](https://github.com/jamesotto852/ggdensity/issues/34))

- Fixed ordering of probabilities in the plot legend to be independent
  of order specified in `probs` argument (Reported by
  [@z3tt](https://github.com/z3tt)
  [\#32](https://github.com/jamesotto852/ggdensity/issues/32))

## ggdensity 1.0.0

CRAN release: 2023-02-09

### Features

- Added
  [`get_hdr()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
  and
  [`get_hdr_1d()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr_1d.md)
  functions, exporting implementation of HDR computations (Suggested by
  [@eliocamp](https://github.com/eliocamp)
  [\#28](https://github.com/jamesotto852/ggdensity/issues/28))

- Reworked `method` argument, allowing for either character or function
  call specification. Implemented related `method_*()` and
  `method_*_1d()` functions
  (e.g. [`method_kde()`](https://jamesotto852.github.io/ggdensity/reference/method_kde.md)
  and
  [`method_kde_1d()`](https://jamesotto852.github.io/ggdensity/reference/method_kde_1d.md)).
  See
  [`?get_hdr`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
  or
  [`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md)
  for details (Suggested by [@eliocamp](https://github.com/eliocamp)
  [\#29](https://github.com/jamesotto852/ggdensity/issues/29))

- Added unit tests (Suggested by
  [@eliocamp](https://github.com/eliocamp),
  [\#30](https://github.com/jamesotto852/ggdensity/issues/30))

### Breaking Changes

- Removed arguments governing density estimators from
  [`stat_hdr()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr.md)
  and other layer functions–these are now specified with `method_*()`
  and `method_*_1d()` functions

### Fixes

- [Added
  support](https://tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/)
  for the new `linewidth` aesthetic (Reported by
  [@eliocamp](https://github.com/eliocamp),
  [\#23](https://github.com/jamesotto852/ggdensity/issues/23))

## ggdensity 0.1.1

CRAN release: 2022-10-24

### Fixes

- Removed **ggplot2** build-time dependencies (Reported by
  [@thomasp85](https://github.com/thomasp85),
  [\#21](https://github.com/jamesotto852/ggdensity/issues/21))

- Fixed bug in
  [`stat_hdr_lines_fun()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr_fun.md)
  which drew lines between components of disconnected HDRs (Reported by
  [@afranks86](https://github.com/afranks86),
  [\#20](https://github.com/jamesotto852/ggdensity/issues/20))

## ggdensity 0.1.0

CRAN release: 2022-07-20

### Features

- Added
  `geom`/[`stat_hdr_rug()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr_rug.md)
  for visualizing marginal HDRs via “rug plot” style graphics along plot
  axes ([\#14](https://github.com/jamesotto852/ggdensity/issues/14))

- Added
  `geom`/[`stat_hdr_points()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr_points.md)
  and
  `geom`/[`stat_hdr_points_fun()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr_points_fun.md)
  for visualizing HDR membership of points via a colored scatterplot
  ([\#15](https://github.com/jamesotto852/ggdensity/issues/15))

### Fixes

- Changed name of computed variable in all stat functions from `level`
  to `probs`
