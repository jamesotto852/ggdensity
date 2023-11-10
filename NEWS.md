# ggdensity (development version)

* Fixed ordering of probabilities in the plot legend to be independent of order specified in `probs` argument (Reported by @z3tt #32)

# ggdensity 1.0.0

## Features

* Added `get_hdr()` and `get_hdr_1d()` functions, 
exporting implementation of HDR computations (Suggested by @eliocamp #28)

* Reworked `method` argument, allowing for either character or function call specification.
Implemented related `method_*()` and `method_*_1d()` functions (e.g. `method_kde()` and `method_kde_1d()`).
See `?get_hdr` or `vignette("method", "ggdensity")` for details (Suggested by @eliocamp #29)

* Added unit tests (Suggested by @eliocamp, #30)

## Breaking Changes

* Removed arguments governing density estimators from `stat_hdr()` and other layer functions--these
are now specified with `method_*()` and `method_*_1d()` functions

## Fixes

* [Added support](https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/) for the new `linewidth` aesthetic (Reported by @eliocamp, #23)

# ggdensity 0.1.1

## Fixes

* Removed **ggplot2** build-time dependencies (Reported by @thomasp85, #21)

* Fixed bug in `stat_hdr_lines_fun()` which drew lines between components of disconnected HDRs (Reported by @afranks86, #20)


# ggdensity 0.1.0

## Features

* Added `geom`/`stat_hdr_rug()` for visualizing marginal HDRs via "rug plot"
style graphics along plot axes (#14)

* Added `geom`/`stat_hdr_points()` and `geom`/`stat_hdr_points_fun()` for 
visualizing HDR membership of points via a colored scatterplot (#15)

## Fixes

* Changed name of computed variable in all stat functions from `level` to `probs`
