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
