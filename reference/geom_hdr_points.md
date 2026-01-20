# Scatterplot colored by highest density regions of a 2D density estimate

Perform 2D density estimation, compute the resulting highest density
regions (HDRs), and plot the provided data as a scatterplot with points
colored according to their corresponding HDR.

## Usage

``` r
stat_hdr_points(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  method = "kde",
  probs = c(0.99, 0.95, 0.8, 0.5),
  n = 100,
  xlim = NULL,
  ylim = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_hdr_points(
  mapping = NULL,
  data = NULL,
  stat = "hdr_points",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data, either as a `ggproto`
  `Geom` subclass or as a string naming the geom stripped of the `geom_`
  prefix (e.g. `"point"` rather than `"geom_point"`)

- position:

  Position adjustment, either as a string naming the adjustment (e.g.
  `"jitter"` to use `position_jitter`), or the result of a call to a
  position adjustment function. Use the latter if you need to change the
  settings of the adjustment.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html). These
  are often aesthetics, used to set an aesthetic to a fixed value, like
  `colour = "red"` or `size = 3`. They may also be parameters to the
  paired geom/stat.

- method:

  Density estimator to use, accepts character vector:
  `"kde"`,`"histogram"`, `"freqpoly"`, or `"mvnorm"`. Alternatively
  accepts functions which return closures corresponding to density
  estimates, see
  [`?get_hdr`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
  or
  [`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md).

- probs:

  Probabilities to compute highest density regions for.

- n:

  Number of grid points in each direction.

- xlim, ylim:

  Range to compute and draw regions. If `NULL`, defaults to range of
  data.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- stat:

  The statistical transformation to use on the data for this layer,
  either as a `ggproto` `Geom` subclass or as a string naming the stat
  stripped of the `stat_` prefix (e.g. `"count"` rather than
  `"stat_count"`)

## Aesthetics

geom_hdr_points understands the following aesthetics (required
aesthetics are in bold):

- **x**

- **y**

- alpha

- color

- fill

- group

- linetype

- size

- subgroup

## Computed variables

- probs:

  The probability associated with the highest density region, specified
  by `probs`.

## Examples

``` r
set.seed(1)
df <- data.frame(x = rnorm(500), y = rnorm(500))
p <- ggplot(df, aes(x, y)) +
 coord_equal()

p + geom_hdr_points()


# Setting aes(fill = after_stat(probs)), color = "black", and
# shape = 21 helps alleviate overplotting:
p + geom_hdr_points(aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2)


# Also works well with geom_hdr_lines()
p +
 geom_hdr_lines(
   aes(color = after_stat(probs)), alpha = 1,
   xlim = c(-5, 5), ylim = c(-5, 5)
 ) +
 geom_hdr_points(
   aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2,
   xlim = c(-5, 5), ylim = c(-5, 5)
 )

```
