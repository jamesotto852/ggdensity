# Rug plots of highest density region estimates of univariate pdfs

Compute and plot the highest density regions (HDRs) of specified
univariate pdf(s). Note, the plotted objects have probabilities mapped
to the `alpha` aesthetic by default.

## Usage

``` r
stat_hdr_rug_fun(
  mapping = NULL,
  data = NULL,
  geom = "hdr_rug_fun",
  position = "identity",
  ...,
  fun_x = NULL,
  fun_y = NULL,
  args_x = list(),
  args_y = list(),
  probs = c(0.99, 0.95, 0.8, 0.5),
  xlim = NULL,
  ylim = NULL,
  n = 512,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_hdr_rug_fun(
  mapping = NULL,
  data = NULL,
  stat = "hdr_rug_fun",
  position = "identity",
  ...,
  outside = FALSE,
  sides = "bl",
  length = unit(0.03, "npc"),
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

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- fun_x, fun_y:

  Functions, the univariate probability density function for the x-
  and/or y-axis. First argument must be vectorized.

- args_x, args_y:

  Named list of additional arguments passed on to `fun_x` and/or
  `fun_y`.

- probs:

  Probabilities to compute highest density regions for.

- xlim, ylim:

  Range to compute and draw regions. If `NULL`, defaults to range of
  data.

- n:

  Resolution of grid defined by `xlim` and `ylim`. Ignored if
  `method = "histogram"` or `method = "freqpoly"`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- outside:

  logical that controls whether to move the rug tassels outside of the
  plot area. Default is off (FALSE). You will also need to use
  `coord_cartesian(clip = "off")`. When set to TRUE, also consider
  changing the sides argument to "tr". See examples.

- sides:

  A string that controls which sides of the plot the rugs appear on. It
  can be set to a string containing any of `"trbl"`, for top, right,
  bottom, and left.

- length:

  A [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object that sets
  the length of the rug lines. Use scale expansion to avoid overplotting
  of data.

## Aesthetics

`geom_hdr_rug_fun()` understands the following aesthetics (required
aesthetics are in bold):

- x

- y

- alpha

- fill

- group

- subgroup

## Computed variables

- probs:

  The probability of the highest density region, specified by `probs`,
  corresponding to each point.

## Examples

``` r
# Plotting data with exponential marginals
df <- data.frame(x = rexp(1e3), y = rexp(1e3))

ggplot(df, aes(x, y)) +
  geom_hdr_rug_fun(fun_x = dexp, fun_y = dexp) +
  geom_point(size = .5) +
  coord_fixed()


# without data/aesthetic mappings
ggplot() +
  geom_hdr_rug_fun(fun_x = dexp, fun_y = dexp, xlim = c(0, 7), ylim = c(0, 7)) +
  coord_fixed()



# Plotting univariate normal data, estimating mean and sd
df <- data.frame(x = rnorm(1e4, mean = 1, sd = 3))

# estimating parameters
mu_hat <- mean(df$x)
sd_hat <- sd(df$x)

ggplot(df, aes(x)) +
  geom_hdr_rug_fun(fun_x = dnorm, args_x = list(mean = mu_hat, sd = sd_hat)) +
  geom_density()


# Equivalent to `method_norm_1d()` with `geom_hdr_rug()`
ggplot(df, aes(x)) +
  geom_hdr_rug(method = method_norm_1d()) +
  geom_density()
```
