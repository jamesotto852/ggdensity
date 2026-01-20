# Highest density regions of a bivariate pdf

Compute and plot the highest density regions (HDRs) of a bivariate pdf.
`geom_hdr_fun()` draws filled regions, and `geom_hdr_lines_fun()` draws
lines outlining the regions. Note, the plotted objects have
probabilities mapped to the `alpha` aesthetic by default.

## Usage

``` r
stat_hdr_fun(
  mapping = NULL,
  data = NULL,
  geom = "hdr_fun",
  position = "identity",
  ...,
  fun,
  args = list(),
  probs = c(0.99, 0.95, 0.8, 0.5),
  xlim = NULL,
  ylim = NULL,
  n = 100,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_hdr_fun(
  mapping = NULL,
  data = NULL,
  stat = "hdr_fun",
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

- fun:

  A function, the joint probability density function, must be vectorized
  in its first two arguments; see examples.

- args:

  Named list of additional arguments passed on to `fun`.

- probs:

  Probabilities to compute highest density regions for.

- xlim, ylim:

  Range to compute and draw regions. If `NULL`, defaults to range of
  data if present.

- n:

  Resolution of grid `fun` is evaluated on.

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

`geom_hdr_fun()` and `geom_hdr_lines_fun()` understand the following
aesthetics (required aesthetics are in bold):

- x

- y

- alpha

- color

- fill (only `geom_hdr_fun`)

- group

- linetype

- linewidth

- subgroup

## Computed variables

- probs:

  The probability associated with the highest density region, specified
  by `probs`.

## Examples

``` r
# HDRs of the bivariate exponential
f <- function(x, y) dexp(x) * dexp(y)
ggplot() + geom_hdr_fun(fun = f, xlim = c(0, 10), ylim = c(0, 10))



# HDRs of a custom parametric model

# generate example data
n <- 1000
th_true <- c(3, 8)

rdata <- function(n, th) {
  gen_single_obs <- function(th) {
    rchisq(2, df = th) # can be anything
  }
  df <- replicate(n, gen_single_obs(th))
  setNames(as.data.frame(t(df)), c("x", "y"))
}
data <- rdata(n, th_true)

# estimate unknown parameters via maximum likelihood
likelihood <- function(th) {
  th <- abs(th) # hack to enforce parameter space boundary
  log_f <- function(v) {
    x <- v[1]; y <- v[2]
    dchisq(x, df = th[1], log = TRUE) + dchisq(y, df = th[2], log = TRUE)
  }
  sum(apply(data, 1, log_f))
}
(th_hat <- optim(c(1, 1), likelihood, control = list(fnscale = -1))$par)
#> [1] 3.039954 8.187069

# plot f for the give model
f <- function(x, y, th) dchisq(x, df = th[1]) * dchisq(y, df = th[2])

ggplot(data, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(size = .25, color = "red") +
  xlim(0, 30) + ylim(c(0, 30))


ggplot(data, aes(x, y)) +
  geom_hdr_lines_fun(fun = f, args = list(th = th_hat)) +
  geom_point(size = .25, color = "red") +
  xlim(0, 30) + ylim(c(0, 30))


```
