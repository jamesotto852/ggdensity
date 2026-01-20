# Bivariate histogram HDR estimator

Function used to specify bivariate histogram density estimator for
[`get_hdr()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
and layer functions (e.g.
[`geom_hdr()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr.md)).

## Usage

``` r
method_histogram(bins = NULL, smooth = FALSE, nudgex = "none", nudgey = "none")
```

## Arguments

- bins:

  Number of bins along each axis. Either a vector of length 2 or a
  scalar value which is recycled for both dimensions. Defaults to normal
  reference rule (Scott, pg 87).

- smooth:

  If `TRUE`, HDRs are smoothed by the marching squares algorithm.

- nudgex, nudgey:

  Horizontal and vertical rules for choosing witness points when
  `smooth == TRUE`. Accepts character vector: `"left"`, `"none"`,
  `"right"` (`nudgex`) or `"down"`, `"none"`, `"up"` (`nudgey`).

## Details

For more details on the use and implementation of the `method_*()`
functions, see
[`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md).

## References

Scott, David W. Multivariate Density Estimation (2e), Wiley.

## Examples

``` r
if (FALSE) { # \dontrun{

# Histogram estimators can be useful when data has boundary constraints
set.seed(1)
df <- data.frame(x = rexp(1e3), y = rexp(1e3))

ggplot(df, aes(x, y)) +
  geom_hdr(method = method_histogram()) +
  geom_point(size = 1)

# The resolution of the histogram estimator can be set via `bins`
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_histogram(bins = c(8, 25))) +
  geom_point(size = 1)

# By setting `smooth = TRUE`, we can graphically smooth the "blocky" HDRs
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_histogram(smooth = TRUE)) +
  geom_point(size = 1)

# However, we need to set `nudgex` and `nudgey` to align the HDRs correctly
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_histogram(smooth = TRUE, nudgex = "left", nudgey = "down")) +
  geom_point(size = 1)

# Can also be used with `get_hdr()` for numerical summary of HDRs
res <- get_hdr(df, method = method_histogram())
str(res)
} # }
```
