# Bivariate frequency polygon HDR estimator

Function used to specify bivariate frequency polygon density estimator
for
[`get_hdr()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
and layer functions (e.g.
[`geom_hdr()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr.md)).

## Usage

``` r
method_freqpoly(bins = NULL)
```

## Arguments

- bins:

  Number of bins along each axis. Either a vector of length 2 or a
  scalar value which is recycled for both dimensions. Defaults to normal
  reference rule (Scott, pg 87).

## Details

For more details on the use and implementation of the `method_*()`
functions, see
[`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md).

## References

Scott, David W. Multivariate Density Estimation (2e), Wiley.

## Examples

``` r
set.seed(1)
df <- data.frame(x = rnorm(1e3), y = rnorm(1e3))

ggplot(df, aes(x, y)) +
  geom_hdr(method = method_freqpoly()) +
  geom_point(size = 1)


# The resolution of the frequency polygon estimator can be set via `bins`
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_freqpoly(bins = c(8, 25))) +
  geom_point(size = 1)


# Can also be used with `get_hdr()` for numerical summary of HDRs
res <- get_hdr(df, method = method_freqpoly())
str(res)
#> List of 3
#>  $ df_est:'data.frame':  7056 obs. of  5 variables:
#>   ..$ x               : num [1:7056] -3.32 -3.23 -3.14 -3.05 -2.96 ...
#>   ..$ y               : num [1:7056] -3.57 -3.57 -3.57 -3.57 -3.57 ...
#>   ..$ fhat            : num [1:7056] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ fhat_discretized: num [1:7056] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ hdr             : num [1:7056] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ breaks: Named num [1:5] 0.0021 0.00804 0.02648 0.06925 Inf
#>   ..- attr(*, "names")= chr [1:5] "99%" "95%" "80%" "50%" ...
#>  $ data  :'data.frame':  1000 obs. of  3 variables:
#>   ..$ x             : num [1:1000] -0.626 0.184 -0.836 1.595 0.33 ...
#>   ..$ y             : num [1:1000] 1.135 1.1119 -0.8708 0.2107 0.0694 ...
#>   ..$ hdr_membership: num [1:1000] 0.8 0.5 0.8 0.8 0.5 0.8 0.5 0.95 0.8 0.5 ...
```
