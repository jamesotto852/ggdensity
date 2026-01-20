# Bivariate parametric normal HDR estimator

Function used to specify bivariate normal density estimator for
[`get_hdr()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr.md)
and layer functions (e.g.
[`geom_hdr()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr.md)).

## Usage

``` r
method_mvnorm()
```

## Details

For more details on the use and implementation of the `method_*()`
functions, see
[`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md).

## Examples

``` r
# Normal estimator is useful when an assumption of normality is appropriate
set.seed(1)
df <- data.frame(x = rnorm(1e3), y = rnorm(1e3))

ggplot(df, aes(x, y)) +
  geom_hdr(method = method_mvnorm(), xlim = c(-4, 4), ylim = c(-4, 4)) +
  geom_point(size = 1)


# Can also be used with `get_hdr()` for numerical summary of HDRs
res <- get_hdr(df, method = method_mvnorm())
str(res)
#> List of 3
#>  $ df_est:'data.frame':  10000 obs. of  5 variables:
#>   ..$ x               : num [1:10000] -3.01 -2.94 -2.87 -2.8 -2.73 ...
#>   ..$ y               : num [1:10000] -3.25 -3.25 -3.25 -3.25 -3.25 ...
#>   ..$ fhat            : num [1:10000] 1.87e-05 2.25e-05 2.71e-05 3.25e-05 3.87e-05 ...
#>   ..$ fhat_discretized: num [1:10000] 8.97e-08 1.08e-07 1.30e-07 1.56e-07 1.86e-07 ...
#>   ..$ hdr             : num [1:10000] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ breaks: Named num [1:5] 0.00188 0.00776 0.02996 0.07397 Inf
#>   ..- attr(*, "names")= chr [1:5] "99%" "95%" "80%" "50%" ...
#>  $ data  :'data.frame':  1000 obs. of  3 variables:
#>   ..$ x             : num [1:1000] -0.626 0.184 -0.836 1.595 0.33 ...
#>   ..$ y             : num [1:1000] 1.135 1.1119 -0.8708 0.2107 0.0694 ...
#>   ..$ hdr_membership: num [1:1000] 0.8 0.5 0.5 0.8 0.5 0.8 0.5 0.95 0.8 0.5 ...
```
