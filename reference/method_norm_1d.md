# Univariate parametric normal HDR estimator

Function used to specify univariate normal density estimator for
[`get_hdr_1d()`](https://jamesotto852.github.io/ggdensity/reference/get_hdr_1d.md)
and layer functions (e.g.
[`geom_hdr_rug()`](https://jamesotto852.github.io/ggdensity/reference/geom_hdr_rug.md)).

## Usage

``` r
method_norm_1d()
```

## Details

For more details on the use and implementation of the `method_*_1d()`
functions, see
[`vignette("method", "ggdensity")`](https://jamesotto852.github.io/ggdensity/articles/method.md).

## Examples

``` r
# Normal estimators are useful when an assumption of normality is appropriate
df <- data.frame(x = rnorm(1e3))

ggplot(df, aes(x)) +
  geom_hdr_rug(method = method_norm_1d()) +
  geom_density()


# Can also be used with `get_hdr_1d()` for numerical summary of HDRs
res <- get_hdr_1d(df$x, method = method_norm_1d())
str(res)
#> List of 3
#>  $ df_est:'data.frame':  512 obs. of  4 variables:
#>   ..$ x               : num [1:512] -3.54 -3.53 -3.51 -3.5 -3.49 ...
#>   ..$ fhat            : num [1:512] 0.00101 0.00105 0.0011 0.00114 0.00119 ...
#>   ..$ fhat_discretized: num [1:512] 1.27e-05 1.32e-05 1.38e-05 1.44e-05 1.50e-05 ...
#>   ..$ hdr             : num [1:512] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ breaks: Named num [1:5] 0.0179 0.0597 0.1718 0.3092 Inf
#>   ..- attr(*, "names")= chr [1:5] "99%" "95%" "80%" "50%" ...
#>  $ data  :'data.frame':  1000 obs. of  2 variables:
#>   ..$ x             : num [1:1000] -1.9223 1.6197 0.5193 -0.0558 0.6964 ...
#>   ..$ hdr_membership: num [1:1000] 0.95 0.95 0.5 0.5 0.5 0.5 0.95 0.99 0.5 0.5 ...
```
