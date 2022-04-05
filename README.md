
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdensity <img src="man/figures/logo.png"  align="right"  width="120" style="padding-left:10px;background-color:white;" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jamesotto852/ggdensity/workflows/R-CMD-check/badge.svg)](https://github.com/jamesotto852/ggdensity/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/ggdensity)](https://cran.r-project.org/package=ggdensity)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/ggdensity)](https://cran.r-project.org/package=ggdensity)
<!-- badges: end -->

**ggdensity** extends
[**ggplot2**](https://github.com/tidyverse/ggplot2) providing more
interpretable visualizations of density estimates based on highest
density regions (HDRs). **ggdensity** offers drop-in replacements for
[**ggplot2**](https://github.com/tidyverse/ggplot2) functions:

-   instead of `ggplot2::geom_density_2d_filled()`, use
    `ggdensity::geom_hdr()`;
-   instead of `ggplot2::geom_density_2d()`, use
    `ggdensity::geom_hdr_lines()`.

Also included are the functions `geom_hdr_fun()` and
`geom_hdr_lines_fun()` for plotting HDRs of user-specified bivariate
probability density functions.

## Installation

**ggdensity** is available on CRAN and can be installed with
`install.packages("ggdensity")`. You can also install its development
version from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("jamesotto852/ggdensity")
```

## `geom_density_2d_filled()` vs. `geom_hdr()`

The standard way to visualize the joint distribution of two continuous
variables in **ggplot2** is to use `ggplot2::geom_density_2d()` or
`geom_density_2d_filled()`. Here’s an example:

``` r
library("ggplot2"); theme_set(theme_bw())
library("ggdensity")

df <- data.frame("x" = rnorm(1000), "y" = rnorm(1000))
p <- ggplot(df, aes(x, y)) + coord_equal()
p + geom_density_2d_filled()
```

<img src="man/figures/README-ex0-1.png" width="100%" />

While it’s a nice looking plot, it isn’t immediately clear how we should
understand it. That’s because `geom_density_2d_filled()` generates its
contours as equidistant level sets of the estimated bivariate density,
i.e. taking horizontal slices of the 3d surface at equally-spaced
heights, and projecting the intersections down into the plane. So you
get a general feel of where the density is high, but not much else. To
interpret a contour, you would need to multiply its height by the area
it bounds, which of course is very challenging to do by just looking at
it.

`geom_hdr()` tries to get around this problem by presenting you with
regions of the estimated distribution that are immediately
interpretable:

``` r
p + geom_hdr()
```

<img src="man/figures/README-ex1-1.png" width="100%" />

`probs` here tells us the probability bounded by the corresponding
region, and the regions are computed to be the smallest such regions
that bound that level of probability; these are called highest density
regions or HDRs. By default, the plotted regions show the 50%, 80%, 95%,
and 99% HDRs of the estimated density, but this can be changed with the
`probs` argument to `geom_hdr()`. Notice that your take-away from the
plot made with `geom_density_2d_filled()` is subtlely yet significantly
different than that of the plot made by `geom_hdr()`.

## Visualizing subpopulations and `geom_hdr_lines()`

**ggdensity**’s functions were designed to be seamlessly consistent with
the rest of the **ggplot2** framework. As a consequence, pretty much
everything you would expect to just work does. (Well, we hope! [Let us
know](https://github.com/jamesotto852/ggdensity/issues/new) if that’s
not true.)

For example, because `geom_hdr()` maps probability to the `alpha`
aesthetic, the `fill` and `color` aesthetics are available for mapping
to variables. You can use them to visualize subpopulations in your data.
For example, in the `penguins` data from
[**palmerpenguins**](https://github.com/allisonhorst/palmerpenguins) you
may want to look at how the relationship between bill length and flipper
length changes across different species of penguins. Here’s one way you
could look at that:

``` r
library("palmerpenguins")

ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21)
```

<img src="man/figures/README-ex_penguins-1.png" width="100%" />

<div style="height:40px;">

</div>

Nice, but a bit overplotted. To alleviate overplotting, we can use
`geom_hdr_lines()`:

``` r
ggplot(penguins, aes(flipper_length_mm, bill_length_mm, color = species)) +
  geom_hdr_lines(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(size = 1)
```

<img src="man/figures/README-ex_penguins_lines-1.png" width="100%" />

Or you could facet the plot:

<div style="height:40px;">

</div>

``` r
ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21) +
  facet_wrap(vars(species))
```

<img src="man/figures/README-ex_penguins_facet-1.png" width="100%" />

The main point here is that you should really think of `geom_hdr()` and
`geom_hdr_lines()` as drop-in replacements for functions like
`geom_density_2d_filled()`, `geom_density2d()`, and so on, and you can
expect all of the rest of the **ggplot2** stuff to just work.

## A deeper cut illustrating **ggplot2** integration

The underlying stat used by `geom_hdr()` creates the computed variable
`probs` that can be mapped in the standard way you map computed
variables in **ggplot2**, with `after_stat()`.

For example, `geom_hdr()` and `geom_hdr_lines()` map `probs` to the
`alpha` aesthetic by default. But you can override it like this, just be
sure to override the `alpha` aesthetic by setting `alpha = 1`.

``` r
ggplot(faithful, aes(eruptions, waiting)) +
  geom_hdr(
    aes(fill = after_stat(probs)), 
    alpha = 1, xlim = c(0, 8), ylim = c(30, 110)
  ) +
  scale_fill_viridis_d()
```

<img src="man/figures/README-ex_after_stat-1.png" width="100%" />

``` r
ggplot(faithful, aes(eruptions, waiting)) +
  geom_hdr_lines(
    aes(color = after_stat(probs)), 
    alpha = 1, xlim = c(0, 8), ylim = c(30, 110)
  ) +
  scale_color_viridis_d()
```

<img src="man/figures/README-ex_after_stat-2.png" width="100%" />

## Statistics details

In addition to trying to make the visuals clean and the functions what
you would expect as a **ggplot2** user, we’ve spent considerable effort
in trying to ensure that the graphics you’re getting with **ggdensity**
are statistically rigorous and provide a range of estimation options for
more detailed control.

To that end, you can pass a `method` argument into `geom_hdr()` and
`geom_hdr_lines()` that allows you to specify various nonparametric and
parametric ways to estimate the underlying bivariate distribution, and
we have plans for even more. Each of the estimators below offers
advantages in certain contexts. For example, histogram estimators result
in HDRs that obey constrained supports. Normal estimators can be helpful
in providing simplified visuals that give the viewer a sense of where
the distributions are, potentially at the expense of over-simplifying
and removing important features of how the variables (co-)vary.

<img src="man/figures/README-ex_methods-1.png" width="100%" />

## If you know your PDF

The above discussion has focused around densities that are estimated
from data. But in some instances, you have the distribution in the form
of a function that encodes the [joint
PDF](https://en.wikipedia.org/wiki/Probability_density_function). In
those circumstances, you can use `geom_hdr_fun()` and
`geom_hdr_lines_fun()` to make the analogous plots. These functions
behave similarly to `geom_function()` from
[**ggplot2**](https://github.com/tidyverse/ggplot2), accepting the
argument `fun` specifying the pdf to be summarized. Here’s an example:

``` r
f <- function(x, y) dnorm(x) * dgamma(y, 5, 3)

ggplot() +
  geom_hdr_fun(fun = f, xlim = c(-4, 4), ylim = c(0, 5))
```

<img src="man/figures/README-ex_hdr_fun_1-1.png" width="100%" />

<!-- Discuss un-normalized densities here with example of posteriors -->
<!-- In the context of a Bayesian analysis, `geom_hdr()` creates plots of highest posterior regions. -->
<!-- All we need to do is give `geom_hdr()` a data frame with draws from a posterior, and  -->

### Visualizing custom parametric density estimates with `geom_hdr_fun()`

In addition to all of the methods of density estimation available with
`geom_hdr()`, one of the perks of having `geom_hdr_fun()` is that it
allows you to plot parametric densities that you estimate outside the
**ggdensity** framework. The basic idea is that you fit your
distribution outside **ggdensity** calls with your method of choice, say
maximum likelihood, and then plug the maximum likelihood estimate into
the density formula to obtain a function to plug into `geom_hdr_fun()`.

Here’s an example of how you can do that that assuming that the
underlying data are independent and exponentially distributed with
unknown rates.

``` r
set.seed(123)
th <- c(3, 5)
df <- data.frame("x" = rexp(1000, th[1]), "y" = rexp(1000, th[2]))

# construct the likelihood function
l <- function(th) {
  log_liks <- apply(df, 1, function(xy) {
    dexp(xy[1], rate = th[1], log = TRUE) +
    dexp(xy[2], rate = th[2], log = TRUE)
  })
  sum(log_liks)
}

# compute the mle
(th_hat <- optim(c(2, 2), l, control = list(fnscale = -1))$par)
#> [1] 2.912736 5.032125

# construct the parametric density estimate
f <- function(x, y, th) dexp(x, th[1]) * dexp(y, th[2])

# pass estimated density into geom_hdr_fun()
ggplot(df, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(shape = 21, fill = "lightgreen", alpha = .25) +
  coord_equal()
```

<img src="man/figures/README-ex_hdr_fun_2-1.png" width="100%" />

## Stay tuned!

We have a number of neat new features cooking. Check back soon!
