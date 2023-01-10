#' Rug plots of highest density region estimates of univariate pdfs
#'
#' Compute and plot the highest density regions (HDRs) of specified univariate pdf(s).
#' Note, the plotted objects have probabilities mapped to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: `geom_hdr_rug_fun()` understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - fill
#'   - group
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability of the highest density region, specified
#'   by `probs`, corresponding to each point.} }
#'
#' @inheritParams ggplot2::geom_rug
#' @inheritParams stat_hdr_rug
#' @param fun_x,fun_y Functions, the univariate probability density function for the x- and/or y-axis.
#'   First argument must be vectorized.
#' @param args_x,args_y Named list of additional arguments passed on to `fun_x` and/or `fun_y`.
#' @name geom_hdr_rug_fun
#' @rdname geom_hdr_rug_fun
#'
#' @examples
#' # plotting data with exponential marginals
#' df <- data.frame(x = rexp(1e3), y = rexp(1e3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr_rug_fun(fun_x = dexp, fun_y = dexp) +
#'   geom_point(size = .5) +
#'   coord_fixed()
#'
#' # without data/aesthetic mappings
#' ggplot() +
#'   geom_hdr_rug_fun(fun_x = dexp, fun_y = dexp, xlim = c(0, 7), ylim = c(0, 7)) +
#'   coord_fixed()
#'
#'
#' # plotting univariate normal data, estimating mean and sd
#' df <- data.frame(x = rnorm(1e4, mean = 1, sd = 3))
#'
#' # estimating parameters
#' mu_hat <- mean(df$x)
#' sd_hat <- sd(df$x)
#'
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug_fun(fun_x = dnorm, args_x = list(mean = mu_hat, sd = sd_hat)) +
#'   geom_density()
#'
#' # equivalent to `method_norm_1d()` with `geom_hdr_rug()`
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug(method = method_norm_1d()) +
#'   geom_density()
NULL


#' @rdname geom_hdr_rug_fun
#' @export
stat_hdr_rug_fun <- function(mapping = NULL, data = NULL,
  geom = "hdr_rug_fun", position = "identity",
  ...,
  fun_x = NULL, fun_y = NULL,
  args_x = list(), args_y = list(),
  probs = c(.99, .95, .8, .5),
  xlim = NULL, ylim = NULL, n = 512,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrRugFun,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun_x = fun_x,
      fun_y = fun_y,
      args_x = args_x,
      args_y = args_y,
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr_rug_fun
#' @format NULL
#' @usage NULL
#' @export
StatHdrRugFun <- ggproto("StatHdrRugFun", Stat,

  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),
  # if fun_x or fun_y are unspecified data might be dropped
  dropped_aes = c("x", "y"),

  # very similar to StatHdrRug$compute_group(),
  # only difference are the parameters fun + args (vs. method + parameters)
  # -- this prevents factoring into one compute_group() method,
  #    compute_group()'s arguments are different.
  compute_group = function(self, data, scales, na.rm = FALSE,
                           fun_x = NULL, fun_y = NULL, args_x = list(), args_y = list(),
                           probs = c(.99, .95, .8, .5),
                           n = 512, xlim = NULL, ylim = NULL) {


    # Recycle for both x, y
    if (length(n) == 1) n <- rep(n, 2)

    # Estimate marginal densities

    # Initialize dfs for x and y axes,
    # in case only x or y are supplied:
    df_x <- data.frame()
    df_y <- data.frame()


    if (!is.null(fun_x)) {

      if (is.null(xlim) & is.null(scales$x)) {
        stop("`xlim` must be specified if `x` aesthetic not provided to `StatHdrRugFun`")
      }

      rangex <- xlim %||% scales$x$dimension()

      res_x <- get_hdr_1d(data$x, method = "fun", probs, n[1], rangex, hdr_membership = FALSE, fun = fun_x, args = args_x)

      df_x <- res_to_df_1d(res_x, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_x$axis <- "x"
      df_x$y <- NA

    }


    if (!is.null(fun_y)) {

      if (is.null(ylim) & is.null(scales$y)) {
        stop("`ylim` must be specified if `y` aesthetic not provided to `StatHdrRugFun`")
      }

      rangey <- ylim %||% scales$y$dimension()

      res_y <- get_hdr_1d(data$y, method = "fun", probs, n[1], rangey, hdr_membership = FALSE, fun = fun_y, args = args_y)

      df_y <- res_to_df_1d(res_y, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_y$axis <- "y"
      df_y$y <- df_y$x
      df_y$x <- NA

    }

    df <- rbind(df_x, df_y)

    # Need to remove extra col if only plotting x or y rug
    if (is.null(fun_x)) df$x <- NULL
    if (is.null(fun_y)) df$y <- NULL

    df


  }
)



#' @rdname geom_hdr_rug_fun
#' @export
geom_hdr_rug_fun <- function(mapping = NULL, data = NULL,
                         stat = "hdr_rug_fun", position = "identity",
                         ...,
                         outside = FALSE,
                         sides = "bl",
                         length = unit(0.03, "npc"),
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrRugFun,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outside = outside,
      sides = sides,
      length = length,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_hdr_rug_fun
#' @format NULL
#' @usage NULL
#' @export
GeomHdrRugFun <- ggproto("GeomHdrRugFun", GeomHdrRug)





