#' Highest density regions of a bivariate pdf
#'
#' Compute and plot the highest density regions (HDRs) of a bivariate pdf.
#' `geom_hdr_fun()` draws filled regions, and `geom_hdr_lines_fun()` draws
#' lines outlining the regions. Note, the plotted objects have the probs mapped
#' to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: geom_hdr_fun understands the following aesthetics
#'   (required aesthetics are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - linewidth
#'   - subgroup
#'
#'   geom_hdr_fun_lines understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - color
#'   - group
#'   - linetype
#'   - linewidth
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability associated with the highest density region, specified
#'   by `probs`.} }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param fun A function, the joint probability density function, must be
#' vectorized in its first two arguments; see examples.
#' @param args List of additional arguments passed on to the function `fun` as a
#'   named list.
#' @param probs Probabilities to compute highest density regions for.
#' @param n Resolution of grid `fun` is evaluated on.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to
#'   range of data if present.
#' @name geom_hdr_fun
#' @rdname geom_hdr_fun
#'
#' @import ggplot2
#'
#' @examples
#'
#' f <- function(x, y) dexp(x) * dexp(y)
#' ggplot() +
#'   geom_hdr_fun(fun = f, xlim = c(0, 10), ylim = c(0, 10))
#'
#'
#' # the hdr of a custom parametric model
#'
#' # generate example data
#' n <- 1000
#' th_true <- c(3, 8)
#'
#' rdata <- function(n, th) {
#'   gen_single_obs <- function(th) {
#'     rchisq(2, df = th) # can be anything
#'   }
#'   df <- replicate(n, gen_single_obs(th))
#'   setNames(as.data.frame(t(df)), c("x", "y"))
#' }
#' data <- rdata(n, th_true)
#'
#' # estimate unknown parameters via maximum likelihood
#' likelihood <- function(th) {
#'   th <- abs(th) # hack to enforce parameter space boundary
#'   log_f <- function(v) {
#'     x <- v[1]; y <- v[2]
#'     dchisq(x, df = th[1], log = TRUE) + dchisq(y, df = th[2], log = TRUE)
#'   }
#'   sum(apply(data, 1, log_f))
#' }
#' (th_hat <- optim(c(1, 1), likelihood, control = list(fnscale = -1))$par)
#'
#' # plot f for the give model
#' f <- function(x, y, th) dchisq(x, df = th[1]) * dchisq(y, df = th[2])
#'
#' ggplot(data, aes(x, y)) +
#'   geom_hdr_fun(fun = f, args = list(th = th_hat)) +
#'   geom_point(size = .25, color = "red")
#'
#' ggplot(data, aes(x, y)) +
#'   geom_hdr_fun(fun = f, args = list(th = th_hat)) +
#'   geom_point(size = .25, color = "red") +
#'   xlim(0, 40) + ylim(c(0, 40))
#'
#'
#'
NULL






#' @rdname geom_hdr_fun
#' @export
stat_hdr_fun <- function(mapping = NULL, data = NULL,
  geom = "hdr_fun", position = "identity",
  ...,
  fun, args = list(),
  probs = c(.99, .95, .8, .5),
  xlim = NULL, ylim = NULL, n = 100,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrFun,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      args = args,
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr_fun
#' @format NULL
#' @usage NULL
#' @importFrom scales percent
#' @export
StatHdrFun <- ggproto("StatHdrFun", Stat,

  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),

  output = "bands",

  # very similar to StatHdr$compute_group(),
  # only difference are the parameters fun + args (vs. method + parameters)
  compute_group = function(self, data, scales, na.rm = FALSE,
                           fun, args = list(), probs = c(.99, .95, .8, .5),
                           n = 100, xlim = NULL, ylim = NULL) {

  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  # Only calculate HDR membership if we need to
  need_membership <- (self$output == "points")

  res <- get_hdr(method = "fun", data, probs, n, rangex, rangey, HDR_membership = need_membership, fun = fun, args = args)

  res_to_df(res, probs, data$group[1], self$output)

  }
)


#' @rdname geom_hdr_fun
#' @export
geom_hdr_fun <- function(mapping = NULL, data = NULL,
  stat = "hdr_fun", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrFun,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr_fun
#' @format NULL
#' @usage NULL
#' @export
GeomHdrFun <- ggproto("GeomHdrFun", GeomHdr)
