#' Scatterplot colored by highest density regions of a bivariate pdf
#'
#' Compute the highest density regions (HDRs) of a bivariate pdf and plot the provided
#' data as a scatterplot with points colored according to their corresponding HDR.
#'
#' @section Aesthetics: geom_hdr_points_fun understands the following aesthetics
#'   (required aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - size
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability associated with the highest density region, specified
#'   by `probs`.} }
#'
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @inheritParams geom_hdr_fun
#'
#' @name geom_hdr_points_fun
#' @rdname geom_hdr_points_fun
#'
#' @import ggplot2
#'
#' @examples
#' # Can plot points colored according to known pdf:
#' set.seed(1)
#' df <- data.frame(x = rexp(1000), y = rexp(1000))
#' f <- function(x, y) dexp(x) * dexp(y)
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr_points_fun(fun = f, xlim = c(0, 10), ylim = c(0, 10))
#'
#'
#' # Also allows for hdrs of a custom parametric model
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
#'   geom_hdr_points_fun(fun = f, args = list(th = th_hat))
#'
#' ggplot(data, aes(x, y)) +
#'   geom_hdr_points_fun(aes(fill = after_stat(probs)), shape = 21, color = "black",
#'     fun = f, args = list(th = th_hat), na.rm = TRUE) +
#'   geom_hdr_lines_fun(aes(color = after_stat(probs)), alpha = 1, fun = f, args = list(th = th_hat)) +
#'   lims(x = c(0, 15), y = c(0, 25))
#'
NULL


#' @export
#' @rdname geom_hdr_points_fun
stat_hdr_points_fun <- function(mapping = NULL, data = NULL,
                                geom = "point", position = "identity",
                                ...,
                                fun, args = list(),
                                probs = c(.99, .95, .8, .5),
                                xlim = NULL, ylim = NULL, n = 100,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrPointsFun,
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


#' @export
#' @format NULL
#' @usage NULL
#' @rdname geom_hdr_points_fun
StatHdrPointsFun <- ggproto("StatHdrPointsFun", StatHdrFun,
  default_aes = aes(order = after_stat(probs), color = after_stat(probs)),
  output = "points"
)

#' @export
#' @rdname geom_hdr_points_fun
geom_hdr_points_fun <- function(mapping = NULL, data = NULL,
                                stat = "hdr_points_fun", position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

