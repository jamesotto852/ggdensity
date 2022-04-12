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
#'
#' # can plot points colored according to known pdf:
#' f <- function(x, y) dexp(x) * dexp(y)
#' df <- data.frame(x = rexp(1000), y = rexp(1000))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr_points_fun(fun = f, xlim = c(0, 10), ylim = c(0, 10))
#'
#'
#' # also allows for hdrs of a custom parametric model
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
                                geom = "hdr_points_fun", position = "identity",
                                ...,
                                fun, args = list(), normalized = TRUE,
                                probs = c(.99, .95, .8, .5),
                                xlim = NULL, ylim = NULL, res = 100,
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
      normalized = normalized,
      probs = probs,
      res = res,
      xlim = xlim,
      ylim = ylim,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
#' @format NULL
#' @usage NULL
#' @rdname geom_hdr_points_fun
StatHdrPointsFun <- ggproto("StatHdrpointsfun", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(order = after_stat(probs), color = after_stat(probs)),

  compute_group = function(data, scales, na.rm = FALSE,
                           fun, args = list(), normalized = TRUE, probs = c(.99, .95, .8, .5),
                           res = 100, xlim = NULL, ylim = NULL) {

  # Allow for use if data = NULL
  if (is.null(scales$x)) {
    rangex <- xlim %||% c(0, 1)
    rangey <- ylim %||% c(0, 1)
  } else {
    rangex <- xlim %||% scales$x$dimension()
    rangey <- ylim %||% scales$y$dimension()
  }

  probs <- sort(probs, decreasing = TRUE)

  HDR_fun <- fun_iso(fun, args, normalized, probs, res, rangex, rangey, scales, type = NULL, HDR_fun = TRUE)

  data$probs <- HDR_fun(data$x, data$y)
  # Could do this in HDR_fun
  data$probs <- scales::percent_format(accuracy = 1)(data$probs)
  data$probs <- ordered(data$probs, levels = scales::percent_format(accuracy = 1)(c(1, probs)))

  data
  }
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

