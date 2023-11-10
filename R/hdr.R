#' Highest density regions of a 2D density estimate
#'
#' Perform 2D density estimation, compute and plot the resulting highest density regions.
#' `geom_hdr()` draws filled regions and `geom_hdr_lines()` draws lines outlining the regions.
#' Note, the plotted objects have probabilities mapped to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: `geom_hdr()` and `geom_hdr_lines()` understand the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - alpha
#'   - color
#'   - fill (only `geom_hdr`)
#'   - group
#'   - linetype
#'   - linewidth
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability associated with the highest density region, specified
#'   by `probs` argument.} }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param method Density estimator to use, accepts character vector:
#'   `"kde"`,`"histogram"`, `"freqpoly"`, or `"mvnorm"`.
#'   Alternatively accepts functions  which return closures corresponding to density estimates,
#'   see `?get_hdr` or `vignette("method", "ggdensity")`.
#' @param probs Probabilities to compute highest density regions for.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to
#'   range of data.
#' @param n Resolution of grid defined by `xlim` and `ylim`.
#'   Ignored if `method = "histogram"` or `method = "freqpoly"`.
#' @name geom_hdr
#' @rdname geom_hdr
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @import ggplot2
#'
#' @examples
#' # Basic simulated data with bivariate normal data and various methods
#' df <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' p <- ggplot(df, aes(x, y)) + coord_equal()
#'
#' p + geom_hdr()
#' p + geom_hdr(method = "mvnorm")
#' p + geom_hdr(method = "freqpoly")
#' # p + geom_hdr(method = "histogram")
#'
#' # Adding point layers on top to visually assess region estimates
#' pts <- geom_point(size = .2, color = "red")
#'
#' p + geom_hdr() + pts
#' p + geom_hdr(method = "mvnorm") + pts
#' p + geom_hdr(method = "freqpoly") + pts
#' # p + geom_hdr(method = "histogram") + pts
#'
#' # Highest density region boundary lines
#' p + geom_hdr_lines()
#' p + geom_hdr_lines(method = "mvnorm")
#' p + geom_hdr_lines(method = "freqpoly")
#' # p + geom_hdr_lines(method = "histogram")
#'
#' \dontrun{
#'
#' # 2+ groups - mapping other aesthetics in the geom
#' rdata <- function(n, n_groups = 3, radius = 3) {
#'   list_of_dfs <- lapply(0:(n_groups-1), function(k) {
#'     mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
#'     m <- MASS::mvrnorm(n, radius*mu, diag(2))
#'     structure(data.frame(m, as.character(k)), names = c("x", "y", "c"))
#'   })
#'   do.call("rbind", list_of_dfs)
#' }
#'
#' dfc <- rdata(1000, n_groups = 5)
#' pf <- ggplot(dfc, aes(x, y, fill = c)) + coord_equal()
#'
#' pf + geom_hdr()
#' pf + geom_hdr(method = "mvnorm")
#' pf + geom_hdr(method = "mvnorm", probs = .90, alpha = .5)
#' pf + geom_hdr(method = "histogram")
#' pf + geom_hdr(method = "freqpoly")
#'
#' pc <- ggplot(dfc, aes(x, y, color = c)) +
#'  coord_equal() +
#'  theme_minimal() +
#'  theme(panel.grid.minor = element_blank())
#'
#' pc + geom_hdr_lines()
#' pc + geom_hdr_lines(method = "mvnorm")
#'
#'
#' # Data with boundaries
#' ggplot(df, aes(x^2)) + geom_histogram(bins = 30)
#' ggplot(df, aes(x^2)) + geom_histogram(bins = 30, boundary = 0)
#' ggplot(df, aes(x^2, y^2)) + geom_hdr(method = "histogram")
#'
#' }
#'
NULL


#' @rdname geom_hdr
#' @export
stat_hdr <- function(mapping = NULL, data = NULL,
                     geom = "hdr", position = "identity",
                     ...,
                     method = "kde",
                     probs = c(.99, .95, .8, .5),
                     n = 100,
                     xlim = NULL,
                     ylim = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHdr,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      probs = probs,
      n = n,
      xlim = xlim,
      ylim = ylim,
      na.rm = na.rm,
      ...
    )
  )
}




#' @rdname geom_hdr
#' @format NULL
#' @usage NULL
#' @importFrom scales percent
#' @export
StatHdr <- ggproto("StatHdr", Stat,

  required_aes = c("x", "y"),
  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),

  output = "bands",

  compute_group = function(self, data, scales, na.rm = FALSE,
                           method = "kde", probs = c(.99, .95, .8, .5),
                           n = 100, xlim = NULL, ylim = NULL) {

    rangex <- xlim %||% scales$x$dimension()
    rangey <- ylim %||% scales$y$dimension()

    # Only calculate HDR membership if we need to
    need_membership <- (self$output == "points")

    res <- get_hdr(data, method, probs, n, rangex, rangey, hdr_membership = need_membership)

    res_to_df(res, probs, data$group[1], self$output)

  }
)

# internal helper function to convert output of `get_hdr[_1d]()` into
# what `GeomHdr*$draw_group()` methods need
res_to_df <- function(res, probs, group, output) {

  probs <- fix_probs(probs)

  # Need z for xyz_to_isobands/lines()
  res$df_est$z <- res$df_est$fhat

  if (output == "bands") {

    isobands <- xyz_to_isobands(res$df_est, res$breaks)
    names(isobands) <- scales::percent_format(accuracy = 1)(probs)
    df <- iso_to_polygon(isobands, group)
    df$probs <- ordered(df$level, levels = names(isobands))
    df$level <- NULL

  } else if (output == "lines") {

    isolines <- xyz_to_isolines(res$df_est, res$breaks)
    names(isolines) <- scales::percent_format(accuracy = 1)(probs)
    df <- iso_to_path(isolines, group)
    df$probs <- ordered(df$level, levels = names(isolines))
    df$level <- NULL

  } else if (output == "points") {

    df <- res$data
    df$hdr_membership <- scales::percent_format(accuracy = 1)(df$hdr_membership)
    df$probs <- ordered(df$hdr_membership, levels = scales::percent_format(accuracy = 1)(c(1, probs)))
    df$hdr_membership <- NULL

  }

  df

}



#' @rdname geom_hdr
#' @export
geom_hdr <- function(mapping = NULL, data = NULL,
                       stat = "hdr", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdr,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr
#' @format NULL
#' @usage NULL
#' @export
GeomHdr <- ggproto("GeomHdr", GeomPolygon)
