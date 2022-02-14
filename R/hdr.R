#' Highest density regions of a 2D density estimate
#'
#' Perform 2D density estimation, compute and plot the resulting highest density
#' regions. `geom_hdr()` draws filled regions, and `geom_hdr_lines()` draws
#' lines outlining the regions. Note, the plotted objects have the probs mapped
#' to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: geom_hdr understands the following aesthetics (required
#'   aesthetics are in bold):
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
#'   geom_hdr_lines understands the following aesthetics (required aesthetics
#'   are in bold):
#'
#'   - **x**
#'   - **y**
#'   - alpha
#'   - color
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
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param method Density estimator to use, accepts character vector: `"kde"`,
#'   `"histogram"`, `"freqpoly"`, or `"mvnorm"`.
#' @param probs Probabilities to compute highest density regions for.
#' @param bins Number of bins along each axis for histogram and frequency polygon estimators.
#'   Either a vector of length 2 or a scalar value which is recycled for both dimensions.
#'   Defaults to normal reference rule (Scott, pg 87).
#' @param n Resolution of grid used in discrete approximations for kernel
#'   density and parametric estimators.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to
#'   range of data.
#' @param smooth If `TRUE`, HDRs computed by the `"histogram"` method are
#'   smoothed.
#' @param nudgex Horizontal rule for choosing witness points for smoothed
#'   histogram method, accepts character vector: `"left"`, `"none"`, `"right"`.
#' @param nudgey Vertical rule for choosing witness points for smoothed
#'   histogram method, accepts character vector: `"down"`, `"none"`, `"up"`.
#' @param h Bandwidth for kernel density estimator. If `NULL`, estimated using
#'   [MASS::bandwidth.nrd()]
#' @param adjust A multiplicative bandwidth adjustment to be used if `h` is
#'   `NULL`.
#' @name geom_hdr
#' @rdname geom_hdr
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @import ggplot2
#'
#' @examples
#'
#' # basic simulated data with bivariate normal data and various methods
#' df <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' p <- ggplot(df, aes(x, y)) + coord_equal()
#' p + geom_hdr()
#' p + geom_hdr(method = "mvnorm")
#' p + geom_hdr(method = "histogram")
#' p + geom_hdr(method = "freqpoly")
#'
#'
#'
#' # adding point layers on top to visually assess region estimates
#' pts <- geom_point(size = .2, color = "red")
#' p + geom_hdr() + pts
#' p + geom_hdr(method = "mvnorm") + pts
#' # p + geom_hdr(method = "histogram") + pts
#'
#'
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
#' pf + geom_hdr()
#' pf + geom_hdr(method = "mvnorm")
#' pf + geom_hdr(method = "mvnorm", probs = .90, alpha = .5)
#' # pf + geom_hdr(method = "histogram")
#' # pf + geom_hdr(method = "freqpoly")
#'
#'
#'
#' # highest density region boundary lines
#' p + geom_hdr_lines()
#' p + geom_hdr_lines(method = "mvnorm")
#'
#' pc <- ggplot(dfc, aes(x, y, color = c)) + coord_equal() + theme_minimal() +
#'   theme(panel.grid.minor = element_blank())
#' pc + geom_hdr_lines()
#' pc + geom_hdr_lines(method = "mvnorm")
#'
#'
#'
#' # data with boundaries
#' ggplot(df, aes(x^2)) + geom_histogram(bins = 30)
#' ggplot(df, aes(x^2)) + geom_histogram(bins = 30, boundary = 0)
#' ggplot(df, aes(x^2, y^2)) + geom_hdr(method = "histogram")
#'
#'
#'
NULL






#' @rdname geom_hdr
#' @export
stat_hdr <- function(mapping = NULL, data = NULL,
                                      geom = "hdr", position = "identity",
                                      ...,
                                      method = "kde",
                                      probs = c(.99, .95, .8, .5),
                                      bins = NULL,
                                      n = 100,
                                      xlim = NULL,
                                      ylim = NULL,
                                      nudgex = "none",
                                      nudgey = "none",
                                      smooth = FALSE,
                                      adjust = c(1, 1),
                                      h = NULL,
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
      bins = bins,
      n = n,
      xlim = xlim,
      ylim = ylim,
      nudgex = nudgex,
      nudgey = nudgey,
      smooth = smooth,
      adjust = adjust,
      h = h,
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

  compute_group = function(data, scales, na.rm = FALSE,
                           method = "kde", probs = c(.99, .95, .8, .5),
                           xlim = NULL, ylim = NULL,
                           nudgex = "none", nudgey = "none", smooth = FALSE,
                           bins = NULL, n = 100,
                           adjust = c(1, 1), h = NULL) {

  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  # Should this be factored out?
  if (is.null(bins)) {
    bins <- numeric(2)

    # define histogram mesh according to Scott p. 87
    if (method == "histogram") {
      rho <- cor(data$x, data$y)
      hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      bins[1] <- round((rangex[2] - rangex[1]) / hx)
      bins[2] <- round((rangey[2] - rangey[1]) / hy)

      # message(paste0("Argument `bins` not specified. \n",
      #                "Setting according to normal reference rule. \n",
      #                "Specify alternative values for `bins` for improved visualization."))
    }

    if (method == "freqpoly") {
        # To-Do: fill in with rules for frequency polygons
        rho <- cor(data$x, data$y)
        hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        bins[1] <- round((rangex[2] - rangex[1]) / hx)
        bins[2] <- round((rangey[2] - rangey[1]) / hy)

        # message(paste0("Argument `bins` not specified. \n",
        #                "Setting according to normal reference rule. \n",
        #                "Specify alternative values for `bins` for improved visualization."))
    }
  }

  # recycling scalar-valued bins if necessary
  bins <- rep(bins, length.out = 2)

  probs <- sort(probs, decreasing = TRUE)

  isobands <- switch(method,
    "kde" = kde_iso(probs, data, n, rangex, rangey, h, adjust, type = "bands"),
    "histogram" = histogram_iso(probs, data, bins, rangex, rangey, nudgex, nudgey, smooth, type = "bands"),
    "freqpoly" = freqpoly_iso(probs, data, bins, rangex, rangey, type = "bands"),
    "mvnorm" = mvnorm_iso(probs, data, n, rangex, rangey, type = "bands"),
  )
  if (!(method %in% c("kde", "mvnorm", "histogram", "freqpoly"))) stop("Invalid method specified")

  names(isobands) <- scales::percent_format(accuracy = 1)(probs)
  path_df <- iso_to_polygon(isobands, data$group[1])
  path_df$probs <- ordered(path_df$level, levels = names(isobands))

  path_df

  }
)


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
