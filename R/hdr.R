#' Highest density regions of a 2D density estimate
#'
#' Perform 2D density estimation, compute and plot the resulting highest density regions.
#' `geom_hdr()` draws filled regions, and `geom_hdr_lines()` draws lines outlining the regions.
#' Note, the plotted objects have the level mapped to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: geom_hdr understands the following aesthetics (required aesthetics
#'   are in bold):
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
#'   geom_hdr_lines understands the following aesthetics (required aesthetics are in
#'   bold):
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
#'   \describe{ \item{level}{The level of the highest density region, specified by `probs`, corresponding to each point.} }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param method Density estimator to use, accepts character vector: `"kde"`,
#'   `"histogram"`, `"freqpoly"`, or `"mvnorm"`.
#' @param probs Probabilities to compute highest density regions for.
#' @param n,nx,ny Number of bins for histogram and frequency polygon estimators. Defaults to normal reference rule.
#' @param res Resolution of grid used in discrete approximations for kernel density and parametric estimators.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to range of data.
#' @param smooth If `TRUE`, HDRs computed by the `"histogram"` method are smoothed.
#' @param nudgex Horizontal rule for choosing witness points for smoothed histogram method, accepts character vector: `"left"`, `"none"`, `"right"`.
#' @param nudgey Vertical rule for choosing witness points for smoothed histogram method, accepts character vector: `"down"`, `"none"`, `"up"`.
#' @param h Bandwidth for kernel density estimator. If `NULL`, estimated using [MASS::bandwidth.nrd()]
#' @param adjust A multiplicative bandwidth adjustment to be used if `h` is `NULL`.
#' @name geom_hdr
#' @rdname geom_hdr
#'
#' @import ggplot2
#'
#' @examples
#'
#' # basic simulated data with bivariate normal data
#' df <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' ggplot(df, aes(x, y)) + geom_hdr()
#' ggplot(df, aes(x, y)) + geom_hdr(method = "mvnorm")
#' ggplot(df, aes(x, y)) + geom_hdr(method = "histogram", n = 10)
#'
#'
#' # two groups
#' df_a <- data.frame(x = rnorm(1000, -2), y = rnorm(1000), c = "a")
#' df_b <- data.frame(x = rnorm(1000,  2), y = rnorm(1000), c = "b")
#' df <- rbind(df_a, df_b)
#' ggplot(df, aes(x, y, fill = c)) + geom_hdr()
#' ggplot(df, aes(x, y, fill = c)) + geom_hdr(method = "mvnorm")
#'
#'
#' # highest density region boundary lines
#' ggplot(df, aes(x, y)) + geom_hdr_lines()
#' ggplot(df, aes(x, y)) + geom_hdr_lines(method = "mvnorm")
#' ggplot(df, aes(x, y, color = c)) + geom_hdr_lines() + theme_minimal()
#' ggplot(df, aes(x, y, color = c)) + geom_hdr_lines(method = "mvnorm") + theme_minimal()
#'
#'
#' # data with boundaries
#' ggplot(df, aes(x^2)) + geom_histogram()
#' ggplot(df, aes(x^2)) + geom_histogram(boundary = 0)
#' ggplot(df, aes(x^2, y^2)) + geom_hdr(method = "histogram")
#' ggplot(df, aes(x^2, y^2)) +
#'   geom_hdr(method = "histogram", boundary_x = 0, boundary_y = 0)
#'
NULL






#' @rdname geom_hdr
#' @export
stat_hdr <- function(mapping = NULL, data = NULL,
                                      geom = "hdr", position = "identity",
                                      ...,
                                      method = "kde",
                                      probs = c(.99, .95, .8, .5),
                                      n = NULL,
                                      nx = n,
                                      ny = n,
                                      res = 100,
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
      n = n,
      nx = nx,
      ny = ny,
      res = res,
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
  default_aes = aes(order = after_stat(level), alpha = after_stat(level)),

  compute_group = function(data, scales, na.rm = FALSE,
                           method = "kde", probs = c(.99, .95, .8, .5),
                           xlim = NULL, ylim = NULL,
                           nudgex = "none", nudgey = "none", smooth = FALSE,
                           n = NULL, nx = n, ny = n, res = 100,
                           adjust = c(1, 1), h = NULL) {

  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  # Should this be factored out?
  if (is.null(n)) {
    # define histogram mesh according to Scott p. 87
    if (method == "histogram") {
      rho <- cor(data$x, data$y)
      hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      nx <- round((rangex[2] - rangex[1]) / hx)
      ny <- round((rangey[2] - rangey[1]) / hy)

      # Should this be a message? A warning? Similar to geom_histogram?
      message(paste0("Argument `n` not specified. Setting `nx = ", nx, "` `ny = ", ny, "` according to normal reference rule. \n",
                     "Specify alternative values for `n` or `nx`, `ny` for improved visualization."))
    }

    if (method == "freqpoly") {
        # To-Do: fill in with rules for frequency polygons
        rho <- cor(data$x, data$y)
        hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        nx <- round((rangex[2] - rangex[1]) / hx)
        ny <- round((rangey[2] - rangey[1]) / hy)

        message(paste0("Argument `n` not specified. Setting `nx = ", nx, "` `ny = ", ny, "` according to normal reference rule. \n",
                       "Specify alternative values for `n` or `nx`, `ny` for improved visualization."))
    }
  }

  probs <- sort(probs, decreasing = TRUE)


  if (method == "kde")  isobands <- kde_iso(probs, data, res, rangex, rangey, h, adjust, type = "bands")
  if (method == "histogram") isobands <- histogram_iso(probs, data, nx, ny, rangex, rangey, nudgex, nudgey, smooth, type = "bands")
  if (method == "freqpoly") isobands <- freqpoly_iso(probs, data, nx, ny, rangex, rangey, type = "bands")
  if (method == "mvnorm") isobands <- mvnorm_iso(probs, data, res, rangex, rangey, type = "bands")

  if (!method %in% c("kde", "mvnorm", "histogram", "freqpoly")) stop("Invalid method specified")



  names(isobands) <- scales::percent_format(accuracy = 1)(probs)
  path_df <- iso_to_polygon(isobands, data$group[1])
  path_df$level <- ordered(path_df$level, levels = names(isobands))

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
