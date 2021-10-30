#' Highest density regions of a 2D density estimate
#'
#' Put description here
#'
#' @section Aesthetics:
#' geom_hdr understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - color
#' - fill
#' - group ?
#' - linetype
#' - size ?
#' - subgroup ?
#'
#' geom_hdr_lines understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - color
#' - group ?
#' - linetype
#' - size
#' - subgroup ?
#'
#' @section Computed variables:
#'
#' Describe difference between geom_hdr and geom_hdr_lines, see `?geom_density2d`
#'
#' \describe{
#'   \item{value}{The highest density region estimate}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param method Density estimator to use, accepts character vector: `"kde"`, `"histogram"`, or `"mvnorm"`.
#' @param probs Temp
#' @param n,nx,ny Temp
#' @param xlim,ylim Temp
#' @param nudgex Temp
#' @param nudgey Temp
#' @param h Temp
#'
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
                                      n = 100,
                                      nx = n,
                                      ny = n,
                                      xlim = NULL,
                                      ylim = NULL,
                                      nudgex = "none",
                                      nudgey = "none",
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
      xlim = xlim,
      ylim = ylim,
      nudgex = nudgex,
      nudgey = nudgey,
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
                           nudgex = "none", nudgey = "none",
                           n = NULL, nx = n, ny = n,
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
    } else {
      nx <- 100
      ny <- 100
    }
  }

  probs <- sort(probs, decreasing = TRUE)


  if (method == "kde")  isobands <- kde_iso(probs, data, nx, ny, rangex, rangey, h, adjust, type = "bands")
  if (method == "histogram") isobands <- histogram_iso(probs, data, nx, ny, rangex, rangey, nudgex, nudgey, type = "bands")
  if (method == "mvnorm") isobands <- mvnorm_iso(probs, data, nx, ny, rangex, rangey, type = "bands")

  if (!method %in% c("kde", "mvnorm", "histogram")) stop("Invalid method specified")



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
