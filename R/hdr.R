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
#' - size
#' - subgroup ?
#'
#' @section Computed variables:
#'
#' \describe{
#'   \item{value}{The highest density region estimate}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
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
#' # Simulating data
#' df <- data.frame(
#'   x = rnorm(1000),
#'   y = rnorm(1000)
#' )
#'
#' # Plotting the estimated density
#' ggplot(df, aes(x, y)) +
#'   geom_hdr()
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = "mvnorm")
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = "histogram", n = 10)
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
                           n = 100, nx = n, ny = n,
                           adjust = c(1, 1), h = NULL) {


  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  probs <- sort(probs, decreasing = TRUE)


  if (method == "kde")  isobands <- kde_isobands(probs, data, nx, ny, rangex, rangey, h, adjust)
  if (method == "histogram") isobands <- histogram_isobands(probs, data, nx, ny, rangex, rangey, nudgex, nudgey)
  if (method == "mvnorm") isobands <- mvnorm_isobands(probs, data, nx, ny, rangex, rangey)

  if (!method %in% c("kde", "mvnorm", "histogram")) stop("Invalid method specified")



  names(isobands) <- scales::percent(probs)
  path_df <- ggplot2:::iso_to_polygon(isobands, data$group[1])
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
