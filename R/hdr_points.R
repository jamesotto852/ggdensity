#' Scatterplot colored by highest density regions of a 2D density estimate
#'
#' Perform 2D density estimation, compute the resulting highest density regions (HDRs),
#' and plot the provided data as a scatterplot with points colored according to
#' their corresponding HDR.
#'
#' @section Aesthetics: geom_hdr_points understands the following aesthetics (required
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
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability associated with the highest density region, specified
#'   by `probs`.} }
#'
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @inheritParams geom_hdr
#'
#' @name geom_hdr_points
#' @rdname geom_hdr_points
#'
#' @import ggplot2
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(500), y = rnorm(500))
#' p <- ggplot(df, aes(x, y)) +
#'  coord_equal()
#'
#' p + geom_hdr_points()
#'
#' # Setting aes(fill = after_stat(probs)), color = "black", and
#' # shape = 21 helps alleviate overplotting:
#' p + geom_hdr_points(aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2)
#'
#' # Also works well with geom_hdr_lines()
#' p +
#'  geom_hdr_lines(
#'    aes(color = after_stat(probs)), alpha = 1,
#'    xlim = c(-5, 5), ylim = c(-5, 5)
#'  ) +
#'  geom_hdr_points(
#'    aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2,
#'    xlim = c(-5, 5), ylim = c(-5, 5)
#'  )
#'
NULL



#' @export
#' @rdname geom_hdr_points
stat_hdr_points <- function(mapping = NULL, data = NULL,
                            geom = "point", position = "identity",
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
    stat = StatHdrPoints,
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


#' @export
#' @rdname geom_hdr_points
#' @format NULL
#' @usage NULL
StatHdrPoints <- ggproto("StatHdrPoints", StatHdr,
  default_aes = aes(order = after_stat(probs), color = after_stat(probs)),
  output = "points"
)


#' @export
#' @rdname geom_hdr_points
geom_hdr_points <- function(mapping = NULL, data = NULL,
                            stat = "hdr_points", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

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

