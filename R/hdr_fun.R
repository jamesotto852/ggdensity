#' Highest density regions of a bivariate pdf
#'
#'
#' @section Aesthetics: geom_hdr_fun understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - size
#'   - subgroup
#'
#'   geom_hdr_fun_lines understands the following aesthetics (required aesthetics
#'   are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - color
#'   - group
#'   - linetype
#'   - size
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{level}{The level of the highest density region, specified
#'   by `probs`, corresponding to each point.} }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_density2d
#' @param probs Probabilities to compute highest density regions for.
#' @param res Resolution of grid `fun` is evaluated on.
#' @name geom_hdr_fun
#' @rdname geom_hdr_fun
#'
#' @import ggplot2
#'
#' @examples
#'
#'
#'
NULL






#' @rdname geom_hdr_fun
#' @export
stat_hdr_fun <- function(mapping = NULL, data = NULL,
                                         geom = "hdr_fun", position = "identity",
                                         ...,
                                         fun, args = list(), normalized = TRUE,
                                         probs = c(.99, .95, .8, .5),
                                         xlim = NULL, ylim = NULL, res = 100,
                                         na.rm = FALSE,
                                         show.legend = NA,
                                         inherit.aes = TRUE) {

  if (is.null(data)) {
    data <- ggplot2:::ensure_nonempty_data
  }

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



#' @rdname geom_hdr_fun
#' @format NULL
#' @usage NULL
#' @importFrom scales percent
#' @export
StatHdrFun <- ggproto("StatHdrFun", Stat,

  default_aes = aes(order = after_stat(level), alpha = after_stat(level)),

  compute_group = function(data, scales, na.rm = FALSE,
                           fun, args = list(), normalized = TRUE, probs = c(.99, .95, .8, .5),
                           res = 100, xlim = NULL, ylim = NULL)  {

  # Allow for use if data = NULL
  if (is.null(scales$x)) {
    rangex <- xlim %||% c(0, 1)
    rangey <- ylim %||% c(0, 1)
  } else {
    rangex <- xlim %||% scales$x$dimension()
    rangey <- ylim %||% scales$y$dimension()
  }


  probs <- sort(probs, decreasing = TRUE)

  isobands <- fun_iso(fun, args, normalized, probs, res, rangex, rangey, type = "bands")

  names(isobands) <- scales::percent_format(accuracy = 1)(probs)
  path_df <- iso_to_polygon(isobands, data$group[1])
  path_df$level <- ordered(path_df$level, levels = names(isobands))

  path_df

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

  if (is.null(data)) {
    data <- ggplot2:::ensure_nonempty_data
  }

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
