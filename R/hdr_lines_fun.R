#' @rdname geom_hdr_fun
#' @export
stat_hdr_lines_fun <- function(mapping = NULL, data = NULL,
                                         geom = "hdr_lines_fun", position = "identity",
                                         ...,
                                         fun, args = list(), normalized = TRUE,
                                         probs = c(.99, .95, .8, .5),
                                         xlim = NULL, ylim = NULL, res = 100,
                                         na.rm = FALSE,
                                         show.legend = NA,
                                         inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrLinesFun,
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
StatHdrLinesFun <- ggproto("StatHdrlinesfun", Stat,

  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),

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

  isobands <- fun_iso(fun, args, normalized, probs, res, rangex, rangey, scales, type = "lines")

  names(isobands) <- scales::percent_format(accuracy = 1)(probs)
  path_df <- iso_to_polygon(isobands, data$group[1])
  path_df$probs <- ordered(path_df$level, levels = names(isobands))
  path_df$level <- NULL

  path_df

  }
)


#' @rdname geom_hdr_fun
#' @export
geom_hdr_lines_fun <- function(mapping = NULL, data = NULL,
                         stat = "hdr_lines_fun", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrLinesFun,
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
# GeomHdrLinesFun <- ggproto("GeomHdrlinesfun", GeomHdrLines)
GeomHdrLinesFun <- ggproto("GeomHdrlinesfun", GeomPath,
  default_aes = aes(
    size = 1,
    colour = "#000000",
    linetype = 1,
    alpha = NA
  ))
