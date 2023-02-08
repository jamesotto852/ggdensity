#' @rdname geom_hdr
#' @usage NULL
#' @export
stat_hdr_lines <- function(mapping = NULL, data = NULL,
                           geom = "hdr_lines", position = "identity",
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
    stat = StatHdrLines,
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
#' @importFrom scales percent_format
#' @export
StatHdrLines <- ggproto("StatHdrLines", StatHdr,
  output = "lines"
)


#' @rdname geom_hdr
#' @usage NULL
#' @export
geom_hdr_lines <- function(mapping = NULL, data = NULL,
                           stat = "hdr_lines", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrLines,
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
GeomHdrLines <- ggproto("GeomHdrLines", GeomPath,
  default_aes = aes(
    colour = "#000000",
    linewidth = 1,
    linetype = 1,
    alpha = NA
  ))
