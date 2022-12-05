#' @rdname geom_hdr_fun
#' @export
stat_hdr_lines_fun <- function(mapping = NULL, data = NULL,
                               geom = "hdr_lines_fun", position = "identity",
                               ...,
                               fun, args = list(),
                               probs = c(.99, .95, .8, .5),
                               xlim = NULL, ylim = NULL, n = 100,
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
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
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
StatHdrLinesFun <- ggproto("StatHdrLinesFun", StatHdrFun,
  output = "lines"
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
