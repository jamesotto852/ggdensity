#' @rdname geom_hdr
#' @export
stat_hdr_lines <- function(mapping = NULL, data = NULL,
                                      geom = "hdr_lines", position = "identity",
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
    stat = StatHdrLines,
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
StatHdrLines <- ggproto("StatHdrLines", Stat,

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


  if (method == "kde")  isolines <- kde_iso(probs, data, nx, ny, rangex, rangey, h, adjust, type = "lines")
  if (method == "histogram") isolines <- histogram_iso(probs, data, nx, ny, rangex, rangey, nudgex, nudgey, type = "lines")
  if (method == "mvnorm") isolines <- mvnorm_iso(probs, data, nx, ny, rangex, rangey, type = "lines")

  if (!method %in% c("kde", "mvnorm", "histogram")) stop("Invalid method specified")



  names(isolines) <- scales::percent(probs)
  path_df <- ggplot2:::iso_to_path(isolines, data$group[1])
  path_df$level <- ordered(path_df$level, levels = names(isolines))

  path_df

  }
)


#' @rdname geom_hdr
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
    size = 1,
    colour = "#000000",
    linetype = 1
  ))
