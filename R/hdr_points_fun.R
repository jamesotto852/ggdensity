stat_hdr_points_fun <- function(mapping = NULL, data = NULL,
                                geom = "hdr_points_fun", position = "identity",
                                ...,
                                fun, args = list(), normalized = TRUE,
                                probs = c(.99, .95, .8, .5),
                                xlim = NULL, ylim = NULL, res = 100,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrPointsFun,
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


StatHdrPointsFun <- ggproto("StatHdrpointsfun", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(order = after_stat(probs), color = after_stat(probs)),

  compute_group = function(data, scales, na.rm = FALSE,
                           fun, args = list(), normalized = TRUE, probs = c(.99, .95, .8, .5),
                           res = 100, xlim = NULL, ylim = NULL) {

  # Allow for use if data = NULL
  if (is.null(scales$x)) {
    rangex <- xlim %||% c(0, 1)
    rangey <- ylim %||% c(0, 1)
  } else {
    rangex <- xlim %||% scales$x$dimension()
    rangey <- ylim %||% scales$y$dimension()
  }

  probs <- sort(probs, decreasing = TRUE)

  HDR_fun <- fun_iso(fun, args, normalized, probs, res, rangex, rangey, scales, type = NULL, HDR_fun = TRUE)

  data$probs <- HDR_fun(data$x, data$y)
  # Could do this in HDR_fun
  data$probs <- scales::percent_format(accuracy = 1)(data$probs)
  data$probs <- ordered(data$probs, levels = scales::percent_format(accuracy = 1)(c(1, probs)))

  data
  }
)


geom_hdr_points_fun <- function(mapping = NULL, data = NULL,
                                stat = "hdr_points_fun", position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

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

