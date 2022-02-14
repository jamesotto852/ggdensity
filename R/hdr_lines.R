#' @rdname geom_hdr
#' @export
stat_hdr_lines <- function(mapping = NULL, data = NULL,
                                      geom = "hdr_lines", position = "identity",
                                      ...,
                                      method = "kde",
                                      probs = c(.99, .95, .8, .5),
                                      bins = NULL,
                                      n = 100,
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
    stat = StatHdrLines,
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




#' @rdname geom_hdr
#' @format NULL
#' @usage NULL
#' @importFrom scales percent_format
#' @export
StatHdrLines <- ggproto("StatHdrLines", Stat,

  required_aes = c("x", "y"),
  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),

  compute_group = function(data, scales, na.rm = FALSE,
                           method = "kde", probs = c(.99, .95, .8, .5),
                           xlim = NULL, ylim = NULL,
                           nudgex = "none", nudgey = "none", smooth = FALSE,
                           bins = NULL, n = 100,
                           adjust = c(1, 1), h = NULL) {

  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  # Should this be factored out?
  if (is.null(bins)) {
    bins <- numeric(2)

    # define histogram mesh according to Scott p. 87
    if (method == "histogram") {
      rho <- cor(data$x, data$y)
      hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      bins[1] <- round((rangex[2] - rangex[1]) / hx)
      bins[2] <- round((rangey[2] - rangey[1]) / hy)

      # message(paste0("Argument `bins` not specified. \n",
      #                "Setting according to normal reference rule. \n",
      #                "Specify alternative values for `bins` for improved visualization."))
    }

    if (method == "freqpoly") {
        # To-Do: fill in with rules for frequency polygons
        rho <- cor(data$x, data$y)
        hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
        bins[1] <- round((rangex[2] - rangex[1]) / hx)
        bins[2] <- round((rangey[2] - rangey[1]) / hy)

        # message(paste0("Argument `bins` not specified. \n",
        #                "Setting according to normal reference rule. \n",
        #                "Specify alternative values for `bins` for improved visualization."))
    }
  }

  # recycling scalar-valued bins if necessary
  bins <- rep(bins, length.out = 2)

  probs <- sort(probs, decreasing = TRUE)

  isolines <- switch(method,
    "kde" = kde_iso(probs, data, n, rangex, rangey, h, adjust, type = "lines"),
    "histogram" = histogram_iso(probs, data, bins, rangex, rangey, nudgex, nudgey, smooth, type = "lines"),
    "freqpoly" = freqpoly_iso(probs, data, bins, rangex, rangey, type = "lines"),
    "mvnorm" = mvnorm_iso(probs, data, n, rangex, rangey, type = "lines"),
  )

  if (!method %in% c("kde", "mvnorm", "histogram", "freqpoly")) stop("Invalid method specified")



  names(isolines) <- scales::percent_format(accuracy = 1)(probs)
  path_df <- iso_to_path(isolines, data$group[1])
  path_df$probs <- ordered(path_df$level, levels = names(isolines))

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
    linetype = 1,
    alpha = NA
  ))
