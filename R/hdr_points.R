stat_hdr_points <- function(mapping = NULL,
                            data = NULL,
                            geom = "point",
                            position = "identity",
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
    stat = StatHdrPoints,
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


StatHdrPoints <- ggproto("StatHdrPoints", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(order = after_stat(probs), color = after_stat(probs)),

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

  HDR_fun <- switch(method,
    "kde" = kde_iso(probs, data, n, rangex, rangey, h, adjust, type = "bands", HDR_fun = TRUE),
    "histogram" = histogram_iso(probs, data, bins, rangex, rangey, nudgex, nudgey, smooth, type = "bands", HDR_fun = TRUE),
    "freqpoly" = freqpoly_iso(probs, data, bins, rangex, rangey, type = "bands", HDR_fun = TRUE),
    "mvnorm" = mvnorm_iso(probs, data, n, rangex, rangey, type = "bands", HDR_fun = TRUE)
  )
  if (!(method %in% c("kde", "mvnorm", "histogram", "freqpoly"))) stop("Invalid method specified")

  data$probs <- HDR_fun(data$x, data$y)
  # Could do this in HDR_fun
  data$probs <- scales::percent_format(accuracy = 1)(data$probs)
  data$probs <- ordered(data$probs, levels = scales::percent_format(accuracy = 1)(c(1, probs)))

  data
  }
)


geom_hdr_points <- function(mapping = NULL,
                            data = NULL,
                            stat = "hdr_points",
                            position = "identity",
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
    stat = stat,
    geom = GeomPoint,
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

