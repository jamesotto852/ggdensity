#' Rug plots of marginal highest density region estimates
#'
#' Perform 1D density estimation, compute and plot the resulting highest density
#' regions in a way similar to [ggplot2::geom_rug()]. Note, the plotted objects have the probs mapped to the `alpha`
#' aesthetic by default.
#'
#' @section Aesthetics: geom_hdr_rug understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - alpha
#'   - fill
#'   - group
#'   - subgroup
#'   - x
#'   - y
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability of the highest density region, specified
#'   by `probs`, corresponding to each point.} }
#'
#' @inheritParams ggplot2::geom_rug
#' @inheritParams ggplot2::stat_density
#' @inheritParams stat_hdr
#' @param bins Number of bins along each axis for histogram and frequency polygon estimators.
#'   Either a vector of length 2 or a scalar value which is recycled for both dimensions.
#'   Defaults to normal reference rule (Scott, pg 87).
#' @param n Resolution of grid used in discrete approximations for kernel
#'   density and parametric estimators.
#' @param h The smoothing bandwidth to be used.
#'   If numeric, the standard deviation of the smoothing kernel.
#'   If character, a rule to choose the bandwidth, as listed in
#'   [stats::bw.nrd()].
#' @name geom_hdr_rug
#' @rdname geom_hdr_rug
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @import ggplot2
#'
#' @examples
#'
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Plot marginal HDRs for bivariate data
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_hdr_rug() +
#'   coord_fixed()
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr() +
#'   geom_hdr_rug() +
#'   coord_fixed()
#'
#' # Or, plot marginal HDR for univariate data
#' ggplot(df, aes(x)) +
#'   geom_density() +
#'   geom_hdr_rug()
#'
#' ggplot(df, aes(y = y)) +
#'   geom_density() +
#'   geom_hdr_rug()
#'
#' # Can specify location of marginal HDRs as in ggplot2::geom_rug(),
#' ggplot(df, aes(x, y)) +
#'   geom_hdr() +
#'   geom_hdr_rug(sides = "tr", outside = TRUE) +
#'   coord_fixed(clip = "off")
#'
#' # Can use same methods of density estimation as geom_hdr().
#' # For data with constrained support, we suggest setting method = "histogram":
#' ggplot(df, aes(x^2)) +
#'  geom_histogram(bins = 30, boundary = 0) +
#'  geom_hdr_rug(method = "histogram")
#'
#' ggplot(df, aes(x^2, y^2)) +
#'  geom_hdr(method = "histogram") +
#'  geom_hdr_rug(method = "histogram") +
#'  coord_fixed()
#'
NULL






#' @rdname geom_hdr_rug
#' @export
stat_hdr_rug <- function(mapping = NULL, data = NULL,
                                      geom = "hdr_rug", position = "identity",
                                      ...,
                                      method = "kde",
                                      probs = c(.99, .95, .8, .5),
                                      xlim = NULL,
                                      ylim = NULL,
                                      h = "nrd0",
                                      adjust = 1,
                                      kernel = "gaussian",
                                      bins = NULL,
                                      n = 512,
                                      na.rm = FALSE,
                                      show.legend = TRUE,
                                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrRug,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      h = h,
      adjust = adjust,
      kernel = kernel,
      bins = bins,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr_rug
#' @format NULL
#' @usage NULL
#' @export
StatHdrRug <- ggproto("StatHdrRug", Stat,

  required_aes = c("x|y"),
  default_aes = aes(alpha = after_stat(probs)),

  compute_group = function(data, scales, na.rm = FALSE,
                           method = "kde", probs = c(.99, .95, .8, .5),
                           xlim = NULL, ylim = NULL,
                           h = "nrd0",
                           adjust = 1,
                           kernel = "gaussian",
                           bins = NULL,
                           n = 512) {

  probs <- probs[order(probs, decreasing = TRUE)]

  # Recycle vectors of length 1 for both x, y
  h <- rep(h, 2)
  adjust <- rep(adjust, 2)
  kernel <- rep(kernel, 2)
  bins <- rep(bins, 2)
  n <- rep(n, 2)

  # Estimate marginal densities

  # Initialize dfs for x and y axes,
  # in case only x or y are supplied:
  df_x <- data.frame()
  df_y <- data.frame()

  if (!is.null(data$x)) {

    rangex <- xlim %||% scales$x$dimension()

    df_x <- switch(method,
      "kde" = kde_marginal(data$x, data$weight, rangex[1], rangex[2], h[1], adjust[1], kernel[1], n[1]),
      "histogram" = hist_marginal(data$x, rangex[1], rangex[2], bins[1]),
      "freqpoly" = freqpoly_marginal(data$x, rangex[1], rangex[2], bins[1], n[1]),
      "norm" = norm_marginal(data$x, rangex[1], rangex[2], n[1])
    )
    if (!(method %in% c("kde", "norm", "histogram", "freqpoly"))) stop("Invalid method specified")

    # Find vals. of f_hat for different HDRs
    cutoffs_x <- find_cutoff(df_x, probs)
    find_hdr_x <- assign_cutoff(probs, cutoffs_x)

    # Assign each point along axes to an HDR
    df_x$probs <- find_hdr_x(df_x$fhat)
    df_x <- df_x[!is.na(df_x$probs),]
    df_x$probs <- scales::percent_format(accuracy = 1)(df_x$probs)
    df_x$probs <- ordered(df_x$probs, scales::percent_format(accuracy = 1)(probs))

    df_x$axis <- "x"
    df_x$y <- NA

  }


  if (!is.null(data$y)) {

    rangey <- ylim %||% scales$y$dimension()

    df_y <- switch(method,
      "kde" = kde_marginal(data$y, data$weight, rangey[1], rangey[2], h[1], adjust[1], kernel[1], n[1]),
      "histogram" = hist_marginal(data$y, rangey[1], rangey[2], bins[1]),
      "freqpoly" = freqpoly_marginal(data$y, rangey[1], rangey[2], bins[2], n[2]),
      "norm" = norm_marginal(data$y, rangey[1], rangey[2], n[2])
    )
    if (!(method %in% c("kde", "norm", "histogram", "freqpoly"))) stop("Invalid method specified")

    cutoffs_y <- find_cutoff(df_y, probs)
    find_hdr_y <- assign_cutoff(probs, cutoffs_y)

    df_y$probs <- find_hdr_y(df_y$fhat)
    df_y <- df_y[!is.na(df_y$probs),]
    df_y$probs <- scales::percent_format(accuracy = 1)(df_y$probs)
    df_y$probs <- ordered(df_y$probs, scales::percent_format(accuracy = 1)(probs))

    df_y$axis <- "y"
    # Needs correct name for ggplot2 internals
    df_y$y <- df_y$x
    df_y$x <- NA

  }

  df <- rbind(df_x, df_y)

  # Need to remove extra col if only plotting x or y rug
  if (is.null(data$x)) df$x <- NULL
  if (is.null(data$y)) df$y <- NULL

  df
  }
)


#' @rdname geom_hdr_rug
#' @export
geom_hdr_rug <- function(mapping = NULL, data = NULL,
                         stat = "hdr_rug", position = "identity",
                         ...,
                         outside = FALSE,
                         sides = "bl",
                         length = unit(0.03, "npc"),
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrRug,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outside = outside,
      sides = sides,
      length = length,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_hdr_rug
#' @format NULL
#' @usage NULL
#' @export
GeomHdrRug <- ggproto("GeomHdrRug", Geom,
   optional_aes = c("x", "y"),

   draw_panel = function(data, panel_params, coord, sides = "bl",
                         outside = FALSE, length = unit(0.03, "npc")) {

     if (!inherits(length, "unit")) {
       abort("'length' must be a 'unit' object.")
     }
     rugs <- list()

     # For coord_flip, coord$tranform does not flip the sides where to
     # draw the rugs. We have to flip them.
     if (inherits(coord, 'CoordFlip')) {
       sides <- chartr('tblr', 'rlbt', sides)
     }

     # move the rug to outside the main plot space
     if (outside) length <- -length

     # Set up data frames for x and y:
     data_x <- data[data$axis == "x",]
     data_y <- data[data$axis == "y",]


     if (nrow(data_x) > 0) {

       data_x <- coord$transform(data_x, panel_params)
       data_x$width <- resolution(data_x$x, FALSE)

       gp_x <- grid::gpar(
         col = alpha(data_x$fill, data_x$alpha),
         fill = alpha(data_x$fill, data_x$alpha),
         lwd = 0
       )

       # set up x axis rug rasters
       if (grepl("b", sides)) {
         rugs$x_b <- grid::rectGrob(
           x = unit(data_x$x, "native"),
           y = unit(0, "npc"),
           width = data_x$width,
           height = length,
           just = "bottom",
           gp = gp_x
         )
       }

       if (grepl("t", sides)) {
         rugs$x_t <- grid::rectGrob(
           x = unit(data_x$x, "native"),
           y = unit(1, "npc"),
           width = data_x$width,
           height = length,
           just = "top",
           gp = gp_x
         )
       }
     }

     if (nrow(data_y) > 0) {

       data_y <- coord$transform(data_y, panel_params)
       data_y$height <- resolution(data_y$y, FALSE)

       gp_y <- grid::gpar(
         col = alpha(data_y$fill, data_y$alpha),
         fill = alpha(data_y$fill, data_y$alpha),
         lwd = 0
       )


       # set up y axis rug rasters
       if (grepl("l", sides)) {
         rugs$y_l <- grid::rectGrob(
           x = unit(0, "npc"),
           y = unit(data_y$y, "native"),
           width = length,
           height = data_y$height,
           just = "left",
           gp = gp_y
         )
       }

       if (grepl("r", sides)) {
         rugs$y_r <- grid::rectGrob(
           x = unit(1, "npc"),
           y = unit(data_y$y, "native"),
           width = length,
           height = data_y$height,
           just = "right",
           gp = gp_y
         )
       }

     }

     grid::gTree(children = do.call(grid::gList, rugs))

   },

  default_aes = aes(fill = "grey20", alpha = NA),

  draw_key = draw_key_rect
)





