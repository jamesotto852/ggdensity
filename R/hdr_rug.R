#' Rug plots of marginal highest density region estimates
#'
#' Perform 1D density estimation, compute and plot the resulting highest density
#' regions in a way similar to [ggplot2::geom_rug()].
#' Note, the plotted objects have probabilities mapped to the `alpha` aesthetic by default.
#'
#' @section Aesthetics: geom_hdr_rug understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - x
#'   - y
#'   - alpha
#'   - fill
#'   - group
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{ \item{probs}{The probability of the highest density region, specified
#'   by `probs`, corresponding to each point.} }
#'
#' @inheritParams ggplot2::geom_rug
#' @inheritParams stat_hdr
#' @param method,method_y Density estimator(s) to use.
#'   By default `method` is used for both x- and y-axis.
#'   If specified, `method_y` will be used for y-axis.
#'   Accepts character vector: `"kde"`,`"histogram"`, `"freqpoly"`, or `"norm"`.
#'   Alternatively accepts functions  which return closures corresponding to density estimates,
#'   see `?get_hdr_1d` or `vignette("method", "ggdensity")`.
#' @name geom_hdr_rug
#' @rdname geom_hdr_rug
#'
#' @import ggplot2
#'
#' @examples
#' set.seed(1)
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
#' # Plot HDR for univariate data
#' ggplot(df, aes(x)) +
#'   geom_density() +
#'   geom_hdr_rug()
#'
#' ggplot(df, aes(y = y)) +
#'   geom_density() +
#'   geom_hdr_rug()
#'
#' # Specify location of marginal HDRs as in ggplot2::geom_rug()
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
                         method_y = "kde",
                         probs = c(.99, .95, .8, .5),
                         xlim = NULL,
                         ylim = NULL,
                         n = 512,
                         na.rm = FALSE,
                         show.legend = NA,
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
      method_y = method_y,
      probs = probs,
      xlim = xlim,
      ylim = ylim,
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
                           method = "kde", method_y = NULL,
                           probs = c(.99, .95, .8, .5),
                           xlim = NULL, ylim = NULL, n = 512) {

    # Recycle for both x, y
    if (length(n) == 1) n <- rep(n, 2)

    # If no alternative method_y, use method
    if (is.null(method_y)) method_y <- method


    # Estimate marginal densities

    # Initialize dfs for x and y axes,
    # in case only x or y are supplied:
    df_x <- data.frame()
    df_y <- data.frame()

    if (!is.null(data$x)) {

      rangex <- xlim %||% scales$x$dimension()

      res_x <- get_hdr_1d(data$x, method, probs, n[1], rangex, hdr_membership = FALSE)

      df_x <- res_to_df_1d(res_x, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_x$axis <- "x"
      df_x$y <- NA

    }


    if (!is.null(data$y)) {

      rangey <- ylim %||% scales$y$dimension()

      res_y <- get_hdr_1d(data$y, method_y, probs, n[2], rangey, hdr_membership = FALSE)

      df_y <- res_to_df_1d(res_y, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_y$axis <- "y"
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


res_to_df_1d <- function(res, probs, group, output) {

  probs <- fix_probs(probs)

  if (output == "rug") {

    probs_formatted <- scales::percent_format(accuracy = 1)(probs)

    df <- res$df_est

    # alpha will be mapped to df$probs
    df$probs <- scales::percent_format(accuracy = 1)(df$hdr)
    df$probs <- ordered(df$probs, levels = probs_formatted)
    df$hdr <- NULL

    # Discard 100% HDR if it's not in probs:
    df <- df[!is.na(df$probs),]

  }

  df

}



#' @rdname geom_hdr_rug
#' @export
geom_hdr_rug <- function(mapping = NULL, data = NULL,
                         stat = "hdr_rug", position = "identity",
                         ...,
                         outside = FALSE,
                         sides = "bl",
                         length = unit(0.03, "npc"),
                         na.rm = FALSE,
                         show.legend = NA,
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





