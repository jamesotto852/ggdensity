#' Rug plots of highest density region estimates of univariate pdfs
#'
#' TODO
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
#' @inheritParams stat_hdr_rug
#' @param fun_x,fun_y temp
#' @param args_x,args_y temp
#' @name geom_hdr_rug_fun
#' @rdname geom_hdr_rug_fun
#'
#' @examples
#'
#' # TODO
#'
NULL


#' @rdname geom_hdr_rug_fun
#' @export
stat_hdr_rug_fun <- function(mapping = NULL, data = NULL,
  geom = "hdr_rug_fun", position = "identity",
  ...,
  fun_x = NULL, fun_y = NULL,
  args_x = list(), args_y = list(),
  probs = c(.99, .95, .8, .5),
  xlim = NULL, ylim = NULL, n = 512,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrRugFun,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun_x = fun_x,
      fun_y = fun_y,
      args_x = args,
      args_y = args_y,
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_hdr_rug_fun
#' @format NULL
#' @usage NULL
#' @export
StatHdrRugFun <- ggproto("StatHdrRugFun", Stat,

  default_aes = aes(order = after_stat(probs), alpha = after_stat(probs)),

  # very similar to StatHdrRug$compute_group(),
  # only difference are the parameters fun + args (vs. method + parameters)
  # -- this prevents factoring into one compute_group() method,
  #    compute_group()'s arguments are different.
  compute_group = function(self, data, scales, na.rm = FALSE,
                           fun_x = NULL, fun_y = NULL, args_x = list(), args_y = list(),
                           probs = c(.99, .95, .8, .5),
                           n = 512, xlim = NULL, ylim = NULL) {


    # Recycle for both x, y
    if (length(n) == 1) n <- rep(n, 2)

    # Estimate marginal densities

    # Initialize dfs for x and y axes,
    # in case only x or y are supplied:
    df_x <- data.frame()
    df_y <- data.frame()


    if (!is.null(fun_x)) {

      if (is.null(xlim) & is.null(scales$x)) {
        stop("`xlim` must be specified if `x` aesthetic not provided to `StatHdrRugFun`")
      }

      rangex <- xlim %||% scales$x$dimension()

      res_x <- get_hdr_1d(data$x, method = "fun", probs, n[1], rangex, HDR_membership = FALSE, fun = fun_x, args = args_x)

      df_x <- res_to_df_1d(res_x, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_x$axis <- "x"
      df_x$y <- NA

    } else {
      # If x aesthetic is provided but no fun_x, need to issue warning (alongside ggplot2's warning)
      if (! is.null(data$x)) warning("`x` aesthetic provided to `StatHdrRugFun` but not `fun_x`. \n Either provide `fun_x`, remove the `x` mapping, \n or set `inherit.aes = FALSE`")
    }


    if (!is.null(fun_y)) {

      if (is.null(ylim) & is.null(scales$y)) {
        stop("`ylim` must be specified if `y` aesthetic not provided to `StatHdrRugFun`")
      }

      rangey <- ylim %||% scales$y$dimension()

      res_y <- get_hdr_1d(data$y, method = "fun", probs, n[1], rangey, HDR_membership = FALSE, fun = fun_y, args = args_y)

      df_y <- res_to_df_1d(res_y, probs, data$group[1], output = "rug")

      # Needs correct name for ggplot2 internals
      df_y$axis <- "y"
      df_y$y <- df_y$x
      df_y$x <- NA

    } else {
      # If y aesthetic is provided but no fun_y, need to issue warning (alongside ggplot2's warning)
      if (! is.null(data$y)) warning("`y` aesthetic provided to `StatHdrRugFun` but not `fun_y`. \n Either provide `fun_y`, remove the `y` mapping, \n or set `inherit.aes = FALSE`")
    }

    df <- rbind(df_x, df_y)

    # Need to remove extra col if only plotting x or y rug
    if (is.null(fun_x)) df$x <- NULL
    if (is.null(fun_y)) df$y <- NULL

    df


  }
)



#' @rdname geom_hdr_rug_fun
#' @export
geom_hdr_rug_fun <- function(mapping = NULL, data = NULL,
                         stat = "hdr_rug_fun", position = "identity",
                         ...,
                         outside = FALSE,
                         sides = "bl",
                         length = unit(0.03, "npc"),
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrRugFun,
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


#' @rdname geom_hdr_rug_fun
#' @format NULL
#' @usage NULL
#' @export
GeomHdrRugFun <- ggproto("GeomHdrRugFun", GeomHdrRug)
