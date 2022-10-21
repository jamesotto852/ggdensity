# unexported functions from ggplot2

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

tibble0 <- function(...) {
  tibble::tibble(..., .name_repair = "minimal")
}

unique0 <- function(x, ...) {
  if (is.null(x)) x else vctrs::vec_unique(x, ...)
}

isoband_z_matrix <- function(data) {
  x_pos <- as.integer(factor(data$x, levels = sort(unique0(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique0(data$y))))
  nrow <- max(y_pos)
  ncol <- max(x_pos)
  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z
  raster
}

xyz_to_isobands <- function(data, breaks) {
  isoband::isobands(x = sort(unique0(data$x)), y = sort(unique0(data$y)),
                    z = isoband_z_matrix(data), levels_low = breaks[-length(breaks)],
                    levels_high = breaks[-1])
}

xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(x = sort(unique0(data$x)), y = sort(unique0(data$y)),
                    z = isoband_z_matrix(data), levels = breaks)
}

iso_to_polygon <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))
  if (all(lengths == 0)) {
    warning("Zero contours were generated")
    return(tibble0())
  }
  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)
  groups <- paste(group, sprintf("%03d", item_id), sep = "-")
  groups <- factor(groups)
  tibble0(level = rep(levels, lengths), x = xs, y = ys,
              piece = as.integer(groups), group = groups, subgroup = ids,
              .size = length(xs))
}

iso_to_path <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))
  if (all(lengths == 0)) {
    warning("Zero contours were generated")
    return(tibble0())
  }
  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)
  groups <- paste(group, sprintf("%03d", item_id), sprintf("%03d",
                                                           ids), sep = "-")
  groups <- factor(groups)
  tibble0(level = rep(levels, lengths), x = xs, y = ys,
              piece = as.integer(groups), group = groups, .size = length(xs))
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    tibble0(group = 1, .size = 1)
  }
  else {
    data
  }
}
