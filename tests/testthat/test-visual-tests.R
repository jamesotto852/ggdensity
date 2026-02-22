## Checking basic plots with vdiffr::expect_doppelganger()

test_that("Basic 2d HDRs render consistently", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  # geom/stat_hdr
  geom_hdr_ggplot <- ggplot(data, aes(x, y)) + geom_hdr()
  stat_hdr_ggplot <- ggplot(data, aes(x, y)) + stat_hdr()
  vdiffr::expect_doppelganger("geom-hdr-ggplot", geom_hdr_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-ggplot", stat_hdr_ggplot)

  # geom/stat_hdr_lines
  geom_hdr_lines_ggplot <- ggplot(data, aes(x, y)) + geom_hdr_lines()
  stat_hdr_lines_ggplot <- ggplot(data, aes(x, y)) + stat_hdr_lines()
  vdiffr::expect_doppelganger("geom-hdr_lines-ggplot", geom_hdr_lines_ggplot)
  vdiffr::expect_doppelganger("stat-hdr_lines-ggplot", stat_hdr_lines_ggplot)

  # geom/stat_hdr_points
  geom_hdr_points_ggplot <- ggplot(data, aes(x, y)) + geom_hdr_points()
  stat_hdr_points_ggplot <- ggplot(data, aes(x, y)) + stat_hdr_points()
  vdiffr::expect_doppelganger("geom-hdr-points-ggplot", geom_hdr_points_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-points-ggplot", stat_hdr_points_ggplot)

  # geom/stat_hdr_points_fun
  geom_hdr_points_fun_ggplot <- ggplot(data, aes(x, y)) + geom_hdr_points_fun(fun = function(x, y) dnorm(x) * dnorm(y))
  stat_hdr_points_fun_ggplot <- ggplot(data, aes(x, y)) + stat_hdr_points_fun(fun = function(x, y) dnorm(x) * dnorm(y))
  vdiffr::expect_doppelganger("geom-hdr-points-fun-ggplot", geom_hdr_points_fun_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-points-fun-ggplot", stat_hdr_points_fun_ggplot)

  # geom/stat_hdr_fun
  geom_hdr_fun_ggplot <- ggplot() +
    geom_hdr_fun(fun = function(x, y) dnorm(x) * dnorm(y), xlim = c(-5, 5), ylim = c(-5, 5))
  stat_hdr_fun_ggplot <- ggplot() +
    stat_hdr_fun(fun = function(x, y) dnorm(x) * dnorm(y), xlim = c(-5, 5), ylim = c(-5, 5))
  vdiffr::expect_doppelganger("geom-hdr-fun-ggplot", geom_hdr_fun_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-fun-ggplot", stat_hdr_fun_ggplot)

})

test_that("Basic 1d HDRs render consistently", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  # geom/stat_hdr_rug
  geom_hdr_rug_ggplot <- ggplot(data, aes(x, y)) + geom_hdr_rug()
  stat_hdr_rug_ggplot <- ggplot(data, aes(x, y)) + stat_hdr_rug()
  vdiffr::expect_doppelganger("geom-hdr-rug-ggplot", geom_hdr_rug_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-rug-ggplot", stat_hdr_rug_ggplot)

  # geom/stat_hdr_rug_fun
  geom_hdr_rug_fun_ggplot <- ggplot() +
    geom_hdr_rug_fun(fun_x = dnorm, fun_y = dexp, xlim = c(-5, 5), ylim = c(0, 10))
  stat_hdr_rug_fun_ggplot <- ggplot() +
    stat_hdr_rug_fun(fun_x = dnorm, fun_y = dexp, xlim = c(-5, 5), ylim = c(0, 10))

  vdiffr::expect_doppelganger("geom-hdr-rug-fun-ggplot", geom_hdr_rug_fun_ggplot)
  vdiffr::expect_doppelganger("stat-hdr-rug-fun-ggplot", stat_hdr_rug_fun_ggplot)

})

test_that("Specified order of probabilities doesn't impact legend ordering", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  geom_hdr_prob_order_ggplot <- ggplot(data, aes(x, y)) +
    geom_hdr(probs = c(.25, .5, .75, .95))

  geom_hdr_rug_prob_order_ggplot <- ggplot(data, aes(x, y)) +
    geom_hdr_rug(probs = c(.25, .5, .75, .95))

  vdiffr::expect_doppelganger("geom_hdr_prob_order_ggplot", geom_hdr_prob_order_ggplot)
  vdiffr::expect_doppelganger("geom_hdr_rug_prob_order_ggplot", geom_hdr_rug_prob_order_ggplot)
})



