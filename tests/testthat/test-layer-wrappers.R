test_that("wrapper functions for `layer()` are passing arguments on as expected", {

  df <- readRDS(test_path("fixtures", "df_norm.rds"))

  check_layer <- function(layer_fun, Geom, Stat, mapping = aes(x, y), data = df, ...) {

    hdr_layer <- layer_fun(data = data, mapping = mapping, ...)

    expect_type(hdr_layer, "environment")
    expect_identical(hdr_layer$geom, Geom)
    expect_identical(hdr_layer$stat, Stat)
    expect_identical(hdr_layer$mapping, mapping)

  }

  # 2-d layer functions -----------------------------------------------------

  # geom/stat_hdr()
  check_layer(geom_hdr, GeomHdr, StatHdr)
  check_layer(stat_hdr, GeomHdr, StatHdr)

  # geom/stat_hdr_lines()
  check_layer(geom_hdr_lines, GeomHdrLines, StatHdrLines)
  check_layer(stat_hdr_lines, GeomHdrLines, StatHdrLines)

  # geom/stat_hdr_points()
  check_layer(geom_hdr_points, GeomPoint, StatHdrPoints)
  check_layer(stat_hdr_points, GeomPoint, StatHdrPoints)

  # geom/stat_hdr_lines_fun()
  # -- stat_hdr_points_fun needs to have a `fun` arg provided
  check_layer(geom_hdr_points_fun, GeomPoint, StatHdrPointsFun)
  check_layer(stat_hdr_points_fun, GeomPoint, StatHdrPointsFun, fun = function(x, y) dnorm(x) * dnorm(y))

  # geom/stat_hdr_fun()
  # (stat_hdr_fun needs to have a `fun` arg provided)
  check_layer(geom_hdr_fun, GeomHdrFun, StatHdrFun)
  check_layer(stat_hdr_fun, GeomHdrFun, StatHdrFun, fun = function(x, y) dnorm(x) * dnorm(y))

  # -- checking that data doesn't need to be provided
  check_layer(geom_hdr_fun, GeomHdrFun, StatHdrFun, data = NULL, mapping = NULL)
  check_layer(stat_hdr_fun, GeomHdrFun, StatHdrFun, data = NULL, mapping = NULL, fun = function(x, y) dnorm(x) * dnorm(y))

  # geom/stat_hdr_lines_fun()
  # -- stat_hdr_lines_fun needs to have a `fun` arg provided
  check_layer(geom_hdr_lines_fun, GeomHdrLinesFun, StatHdrLinesFun)
  check_layer(stat_hdr_lines_fun, GeomHdrLinesFun, StatHdrLinesFun, fun = function(x, y) dnorm(x) * dnorm(y))

  # -- checking that data doesn't need to be provided
  check_layer(geom_hdr_lines_fun, GeomHdrLinesFun, StatHdrLinesFun, data = NULL, mapping = NULL)
  check_layer(stat_hdr_lines_fun, GeomHdrLinesFun, StatHdrLinesFun, data = NULL, mapping = NULL, fun = function(x, y) dnorm(x) * dnorm(y))

  # 1-d layer functions -----------------------------------------------------

  # geom/stat_hdr_rug()
  check_layer(geom_hdr_rug, GeomHdrRug, StatHdrRug)
  check_layer(stat_hdr_rug, GeomHdrRug, StatHdrRug)

  # -- checking that single x/y aesthetics are allowed:
  check_layer(geom_hdr_rug, GeomHdrRug, StatHdrRug, mapping  = aes(x))
  check_layer(stat_hdr_rug, GeomHdrRug, StatHdrRug, mapping  = aes(x))
  check_layer(geom_hdr_rug, GeomHdrRug, StatHdrRug, mapping  = aes(y))
  check_layer(stat_hdr_rug, GeomHdrRug, StatHdrRug, mapping  = aes(y))

  # geom/stat_hdr_rug_fun()
  check_layer(geom_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun)
  check_layer(stat_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun)

  # -- checking that single x/y aesthetics are allowed:
  check_layer(geom_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun, mapping  = aes(x))
  check_layer(stat_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun, mapping  = aes(x))
  check_layer(geom_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun, mapping  = aes(y))
  check_layer(stat_hdr_rug_fun, GeomHdrRugFun, StatHdrRugFun, mapping  = aes(y))

})


