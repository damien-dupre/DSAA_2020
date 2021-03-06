################################################################################
geocode_batch <- function(address){
  # for osm geocoding ----------------------------------------------------------
  res <-
    jsonlite::fromJSON(
      gsub(
        "\\@addr\\@",
        gsub("\\s+", "\\%20", address),
        'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1'
      )
    )
  # for pickpoint geocoding ----------------------------------------------------
  # res <- 
  #   prettymapr::geocode(
  #     location = address, 
  #     source = "pickpoint", 
  #     key = config::get("pickpoint")$key
  #   )
  if (length(res) > 0) {
    dplyr::select(res, display_name, lat, lon)
  } else {
    data.frame(display_name = NA, lat = NA, lon = NA)
  }
}

################################################################################
gam_density <- function (x, view = NULL, cond = list(), n.grid = 40, too.far = 0, 
                    col = NA, color = "heat", contour.col = NULL, se = -1, type = "link", 
                    plot.type = "persp", zlim = NULL, nCol = 50, ...) {
  fac.seq <- function(fac, n.grid) {
    fn <- length(levels(fac))
    gn <- n.grid
    if (fn > gn) 
      mf <- factor(levels(fac))[1:gn]
    else {
      ln <- floor(gn/fn)
      mf <- rep(levels(fac)[fn], gn)
      mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
      mf <- factor(mf, levels = levels(fac))
    }
    mf
  }
  dnm <- names(list(...))
  v.names <- names(x$var.summary)
  if (is.null(view)) {
    k <- 0
    view <- rep("", 2)
    for (i in 1:length(v.names)) {
      ok <- TRUE
      if (is.matrix(x$var.summary[[i]])) 
        ok <- FALSE
      else if (is.factor(x$var.summary[[i]])) {
        if (length(levels(x$var.summary[[i]])) <= 1) 
          ok <- FALSE
      }
      else {
        if (length(unique(x$var.summary[[i]])) == 1) 
          ok <- FALSE
      }
      if (ok) {
        k <- k + 1
        view[k] <- v.names[i]
      }
      if (k == 2) 
        break
    }
    if (k < 2) 
      stop("Model does not seem to have enough terms to do anything useful")
  }
  ok <- TRUE
  for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
    if (length(levels(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  else {
    if (length(unique(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  if (!ok) 
    stop(gettextf("View variables must contain more than one value. view = c(%s,%s).", 
                  view[1], view[2]))
  if (is.factor(x$var.summary[[view[1]]])) 
    m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
  else {
    r1 <- range(x$var.summary[[view[1]]])
    m1 <- seq(r1[1], r1[2], length = n.grid)
  }
  if (is.factor(x$var.summary[[view[2]]])) 
    m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
  else {
    r2 <- range(x$var.summary[[view[2]]])
    m2 <- seq(r2[1], r2[2], length = n.grid)
  }
  v1 <- rep(m1, n.grid)
  v2 <- rep(m2, rep(n.grid, n.grid))
  newd <- data.frame(matrix(0, n.grid * n.grid, 0))
  for (i in 1:length(x$var.summary)) {
    ma <- cond[[v.names[i]]]
    if (is.null(ma)) {
      ma <- x$var.summary[[i]]
      if (is.numeric(ma)) 
        ma <- ma[2]
    }
    if (is.matrix(x$var.summary[[i]])) 
      newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                          byrow = TRUE)
    else newd[[i]] <- rep(ma, n.grid * n.grid)
  }
  names(newd) <- v.names
  newd[[view[1]]] <- v1
  newd[[view[2]]] <- v2
  fv <- predict.gam(x, newdata = newd, se.fit = TRUE, type = type)
  z <- fv$fit
  if (too.far > 0) {
    ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
                             x$model[, view[2]], dist = too.far)
    fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
  }
  if (is.factor(m1)) {
    m1 <- as.numeric(m1)
    m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
  }
  if (is.factor(m2)) {
    m2 <- as.numeric(m2)
    m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
  }
  if (se <= 0) {
    old.warn <- options(warn = -1)
    av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid,
                 n.grid - 1)
    options(old.warn)
    max.z <- max(z, na.rm = TRUE)
    z[is.na(z)] <- max.z * 10000
    z <- matrix(z, n.grid, n.grid)
    surf.col <- t(av) %*% z %*% av
    surf.col[surf.col > max.z * 2] <- NA
    if (!is.null(zlim)) {
      if (length(zlim) != 2 || zlim[1] >= zlim[2])
        stop("Something wrong with zlim")
      min.z <- zlim[1]
      max.z <- zlim[2]
    }
    else {
      min.z <- min(fv$fit, na.rm = TRUE)
      max.z <- max(fv$fit, na.rm = TRUE)
    }
    z <- matrix(fv$fit, n.grid, n.grid)
  }
  dat <- as.data.frame(z)
  rownames(dat) <- m1
  colnames(dat) <- m2
  dat %>%
    tibble::rownames_to_column("lng") %>%
    tidyr::gather(lat, value, -lng) %>%
    dplyr::mutate(
      lng = as.numeric(lng),
      lat = as.numeric(lat)
    )
}
################################################################################
xgboost_all_parameters <- function(booster_parameter, objective_parameter){
  if (booster_parameter == "gbtree") {
    xgboost::xgboost(
# https://xgboost.readthedocs.io/en/latest/parameter.html ----------------------
      data = train_tbl_x,
      label = train_tbl_y,
# Parameters for Tree Booster --------------------------------------------------
      booster = booster_parameter,
      learning_rate = 0.3,
      min_split_loss = 0,
      max_depth = 6,
      min_child_weight = 1,
      max_delta_step = 0,
      subsample = 1,
      sampling_method = "uniform",
      colsample_bytree = 1,
      colsample_bylevel = 1,
      colsample_bynode = 1,
      reg_lambda = 1,
      reg_alpha = 0,
      tree_method = "auto", # Choices: auto, exact, approx, hist, gpu_hist
      #scale_pos_weight = 1,
      refresh_leaf = 1,
      process_type = "default", # Choices: default, update
      grow_policy = "depthwise", # Choices: depthwise, lossguide
      max_leaves = 0,
# Learning Task Parameters -----------------------------------------------------
      objective = objective_parameter, 
      # Choices (reg only): squarederror, squaredlogerror, gamma, tweedie
      eval_metric = "rmse",
      # Choices: rmse, rmsle, mae, mphe, logloss, error, error@t, merror, ...
      base_score = 0.5,
      verbose = 0,
      nround = 100
    ) %>% 
      predict(test_tbl_x) %>% 
      tibble::enframe(name = NULL, value = "pred") %>% 
      dplyr::mutate(price = as.data.frame(test_tbl_y)$price) %>% 
      yardstick::metrics(truth = price, estimate = pred) %>% 
      select(-.estimator) %>% 
      mutate(
        .estimate = round(.estimate, 2),
        booster = booster_parameter,
        objective = objective_parameter
      )
  } else if (booster_parameter == "gblinear") {
    xgboost::xgboost(
# https://xgboost.readthedocs.io/en/latest/parameter.html ----------------------
      data = train_tbl_x,
      label = train_tbl_y,
# Parameters for Linear Booster ------------------------------------------------
      booster = "gblinear",
      reg_lambda = 0,
      reg_alpha = 0,
      updater = "shotgun", # Choices: shotgun, coord_descent
      feature_selector = "cyclic", # Choices: cyclic, shuffle
# Learning Task Parameters -----------------------------------------------------
      objective = objective_parameter, 
      # Choices (reg only): squarederror, squaredlogerror, gamma, tweedie
      eval_metric = "rmse",
      # Choices: rmse, rmsle, mae, mphe, logloss, error, error@t, merror, ...
      base_score = 0.5,
      verbose = 0,
      nround = 100
    ) %>% 
      predict(test_tbl_x) %>% 
      tibble::enframe(name = NULL, value = "pred") %>% 
      dplyr::mutate(price = as.data.frame(test_tbl_y)$price) %>% 
      yardstick::metrics(truth = price, estimate = pred) %>% 
      select(-.estimator) %>% 
      mutate(
        .estimate = round(.estimate, 2),
        booster = booster_parameter,
        objective = objective_parameter
      )
  } else next
  
}

