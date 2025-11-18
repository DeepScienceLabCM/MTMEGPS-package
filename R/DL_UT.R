#' @export
DL_UT <- function(xtr,xts=NULL,ytr,yts=NULL,hyp,test=NULL){
  options(warn=-1)
  library(tensorflow)
  library(keras3)
  library(dplyr)
  library(future.apply)
  if(is.null(test)){
    x_tr=xtr
    x_ts=xts
    param_grid <- expand.grid(
      activation = hyp[[1]],
      optimizer = hyp[[2]],
      epochs = hyp[[3]],
      batch_size = hyp[[4]],
      loss = hyp[[5]],
      metric = hyp[[6]],
      loss_weights = hyp[[7]],
      repetitions = c(1:hyp[[8]]),
      stringsAsFactors = FALSE
    )

    run_model <- function(params) {

      seed_val <- as.integer(sum(
        params$repetitions,
        utf8ToInt(params$activation),
        utf8ToInt(params$optimizer),
        params$epochs,
        params$batch_size
      )) %% .Machine$integer.max

      set.seed(seed_val)
      tensorflow::tf$random$set_seed(seed_val)

      params <- as.list(params)
      activation <- params$activation
      optimizer <- params$optimizer
      loss <- params$loss
      metric <- params$metric
      loss_weights <- params$loss_weights
      epochs <- params$epochs
      batch_size <- params$batch_size
      repetitions <- params$repetitions

      units <- as.integer(round(ncol(xtr) * 0.7))

      Mean_trn <- mean(ytr)
      SD_trn <- sd(ytr)
      y_tr <- (ytr - Mean_trn) / SD_trn
      y_ts <- (yts - Mean_trn) / SD_trn

      input <- layer_input(shape = ncol(x_tr))

      base_model <- input %>%
        layer_dense(units = units, activation = activation) %>%
        layer_dropout(rate = 0.3) %>%
        layer_dense(units = units, activation = activation) %>%
        layer_dropout(rate = 0.3) %>%
        layer_dense(units = units, activation = activation) %>%
        layer_dropout(rate = 0.3) %>%
        layer_dense(units = units, activation = activation) %>%
        layer_dropout(rate = 0.3)

      yhat <- base_model %>%
        layer_dense(units = 1, name = "yhat")

      model <- keras3::keras_model(inputs = input, outputs = yhat)

      model$compile(
        optimizer = optimizer,
        loss = loss,
        metrics = list(metric),
        loss_weights = as.numeric(loss_weights)
      )

      x_tr <- tensorflow::tf$cast(x_tr, dtype = tensorflow::tf$float32)
      y_tr <- tensorflow::tf$cast(y_tr, dtype = tensorflow::tf$float32)

      model$fit(
        x = x_tr,
        y = y_tr,
        epochs = as.integer(params$epochs),
        batch_size = as.integer(params$batch_size),
        verbose = 0,
        callbacks = list(callback_early_stopping(patience = 5, restore_best_weights = TRUE))
      )

      y_p <- as.vector(model$predict(x_ts))
      y_p <- y_p * SD_trn + Mean_trn
      y_ts_denorm <- y_ts * SD_trn + Mean_trn

      corr <- cor(y_ts_denorm, y_p)


      c(
        corr = corr,
        activation = activation,
        optimizer = optimizer,
        loss = loss,
        metric = metric,
        loss_weights = loss_weights,
        epochs = epochs,
        batch_size = batch_size,
        repetition = repetitions
      )
    }

    plan(multisession, workers = min(4, parallel::detectCores() - 1))
    results_list <- future_lapply(1:nrow(param_grid), function(i) run_model(param_grid[i, ]))
    plan(sequential)

    results_df <- as.data.frame(do.call(rbind, lapply(results_list, function(x) t(as.data.frame(x)))))
    num_cols <- c("corr", "loss_weights", "epochs", "batch_size", "repetition")
    results_df[num_cols] <- lapply(results_df[num_cols], as.numeric)

    return(results_df)
  }else{
    activation = hyp[[1]]
    optimizer = hyp[[2]]
    epochs = hyp[[3]]
    batch_size = hyp[[4]]
    loss = hyp[[5]]
    metric = hyp[[6]]
    loss_weights = hyp[[7]]
    repetitions = c(1:hyp[[8]])
    stringsAsFactors = FALSE

    units <- as.integer(round(ncol(xtr) * 0.7))

    X_tr <- xtr
    X_ts <- xts

    Mean_trn=mean(ytr,na.rm=T)
    SD_trn=sd(ytr,na.rm=T)

    y_tr <- (ytr - Mean_trn) / SD_trn

    input <- layer_input(shape = ncol(X_tr))

    base_model <- input %>%
      layer_dense(units = units, activation = activation) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = units, activation = activation) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = units, activation = activation) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = units, activation = activation) %>%
      layer_dropout(rate = 0.3)

    yhat <- base_model %>%
      layer_dense(units = 1, name = "yhat")

    model <- keras3::keras_model(inputs = input, outputs = yhat)

    model$compile(
      optimizer = optimizer,
      loss = loss,
      metrics = list(metric),
      loss_weights = as.numeric(loss_weights)
    )

    X_tr <- tensorflow::tf$cast(X_tr, dtype = tensorflow::tf$float32)
    y_tr <- tensorflow::tf$cast(y_tr, dtype = tensorflow::tf$float32)

    model$fit(
      x = X_tr,
      y = y_tr,
      epochs = as.integer(epochs),
      batch_size = as.integer(batch_size),
      verbose = 0,
      validation_split = 0.2,
      callbacks = list(callback_early_stopping(patience = 5, restore_best_weights = TRUE))
    )

    y_p <- as.vector(model$predict(X_ts))
    y_p <- y_p * SD_trn + Mean_trn

    return(y_p)
  }
}

