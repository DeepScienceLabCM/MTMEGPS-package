#' @export
DL_MT <- function(x,y,hyp,xts=NULL,test=NULL){
  options(warn=-1)
  library(tensorflow)
  library(keras3)
  library(dplyr)
  library(future.apply)
  if(is.null(test)){
    
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
      
      units <- as.integer(round(ncol(x) * 0.7))
      
      Post_trn <- sample(1:nrow(x), round(nrow(x) * 0.8))
      X_tr <- x[Post_trn, ]
      X_ts <- x[-Post_trn, ]
      
      Mean_trn=apply(y[Post_trn,],2,mean)
      SD_trn=apply(y[Post_trn,],2,sd)
      
      y_tr=matrix(NA,ncol=dim(y)[2],nrow=dim(X_tr)[1])
      y_ts=matrix(NA,ncol=dim(y)[2],nrow=dim(X_ts)[1])
      
      for (t in 1:dim(y)[2]){
        y_tr[,t] =(y[Post_trn,t]- Mean_trn[t])/SD_trn[t]
        y_ts[,t] =(y[-Post_trn,t]- Mean_trn[t])/SD_trn[t]
      }

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
      
      yhat=NULL
      loss_weights2=NULL
      y_tr2=NULL
      for(out_n in 1:dim(y)[2]){
        yh <- base_model %>%
          layer_dense(units = 1, name=paste("yhat",out_n,sep=""))
        yhat=append(yhat,yh)
        loss_weights2=append(loss_weights2,loss_weights)
        y_tr2[[out_n]] = matrix(y_tr[, out_n], ncol = 1)
      }
      
      model <- keras3::keras_model(inputs = input, outputs = yhat)
      
      loss_list       <- setNames(rep(loss,          length(yhat)), names(yhat))
      metrics_list    <- setNames(rep(list(metric),  length(yhat)), names(yhat))
      loss_weights_ls <- setNames(as.numeric(loss_weights2),        names(yhat))
      
      model$compile(optimizer    = optimizer,
          loss         = loss_list,     
          metrics      = metrics_list,                      
          loss_weights = loss_weights_ls
      )
      
      X_tr <- keras3::k_cast_to_floatx(X_tr)
      
      model$fit(
        x = X_tr,
        y = y_tr2,
        epochs = as.integer(params$epochs),
        batch_size = as.integer(params$batch_size),
        verbose = 0,
        validation_split = 0.2,
        callbacks = list(callback_early_stopping(patience = 5, restore_best_weights = TRUE))
      )

      y_p <- model$predict(X_ts)
      y_p <- do.call(cbind, lapply(y_p, as.numeric))
      
      for (out_n in seq_len(ncol(y_p))) {
        y_p[ , out_n] <- as.numeric(y_p[ , out_n]) * SD_trn[out_n] + Mean_trn[out_n]
        y_ts[ , out_n] <- as.numeric(y_ts[ , out_n]) * SD_trn[out_n] + Mean_trn[out_n]
      }
      
      corr <- mean(diag(cor(y_ts, y_p)),na.rm=T)
      
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
    
    units <- as.integer(round(ncol(x) * 0.7))
      
    X_tr <- x
    X_ts <- xts
      
    Mean_trn=apply(y,2,mean)
    SD_trn=apply(y,2,sd)
      
    y_tr=matrix(NA,ncol=dim(y)[2],nrow=dim(X_tr)[1])
      
    for (t in 1:dim(y)[2]){
      y_tr[,t] =(y[,t]- Mean_trn[t])/SD_trn[t]
    }
    
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
      
      yhat=NULL
      loss_weights2=NULL
      y_tr2=NULL
      for(out_n in 1:dim(y)[2]){
        yh <- base_model %>%
          layer_dense(units = 1, name=paste("yhat",out_n,sep=""))
        yhat=append(yhat,yh)
        loss_weights2=append(loss_weights2,loss_weights)
        y_tr2[[out_n]] = matrix(y_tr[, out_n], ncol = 1)
      }
      
      model <- keras3::keras_model(inputs = input, outputs = yhat)
      
      loss_list       <- setNames(rep(loss,          length(yhat)), names(yhat))
      metrics_list    <- setNames(rep(list(metric),  length(yhat)), names(yhat))
      loss_weights_ls <- setNames(as.numeric(loss_weights2),        names(yhat))
      
      model$compile(optimizer    = optimizer,
                    loss         = loss_list,     
                    metrics      = metrics_list,                      
                    loss_weights = loss_weights_ls
      )
      
      X_tr <- keras3::k_cast_to_floatx(X_tr)
      
      model$fit(
        x = X_tr,
        y = y_tr2,
        epochs = as.integer(epochs),
        batch_size = as.integer(batch_size),
        verbose = 0,
        validation_split = 0.2,
        callbacks = list(callback_early_stopping(patience = 5, restore_best_weights = TRUE))
      )
      
      y_p <- model$predict(X_ts)
      y_p <- do.call(cbind, lapply(y_p, as.numeric))
      
      for (out_n in seq_len(ncol(y_p))) {
        y_p[ , out_n] <- as.numeric(y_p[, out_n]) * SD_trn[out_n] + Mean_trn[out_n]
      }
      
    return(y_p)
  }
}

