#' @export
resume_hyperparameters <- function(results,reps,MT=NULL){
  library(agricolae)
  library(ggplot2)
  library(cowplot)

  plots <- function(df){
	y_h=max(df$Accuracy)*0.1
	ggplot(df, aes(row.names(df), Accuracy)) +
    geom_bar(stat = "identity", width=0.8, alpha=0.8, fill = "steelblue") +
    geom_errorbar(aes(ymin=Accuracy-SE, ymax=Accuracy+SE), width = 0.2, color = "steelblue4") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(x = colnames(df)[4])+
    geom_text(aes(label=groups), nudge_x = 0.15, nudge_y = y_h, size = 5, color = "steelblue4")
  }

  anova_split <- function(results, reps, MT = NULL, max_depth = 5, depth = 1) {
    if (depth > max_depth) {
      return(results)
    }
    x  <- seq_len(nrow(results))
    y  <- x[nrow(results) %% x == 0]

    if (is.null(MT)) {
      z <- y[y %% reps == 0 & y < 200]
      max_rows <- reps * 10
    }else{
      z <- y[y %% (reps * MT) == 0 & y < 200]
      max_rows <- reps * MT * 10
    }

    z <- z[length(z)]
    index <- 1
    r_anova <- NULL

    for (i in 1:(nrow(results) / z)) {
      block <- results[index:(index + z - 1), ]
      fit <- aov(result ~ concatenate, data = block)
      groups <- HSD.test(fit, "concatenate")
      top10 <- groups$groups[1:min(10, nrow(groups$groups)), ]
      r_anova <- rbind(r_anova, top10)
      index <- index + z
    }

    r_result <- results[results$concatenate %in% rownames(r_anova), ]
    
    if (nrow(r_result) <= max_rows) {
      return(r_result)
    }
    if (nrow(r_result) == nrow(results)) {
      return(r_result)
    }

    anova_split(r_result, reps, MT, max_depth, depth + 1)
  }

  results=as.data.frame(results)
  results$result=as.numeric(results$corr)
  results$concatenate=paste(results$activation,"-",results$optimizer,"-",results$epoch,"-",results$batch_size,"-",results$loss,"-",results$metric,"-",results$loss_weights,sep="")
  if(length(unique(results$activation))==1){
    r_activations=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_activations)=results$activations[1]
  }else{
    anova=aov(result~activation,data=results[,c("activation","result")])
    groups=HSD.test(anova,"activation")
    r_activations=groups$groups
  }
  for(x in rownames(r_activations)){
    r_activations[x,"SE"]=sd(results[results$activation==x,"result"],na.rm=T)/sqrt(length(results[results$activation==x,"result"]))
    r_activations[x,"Activation Function"]=x
  }
  colnames(r_activations)[1]="Accuracy"
  p1=plots(r_activations)

  if(length(unique(results$optimizer))==1){
    r_optimizers=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_optimizers)=results$optimizers[1]
  }else{
    anova=aov(result~optimizer,data=results[,c("optimizer","result")])
    groups=HSD.test(anova,"optimizer")
    r_optimizers=groups$groups
  }
  for(x in rownames(r_optimizers)){
    r_optimizers[x,"SE"]=sd(results[results$optimizer==x,"result"],na.rm=T)/sqrt(length(results[results$optimizer==x,"result"]))
    r_optimizers[x,"Optimizer"]=x
  }
  colnames(r_optimizers)[1]="Accuracy"
  p2=plots(r_optimizers)

  if(length(unique(results$epoch))==1){
    r_epochs=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_epochs)=results$epochs[1]
  }else{
    anova=aov(result~epochs,data=results[,c("epochs","result")])
    groups=HSD.test(anova,"epochs")
    r_epochs=groups$groups
  }
  for(x in rownames(r_epochs)){
    r_epochs[x,"SE"]=sd(results[results$epochs==x,"result"],na.rm=T)/sqrt(length(results[results$epochs==x,"result"]))
    r_epochs[x,"Epoch"]=x
  }
  colnames(r_epochs)[1]="Accuracy"
  p3=plots(r_epochs)

  if(length(unique(results$batch_size))==1){
    r_batchs=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_batchs)=results$batchs[1]
  }else{
    anova=aov(result~batch_size ,data=results[,c("batch_size","result")])
    groups=HSD.test(anova,"batch_size")
    r_batchs=groups$groups
  }
  for(x in rownames(r_batchs)){
    r_batchs[x,"SE"]=sd(results[results$batch_size==x,"result"],na.rm=T)/sqrt(length(results[results$batch_size==x,"result"]))
    r_batchs[x,"Batch"]=x
  }
  colnames(r_batchs)[1]="Accuracy"
  p4=plots(r_batchs)

  if(length(unique(results$loss))==1){
    r_losses=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_losses)=results$losses[1]
  }else{
    anova=aov(result~loss,data=results[,c("loss","result")])
    groups=HSD.test(anova,"loss")
    r_losses=groups$groups
  }
  for(x in rownames(r_losses)){
    r_losses[x,"SE"]=sd(results[results$loss==x,"result"],na.rm=T)/sqrt(length(results[results$loss==x,"result"]))
    r_losses[x,"Loss"]=x
  }
  colnames(r_losses)[1]="Accuracy"
  p5=plots(r_losses)

  if(length(unique(results$metric))==1){
    r_metrics=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_metrics)=results$metrics[1]
  }else{
    anova=aov(result~metric,data=results[,c("metric","result")])
    groups=HSD.test(anova,"metric")
    r_metrics=groups$groups
  }
  for(x in rownames(r_metrics)){
    r_metrics[x,"SE"]=sd(results[results$metric==x,"result"],na.rm=T)/sqrt(length(results[results$metric==x,"result"]))
    r_metrics[x,"Metric"]=x
  }
  colnames(r_metrics)[1]="Accuracy"
  p6=plots(r_metrics)

  if(length(unique(results$loss_weights))==1){
    r_loss_weights=data.frame("result"=mean(results[,"result"],na.rm=T),"groups"="a")
    rownames(r_loss_weights)=results$loss_weights[1]
  }else{
    anova=aov(result~loss_weights,data=results[,c("loss_weights","result")])
    groups=HSD.test(anova,"loss_weights")
    r_loss_weights=groups$groups
  }
  for(x in rownames(r_loss_weights)){
    r_loss_weights[x,"SE"]=sd(results[results$loss_weights==x,"result"],na.rm=T)/sqrt(length(results[results$loss_weights==x,"result"]))
    r_loss_weights[x,"Loss Weight"]=x
  }
  colnames(r_loss_weights)[1]="Accuracy"
  p7=plots(r_loss_weights)

  if(is.null(MT)){
    r_results=anova_split(results,reps)
  }else{
    r_results=anova_split(results,reps,MT)
  }
  anova=aov(result~concatenate,data=r_results[,c("concatenate","result")])
  groups=HSD.test(anova,"concatenate")
  r_concatenate=groups$groups
  for(x in rownames(r_concatenate)){
    r_concatenate[x,"SE"]=sd(r_results[r_results$concatenate==x,"result"],na.rm=T)/sqrt(length(r_results[r_results$concatenate==x,"result"]))
  }
  r_concatenate$activations_optimizers_epochs_batchs_losses_metrics_loss_weights=rownames(r_concatenate)
  colnames(r_concatenate)[1]="Accuracy"
  if(nrow(r_concatenate)<10){
    rownames(r_concatenate)=1:nrow(r_concatenate)
  }else{
    rownames(r_concatenate)=1:10
  }

  png("r_MTMEGPS.png", width = 7, height = 7, units = "in", res = 700)
    print(plot_grid(p1,p2,p3,p4,p5,p6,p7))
  dev.off()
  return(list(r_activations,r_optimizers,r_epochs,r_batchs,r_losses,r_metrics,r_loss_weights,r_concatenate))
}
