#' @export
prepare_data <- function(pheno,raw_data=NULL,relationship_matrix=NULL,environments=NULL){
  if(!is.null(relationship_matrix) && !is.null(pheno)){
    LG=t(chol(relationship_matrix))
    if(length(environments)==1 || is.null(environments)){
      Z1=model.matrix(~0+as.factor(pheno$ID))
      Z1G=Z1%*%LG
      return(cbind(Z1G))
    }else{
      p=NULL
      for(env in environments){
        p=rbind(p,pheno[pheno$Env==env,])
      }
      Z1=model.matrix(~0+as.factor(p$ID))
      ZE=model.matrix(~0+as.factor(p$Env))
      Z1G=Z1%*%LG
      Z2GE=model.matrix(~0+as.factor(p$ID):as.factor(p$Env))
      G=data.matrix(relationship_matrix)
      G2=kronecker(diag(length(environments)),G)
      LG2=t(chol(G2))
      Z2GE=Z2GE%*%LG2
      return(cbind(ZE,Z1G,Z2GE))
    }
  }
  if(!is.null(raw_data) && !is.null(pheno)){
    LG=raw_data
    if(length(environments)==1 || is.null(environments)){
      Z1=model.matrix(~0+as.factor(pheno$ID))
      Z1G=Z1%*%LG
      return(cbind(Z1G))
    }else{
      p=NULL
      for(env in environments){
        p=rbind(p,pheno[pheno$Env==env,])
      }
      Z1=model.matrix(~0+as.factor(p$ID))
      ZE=model.matrix(~0+as.factor(p$Env))
      colnames(ZE)=environments
      Z1G=Z1%*%LG
      Z2GE=model.matrix(~0+as.factor(p$ID):as.factor(p$Env))
      G=data.matrix(raw_data)
      G2=kronecker(diag(length(environments)),G)
      LG2=G2
      Z2GE=Z2GE%*%LG2
      return(cbind(ZE,Z1G,Z2GE))
    }
  }
}
