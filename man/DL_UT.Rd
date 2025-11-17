\name{DL_UT}
\alias{DL_UT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
UniTrait-UniEnvironment and UniTrait-MultiEnvironment models for Genomic and Phenomic Prediction based on Deep Learning
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The DL_UT ("UniTrait-UniEnvironment and UniTrait-MultiEnvironment models for Genomic or Phenomic Prediction based on Deep Learning") function fits deep learning models to predict complex traits using either a relationship matrix (G or H) or raw data (SNPs or Spectral data), across single or multiple environments.
}
\usage{
DL_UT(x, y, hyp, xts = NULL, test = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Raw data or design matrix according to Montesinos-López (2018) and Montesinos-López (2019). This must be obtained with the prepare_data function.
  }
  \item{y}{
    phenotypes data-vector (numeric).
  }
  \item{hyp}{
    Hyperparameters list (see options of hyperparameters funtion in details).
  }
  \item{xts}{
    Raw data (SNPs or Spectral data) or a relationship matrix (Genomic or Phenomic) used to predict the phenotypic values of new samples. This option is used after building a model and want to predict phenotypes for previously unseen samples. By default, xts = NULL.
  }
  \item{test}{
    Set test=FALSE to test the hyperparameters and test=TRUE to predict new phenotypes. By default test=FALSE.
  }
}
\details{
  DL_UT implements a deep learning model that allows the search for the best hyperparameters combination and the prediction of new phenotypes in single or multiple environments.
To test the hyperparameters combination test=FALSE. See the hyperparameters() function to know the hyperparameters that can be tested.
The package automatically utilizes GPU resources. If you lack a GPU, it's not configured, or you prefer not to use it, we recommend the following statement: Sys.setenv("CUDA_VISIBLE_DEVICES" = "")
}
\value{
A matrix with the hyperparameters combination and its correspondent prediction accuracy value.
}
\references{
Montesinos-López, O. A., Montesinos-López, A., Crossa, J., Gianola, D., Hernández-Suárez, C. M., & Martín-Vallejo, J. (2018). Multi-trait, multi-environment deep learning modeling for genomic-enabled prediction of plant traits. G3: Genes, genomes, genetics, 8(12), 3829-3840.

Montesinos-López, O. A., Montesinos-López, A., Tuberosa, R., Maccaferri, M., Sciara, G., Ammar, K., & Crossa, J. (2019). Multi-trait, multi-environment genomic prediction of durum wheat with genomic best linear unbiased predictor and deep learning methods. Frontiers in Plant Science, 10, 1311.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
############################################
#Genomic information and Multi-Environment
############################################
data(Maize)

  unique(pheno$Env)
  environments=c("Cambira","Sabaudia")
  pheno_ME=pheno[pheno$Env \%in\% environments,]
  x=prepare_data(pheno_ME,as.matrix(G),environments=environments)
  y = pheno_ME[,2]
  head(y)

  #search best hyperparameters
  act=c("relu","sigmoid")
  opt=c("RMSprop","Adam")
  ep <- c(50, 100)
  bat <- c(100, 200)
  los <- c("mean_squared_error", "mean_absolute_error")
  met <- c("mean_squared_error")
  losw <- c(0.001, 0.01)
  rep=3
  hyp=hyperparameters(activations=act,optimizers=opt,epochs=ep,batchs=bat,losses=los,metrics=met,loss_weights=losw,repetitions=rep)

  results_hyperparameters_matrix=DL_UT(x,y,hyp)
  r_hyper=resume_hyperparameters(results_hyperparameters_matrix,rep)
  best_models=r_hyper[[8]]
  best_models

  #create model and predict unknown phenotypes
  act=c("tanh")
  opt=c("Adam")
  ep <- c(10)
  bat <- c(100)
  los <- c("mean_absolute_error")
  met <- c("mean_squared_error")
  losw <- c(0.001)
  rep=1
  hyp=hyperparameters(activations=act,optimizers=opt,epochs=ep,batchs=bat,losses=los,metrics=met,loss_weights=losw,repetitions=rep)

  set.seed(1234)
  Post_trn <- sample(1:nrow(x), round(nrow(x) * 0.8))
  xtr = x[Post_trn,]
  xts = x[-Post_trn,]
  ytr = y[Post_trn]
  yts = y[-Post_trn]
  yhat=DL_UT(xtr,ytr,hyp,xts,test=TRUE)

  #Prediciton accuracy and plot of observed and predicted values
  cor(yhat,yts)
  plot(yhat, yts)


############################################
#Spectral information and Uni-Environment
############################################
data(Eucalyptus)

  pheno_UE=pheno
  x=prepare_data(pheno_UE,as.matrix(NIR))
  y = pheno_UE[,2]
  head(y)

  #search best hyperparameters
  act=c("relu","sigmoid")
  opt=c("RMSprop","Adam")
  ep <- c(50, 100)
  bat <- c(100, 200)
  los <- c("mean_squared_error", "mean_absolute_error")
  met <- c("mean_squared_error")
  losw <- c(0.001, 0.01)
  rep=3
  hyp=hyperparameters(activations=act,optimizers=opt,epochs=ep,batchs=bat,losses=los,metrics=met,loss_weights=losw,repetitions=rep)

  results_hyperparameters_matrix=DL_UT(x,y,hyp)
  r_hyper=resume_hyperparameters(results_hyperparameters_matrix,rep)
  best_models=r_hyper[[8]]
  best_models

  #create model and predict unknown phenotypes
  act=c("tanh")
  opt=c("Adam")
  ep <- c(10)
  bat <- c(100)
  los <- c("mean_absolute_error")
  met <- c("mean_squared_error")
  losw <- c(0.001)
  rep=1
  hyp=hyperparameters(activations=act,optimizers=opt,epochs=ep,batchs=bat,losses=los,metrics=met,loss_weights=losw,repetitions=rep)

  set.seed(1234)
  Post_trn <- sample(1:nrow(x), round(nrow(x) * 0.8))
  xtr = x[Post_trn,]
  xts = x[-Post_trn,]
  ytr = y[Post_trn]
  yts = y[-Post_trn]
  yhat=DL_UT(xtr,ytr,hyp,xts,test=TRUE)

  #Prediciton accuracy and plot of observed and predicted values
  cor(yhat,yts)
  plot(yhat, yts)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
