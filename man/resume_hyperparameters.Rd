\name{resume_hyperparameters}
\alias{resume_hyperparameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs a comparison analysis for hyperparameters
}
\description{
  Performs an ANOVA analysis for each hyperparameter and for the hyperparameters concatenated.
}
\usage{
resume_hyperparameters(results, reps, MT = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
  Results matrix of DL_UT or DL_MT functions
}
  \item{reps}{
  Number of repetitions considered in DL_UT or D_MT functions
}
  \item{MT}{
  Number of traits considered in Multi-Trait analysis (only for results get from DL_MT function)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with mean, standard error, and letter of a Tukey analysis (based on 0.05 significance) are shown by each hyperparameter individually. The eighth value corresponds to analysis based on the hyperparameters concatenated. Moreover, a plot of independent hyperparameters is generated.
}
\references{
%% ~put references to the literature/web site here ~
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
  data(maize)

  pheno2=pheno_maize[pheno_maize$Env=="Cambira",]
  x=prepare_data(pheno2,as.matrix(G_matrix_maize))
  y = pheno2[,2]
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
