\name{hyperparameters}
\alias{hyperparameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a list with hyperparameters
}
\description{
  Create a list with hyperparameters
}
\usage{
hyperparameters(activations = NULL, optimizers = NULL, epochs = NULL, batchs = NULL, losses = NULL, metrics = NULL, loss_weights = NULL, repetitions = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{activations}{
  c("elu", "exponential", "gelu", "linear", "mish", "relu", "selu", "sigmoid", "softmax", "softplus", "softsign", "swish", "tanh") (by default activations="relu")
}
  \item{optimizers}{
  c("SGD", "RMSprop", "Adam", "AdamW", "Adadelta", "Adagrad", "Adamax", "Nadam") (by default optimizers="Adam")
}
  \item{epochs}{
  c(e.g.: 10, 50, 100, other values) (by default epochs=100)
}
  \item{batchs}{
  c(e.g.: 10, 50, 100, other values) (by default batchs=100)
}
  \item{losses}{
  c("mean_squared_error",  "mean_absolute_error", "mean_absolute_percentage_error", "mean_squared_logarithmic_error") (by default losses="mean_squared_error")
}
  \item{metrics}{
  c("mean_squared_error", "mean_absolute_error", "mean_absolute_percentage_error", "mean_squared_logarithmic_error") (by default metrics="mean_squared_error")
}
  \item{loss_weights}{
  c(e.g.: 0.001, 0.01, 0.1, other values) (by default loss_weights=0.001)
}
  \item{repetitions}{
  by default repetitions=3
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  act=c("relu","sigmoid")
  opt=c("RMSprop","Adam")
  ep <- c(100)
  bat <- c(100, 250)
  los <- c("mean_squared_error", "mean_absolute_error")
  met <- c("mean_squared_error")
  losw <- c(0.001, 0.01)
  rep=3
  hyp=hyperparameters(activations=act,
                      optimizers=opt,
                      epochs=ep,
                      batchs=bat,
                      losses=los,
                      metrics=met,
                      loss_weights=losw,
                      repetitions=rep)
  hyp

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
