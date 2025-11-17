\name{prepare_data}
\alias{prepare_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare the dataset
}
\description{
  Prepare the dataset to be used in deep learning uni-trait (DL_UT) or multi-trait (DL_MT)
}
\usage{
prepare_data(pheno, raw_data=NULL, relationship_matrix=NULL, environments = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pheno}{
  Dataframe with IDs, phenotypes, and environments (the titles should be: "ID", "phenotype\emph{1}", ...,"phenotype\emph{x}", "Env")
}
  \item{raw_data}{
  Raw data SNPs or Spectral information.
}
  \item{relationship_matrix}{
  Genomic (G) relationship matrix for the covariance between individuals due to a polygenic effect according to VanRaden (2008), or Spectral (H) relationship matrix according to Rincent et al. (2018).
}
  \item{environments}{
  Environment vector in "pheno" dataframe
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
VanRaden, P. M. (2008). Efficient methods to compute genomic predictions. Journal of dairy science, 91(11), 4414-4423.

Rincent, R., Charpentier, J. P., Faivre-Rampant, P., Paux, E., Le Gouis, J., Bastien, C., Segura, V. (2018). Phenomic selection is a low-cost and high-throughput method based on indirect predictions: proof of concept on wheat and poplar. G3: Genes, Genomes, Genetics, 8(12), 3961-3972.
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
######################
#Genomic information
######################
data(maize)

#Uni-Environment
pheno_UE=pheno[pheno$Env=="Cambira",]
x=prepare_data(pheno_UE,relationship_matrix=as.matrix(G))

#Multi-Environment
unique(pheno$Env)
environments=c("Cambira","Sabaudia")
pheno_ME=pheno[pheno$Env \%in\% environments,]
x=prepare_data(pheno_ME,raw_data=as.matrix(SNPs),environments=environments)

######################
#Spectral information
######################
data(Eucalyptus)

#Uni-Environment Relationship matrix
pheno_UE=pheno
x=prepare_data(pheno_UE,relationship_matrix=as.matrix(H))

#Uni-Environment Raw data
pheno_UE=pheno
x=prepare_data(pheno_UE,raw_data=as.matrix(NIR))



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
