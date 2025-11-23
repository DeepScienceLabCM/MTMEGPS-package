# MTMEGPS

**MTMEGPS** is an R package for deep learning–based genomic and phenomic prediction in multi-trait and multi-environment breeding studies.  
It provides functions for Uni-Trait (UT), Multi-Trait (MT), Uni-Environment (UE), and Multi-Environment (ME) prediction models using TensorFlow and keras3 through the reticulate interface.

---

## Installation

If you're using Windows, you must install **RTools** before installing
**DevTools**, since many R packages need compilation tools. Download RTools here:\
https://cran.r-project.org/bin/windows/Rtools/

```r
install.packages("devtools")
devtools::install_github("DeepScienceLabCM/MTMEGPS-package")
```

---

## Python Requirements

MTMEGPS requires a working Python installation (version 3.9–3.11).  
Before using the package. Recommended version: **Python 3.10**
If you plan to install Python through `reticulate::install_python()`,
Git must be installed first. Download Git here:\
https://git-scm.com/downloads

```r
reticulate::install_python(version = "3.10")
```

After Python is available, install the necessary deep learning dependencies:

```r
library(MTMEGPS)
install_mtmegps_dependencies()
```

This will install TensorFlow, keras3, and all required Python modules.

---

## Main Functions

### `DL_UT()`
Deep learning model for **uni-trait** prediction.

### `DL_MT()`
Deep learning model for **multi-trait** prediction.

### `prepare_data()`
Prepares genomic, phenomic, trait, and environment data for modeling.

### `hyperparameters()`
Generates default hyperparameters for model training.

### `resume_hyperparameters()`
Loads or reuses previous hyperparameter configurations.

### `install_mtmegps_dependencies()`
Installs Python dependencies required by TensorFlow and keras3.

---

## Contact

For issues or suggestions, please open an issue on GitHub.
