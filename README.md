# Computer-Based Executive Function Battery for Chinese Preschool Children

This repository contains the analysis code for the study:

**"Development and Validation of a Computer-Based Battery for Assessing Executive Function of Preschool Children in China: Score Development, Reliability, and Validity"**

## Project Overview

This project provides the complete analytical pipeline for developing and validating a computer-based executive function (EF) assessment battery for preschool children in China. The repository includes code for:

- **IRT model development** and parameter estimation
- **Reliability analysis** (internal consistency, test-retest)
- **Validity evidence** (construct, convergent, discriminant)
- **Measurement invariance** testing across demographic groups
- **Score development** and normative procedures

## Repository Structure
├── data/ # Data processing scripts
├── analysis/
│ ├── 01_irt_models.R # IRT model fitting and item analysis
│ ├── 02_reliability.R # Reliability analyses
│ ├── 03_validity.R # Validity evidence analyses
│ ├── 04_measurement_invariance.R # Measurement invariance testing
│ └── 05_score_development.R # Normative scoring procedures
├── functions/
│ ├── irt_estimation.R # Custom IRT functions
│ └── utility_functions.R # Helper functions
├── output/
│ ├── tables/ # Analysis tables for publication
│ └── figures/ # Figures for publication
├── docs/ # Supplementary documentation
└── README.md

## Quick Start
```r
source("analysis/irt.R")

### Prerequisites

- R (version ≥ 4.1.0)
- Required R packages:

```r
install.packages(c("mirt", "lavaan", "psych", "tidyverse", 
                   "ggplot2", "knitr", "rmarkdown"))
