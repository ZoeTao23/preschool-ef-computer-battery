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

- **data/**: Data files and processing scripts
  - `Table_1_scale_characteristics.xlsx` - Scale descriptive statistics
  - `Table_2_demo_characteristics.xlsx` - Participant demographic data
  - `Table_3_task_characteristics.xlsx` - Task performance characteristics
  - `Table_4_original_game_and_scale_data.xlsx` - Original game and scale data
  - `Table_5_game_data.csv` - Raw game performance data

- **analysis/**: Main analysis scripts
  - `01_irt_models.R` - IRT model fitting and item analysis
  - `02_plots.R` - Normative scoring procedures and visualization
  
- **output/**: Analysis outputs
  - `tables/` - Generated tables for publication
  - `figures/` - Generated figures for publication

- **README.md**: Project documentation

## Quick Start
```r
source("analysis/irt.R")
```

### Prerequisites

- R (version ≥ 4.3.2)
- Required R packages:

```r
install.packages(c("openxlsx", "tidyverse", "compareGroups", "lavaan", 
                   "semTools", "blandr", "ggplot2", "gridExtra", "purrr", 
                   "ltm", "mice", "psych", "mirt", "scales"))
```
