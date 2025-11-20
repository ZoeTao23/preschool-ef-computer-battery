# Computer-Based Executive Function Battery for Chinese Preschool Children

This repository contains the analysis code for the study:

**"Development and Validation of a Computer-Based Battery for Assessing Executive Function of Preschool Children in China: Score Development, Reliability, and Validity"**

## Project Overview

This project provides the complete analytical pipeline for developing and validating a computer-based executive function (EF) assessment battery for preschool children in China. The repository includes code for:

- **IRT model development** and parameter estimation
- **Descriptive analysis** for baseline characteristics and task score
- **Validity analysis**(construct validity, criterion validity, measurement invariance)
- **Reliability analysis** (internal consistency, test-retest reliability, longitudinal measurement invariance)

## Repository Structure

- **data/**: Data files and processing scripts
  - `Table_1_scale_characteristics.xlsx` - Scale variable descriptions
  - `Table_2_demo_characteristics.xlsx` - Demographic variable descriptions
  - `Table_3_task_characteristics.xlsx` - Game task variable descriptions
  - `Table_4_original_game_and_scale_data.xlsx` - Synthetic demographic, scale scores, and game performance data
  - `Table_5_game_data.csv` - Synthetic game task-level performance records

- **analysis/**: Main analysis scripts
  - `IRT.R` - core code
  
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
install.packages(c(
  "openxlsx", "tidyverse", "compareGroups", "lavaan", 
  "semTools", "blandr", "ggplot2", "gridExtra", "purrr", 
  "ltm", "mice", "psych", "mirt", "scales"
))
```
