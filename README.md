# README
# APPS-Public
Git Hub page for public facing materials for the article: "Three Accounts of Electronic Gambling Machine Related Harm: Impacts on Community Views Towards Gambling Policy and Responsibility"  

The related project page on the Open Science Framework can be accessed at: [osf.io/b8a9f](osf.io/b8a9f)

For instructions to reproduce the analysis, see the summary of the "Analysis" folder, below.

# Citation
To refer to this work in an academic text please use the following publication: 

Myles, D., O’Brien, K., Yücel, M., & Carter, A. (2023). Three Contrasting Accounts of Electronic Gambling Machine Related Harm: Impacts on Community Views Towards Gambling Policy and Responsibility. Journal of Gambling Studies. https://doi.org/10.1007/s10899-023-10206-1

# License
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

# Steps to Reproduce Analysis

1. Download the full directory from the GitHub repo.
2. **Open** `APPS-Public.RProj` - this the R Project file for this study.
3. **Open/Run/Knit:** `/Analysis/Quality-Checks/APPS_Quality-Attention_Bayes_2021-10.Rmd` This contains a couple of quality control checks and did some early exploration, including the comprehension check. Knit the document in html to produce a summary.
4. **Open/Run:** `/Analysis/Models/APPS_CM-Probit_Policy_2021-10.R` This will fit the primary 23 models used in the paper. It will also produce posterior draws and summaries of key parameters etc. This script will then output a whole heap of stuff to `/Analysis/Models/Output/` for use in other scripts. I have suppressed the original output files in the `.gitignore`. The model output was very large so it would have been infeasible to store this on github and/or OSF. Running the script will reproduce all these files. It's possible that some model output will differ from that used in the paper due to random sampling / MCMC etc. I've used a larger number of posterior draws to help stabilise 95% interval estimates but things may still vary a little at the second decimal place due to this.
5. **Open/Run:** `/Analysis/Results/APPS_Results-Policy_2021-10-13.Rmd` This document goes through all the output from the models produced in the previous step. I also analyse the "I don't know" responses here. This document was the first full draft of my results section before I switched back to Word to write up. This document should also give you an overview of how to index the model output to consider any individual results not reported in the paper or this R Markdown doc.
6. **Open/Run:** `/Analysis/Results/Plots/APPS_Plots.R` This script will re-produce the plots used in the paper.
7. **Open/Run:** `/Analysis/Results/APPS_Demographic-Counts_2022-01-28` This script will re-produce the large demographic table from the paper.

# Dependencies

Prior to submission I cross checked and re-ran all analysis and re-produced all markdown docs on in February of 2022.

##### R

You can download R from: https://cran.r-project.org/

|My R version info:  |                            |
|:-------------------|:---------------------------|
|**platform:**       |aarch64-apple-darwin20      |
|**arch:**           |aarch64                     |
|**os:**             |darwin20                    |
|**system:**         |aarch64, darwin20           |
|**major:**          |4                           |
|**minor:**          |1.2                         |
|**year:**           |2021                        |
|**month:**          |11                          |
|**day:**            |01                          |
|**svn rev:**        |81115                       |
|**language:**       |R                           |
|**version.string:** |R version 4.1.2 (2021-11-01)|
|**nickname:**       |Bird Hippie                 |  

##### R Studio

2021.09.2 Build 382  
"Ghost Orchid"   
**Release:** (fc9e217980ee9320126e33cdf334d4f4e105dc4f, 2022-01-04) for macOS  
  
##### Basics about my machine: 

MacBook Pro (14-inch, 2021):  

|                      |                             |
|:---------------------|:----------------------------|
|**Operating System:** | macOS Monterey v12.1        |
|**Chip:**             | Apple M1 Max (10 cores)     |
|**Memory:**           | 64 GB                       |

##### Packages:

Dependencies for reproducing the R markdown documents are printed at the end of the html file.

Packages used in modelling script (`APPS_CM-Probit_Policy_2021-10.R`) were as follows:

**R version:** 4.1.2 (2021-11-01)  
**Platform:** aarch64-apple-darwin20 (64-bit)  
**Running under:** macOS Monterey 12.1  

Matrix products: default  
LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib

**locale:**  
en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

**attached base packages:**  
parallel, stats, graphics, grDevices, utils, datasets, methods, base  
  
  
**other attached packages:**  

|                     |                 |                     |               |                 |
|:--------------------|:----------------|:--------------------|:--------------|:----------------|
| svglite_2.1.0       | tidybayes_3.0.2 | brms_2.16.3         | Rcpp_1.0.8    | rethinking_2.21 |
| cmdstanr_0.4.0.9001 | rstan_2.21.3    | StanHeaders_2.21.0-7| forcats_0.5.1 | stringr_1.4.0   |
| dplyr_1.0.8         | purrr_0.3.4     | readr_2.1.2         | tidyr_1.2.0   | tibble_3.1.6    |
| ggplot2_3.3.5       | tidyverse_1.3.1 | datapasta_3.1.0     | here_1.0.1    | |

**loaded via a namespace (and not attached):**  

|                   |                     |                     |                   |                   |
|:------------------|:--------------------|:--------------------|:------------------|:------------------|
| colorspace_2.0-3  | ellipsis_0.3.2      | ggridges_0.5.3      | rsconnect_0.8.25  | rprojroot_2.0.2   | 
| markdown_1.1      | base64enc_0.1-3     | fs_1.5.2            | rstudioapi_0.13   | farver_2.1.0      | 
| svUnit_1.0.6      | DT_0.20             | fansi_1.0.2         | mvtnorm_1.1-3     | lubridate_1.8.0   | 
| xml2_1.3.3        | bridgesampling_1.1-2| codetools_0.2-18    | knitr_1.37        | shinythemes_1.2.0 | 
| bayesplot_1.8.1   | jsonlite_1.7.3      | broom_0.7.12        | dbplyr_2.1.1      | ggdist_3.1.0      | 
| shiny_1.7.1       | compiler_4.1.2      | httr_1.4.2          | backports_1.4.1   | assertthat_0.2.1  | 
| Matrix_1.4-0      | fastmap_1.1.0       | cli_3.2.0           | later_1.3.0       | htmltools_0.5.2   | 
| prettyunits_1.1.1 | tools_4.1.2         | igraph_1.2.11       | coda_0.19-4       | gtable_0.3.0      | 
| glue_1.6.1        | reshape2_1.4.4      | posterior_1.2.0     | cellranger_1.1.0  | vctrs_0.3.8       | 
| nlme_3.1-155      | crosstalk_1.2.0     | tensorA_0.36.2      | xfun_0.29         | ps_1.6.0          | 
| rvest_1.0.2       | miniUI_0.1.1.1      | mime_0.12           | lifecycle_1.0.1   | gtools_3.9.2      | 
| MASS_7.3-55       | zoo_1.8-9           | scales_1.1.1        | colourpicker_1.1.1| hms_1.1.1         | 
| promises_1.2.0.1  | Brobdingnag_1.2-7   | inline_0.3.19       | shinystan_2.5.0   | yaml_2.3.5        | 
| gridExtra_2.3     | loo_2.4.1           | stringi_1.7.6       | dygraphs_1.1.1.6  | checkmate_2.0.0   | 
| pkgbuild_1.3.1    | shape_1.4.6         | systemfonts_1.0.4   | rlang_1.0.1       | pkgconfig_2.0.3   | 
| matrixStats_0.61.0| HDInterval_0.2.2    | distributional_0.3.0| evaluate_0.15     | lattice_0.20-45   | 
| labeling_0.4.2    | rstantools_2.1.1    | htmlwidgets_1.5.4   | processx_3.5.2    | tidyselect_1.1.2  | 
| plyr_1.8.6        | magrittr_2.0.2      | R6_2.5.1            | generics_0.1.2    | DBI_1.1.2         | 
| pillar_1.7.0      | haven_2.4.3         | withr_2.4.3         | xts_0.12.1        | abind_1.4-5       | 
| modelr_0.1.8      | crayon_1.5.0        | arrayhelpers_1.1-0  | utf8_1.2.2        | rmarkdown_2.11    | 
| tzdb_0.2.0        | grid_4.1.2          | readxl_1.3.1        | callr_3.7.0       | threejs_0.3.3     | 
| reprex_2.0.1      | digest_0.6.29       | xtable_1.8-4        | httpuv_1.6.5      | RcppParallel_5.1.5| 
| stats4_4.1.2      | munsell_0.5.0       | shinyjs_2.1.0  |

# Directory Information
## Filenames

Files are named according to the following convention: `ProjectCode_Short-Description_YYYY-MM-DD.filtype`

For example: `APPS_Raw-Data_2020-10-17.csv`

Initials for authors who have contributed comments or suggested changes to a recent revision may also be appended to end of the filename.

For example: `ProjectCode_Short-Description_2020-10-17_DM_AC.filetype`

## Folders

### Analysis
Contains all modelling scripts, cleaned data, and reporting scripts.

#### Models

This is where the scripts necessary to run the models can be found. I output these in three parts

#### css
This folder contains a hack of the css [Tufte theme](https://github.com/rstudio/tufte). There were a few things I din't like about this theme so I edited the css. I use this to style R markdown documents.

#### Data

All data files required to reproduce analyses.

Data have been provided in a number of file-types to assist with reproducibility.

Raw data are not available on the OSF due to privacy etc. Had to scrub IPs and a bunch of potentially identifying information. There were also a number of additional survey items we plan to use in another paper.

#### Quality-Checks
A few quality checks I performed prior to analysis. Includes analysis of the comprehension check. Also some demographic exploration.

#### Results
Contains an R Markdown script for composing and looking over results. Was where I started writing up the results section of the paper. There may more complete tables and figures in here. 

##### Plots

A little script to re-produce the plots from the paper. I've also included some vector graphics of the plots used in the paper.

### Materials
Materials necessary to reproduce the study. These include our news paper style interventions and an export of the Qualtrics survey. I've also included copies of the information statements and consent form.

#### Intervention

The text files in this folder contain the text used in the intervention news articles. I have also included the primary images used in these articles. The location of these images within the body of the original article is indicated by [image here].

The news articles used in the original experiment for the Design and Brain conditions were originally rendered using an .html/css script to match the design of a major Australian newspaper. The Industry intervention was rendered as a simple webpage for a fictitious lobby group. Following advice from our university legal department these have not been included as open materials due to legal concerns about how closely these files resembled the original publication. There were additional concerns raised about the name chosen for the lobby group intervention, so this has also been redacted from open source materials. 

#### Survey
Contains export of the survey used in the experiment. The *.qsf file is a Qualtrics export. You should be able to import this into Qualtrics to view the survey, and edit it as needed. 

The word doc also contains all the survey text, I edited a little for legibility.

Interventions have been removed from both files, see section above.

Note that there are a number of survey items that are not analysed or discussed in the paper. These are planned for use/analysis in another paper or project. 

