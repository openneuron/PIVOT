# PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data

## About this package
  * See http://kim.bio.upenn.edu/software/idv.shtml for more details.

## Installation
  * Main Program: Please copy and paste the following command to R console. Upgrading R and Rstudio to the latest version is strongly recommended.

```
# Load 
install.packages("devtools")
library("devtools")
source("http://bioconductor.org/biocLite.R")  

# Install PIVOT
install_github("qinzhu/PIVOT.data")
install_github("qinzhu/PIVOT.analysis")
install_github("qinzhu/PIVOT.launcher")
```
 * (Optional but strongly recommended) Dependencies:
   * For report generation, you need Pandoc: http://pandoc.org/installing.html
   * For PDF report generation, you need Latex: https://www.latex-project.org/get/

## Running PIVOT
  * Note you MUST launch PIVOT with Rstudio.
  * To run PIVOT, in Rstudio console, use command 
```
library(PIVOT)
pivot()
```

## Troubleshooting
 * URL 'http://xxx.tgz': status was '404 Not Found'
   * Call `chooseCRANmirror()` to select another CRAN mirror.
   
 * Current Known bug:
   * Some times when user press the "launch module" or "clean session" button, new window does not show up. In such cases, please call the last command you see in Rstudio, i.e., `pivot_main()` or `pivot('clean')`

## Citation

Qin Zhu, Stephen A Fisher, Hannah Dueck, Sarah Middleton, Mugdha Khaladkar, Young-Ji Na, Junhyong Kim KimLabIDV?: Application for Interactive RNA-Seq Data Analysis and Visualization (Preprint) bioRxiv 053348; doi: http://dx.doi.org/10.1101/053348


Qin Zhu

Junhyong Kim Lab

University of Pennsylvania

2015 - 2017
