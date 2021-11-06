flowcatchR
==========

[![R build status](https://github.com/federicomarini/flowcatchR/workflows/R-CMD-check/badge.svg)](https://github.com/federicomarini/flowcatchR/actions)

#### A framework for tracking and analyzing flowing blood cells in time lapse microscopy images

**flowcatchR** is a set of tools to analyze in vivo microscopy imaging data, focused on tracking flowing blood cells.
It guides throughout all the steps of bioimage processing, from segmentation to calculation of features, filtering 
out particles not of interest, providing also a set of utilities to help checking the quality of the performed 
operations. The main novel contribution investigates the issue of tracking flowing cells such as the ones in blood
vessels, to categorize the particles in flowing, rolling and adherent by providing a comprehensive analysis of the
identified trajectories. The extracted information is then applied in the study of phenomena such as hemostasis and
study of thrombosis development. We expect this package to be potentially applied to a variety of essays, 
covering a wide range of applications founded on time-lapse microscopy.


### Installation
To install the development version for the package **flowcatchR**, please start a current version of R and type (using `devtools`):

```r 
# currently this can be done via github
install.packages("devtools") # if needed
devtools::install_github("federicomarini/flowcatchR")
```

If you want to install the current release version, just type:
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("flowcatchR")
```




### flowcatchR in a glimpse

```r
library("flowcatchR")
data("MesenteriumSubset")
fullResults <- kinematics(trajectories(particles(channel.Frames(MesenteriumSubset,"red"))))
```



### Vignette

To inspect the vignette and the code used in it, type:

```r
vignette("flowcatchR-vignette")
## and/or
browseVignettes("flowcatchR")
```

### Contact
For additional details regarding the functions of **flowcatchR**, please consult the documentation or write an email to marinif@uni-mainz.de. 

### Code of Conduct

Please note that the flowcatchR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

### Bug reports/Issues/New features

Please use https://github.com/federicomarini/flowcatchR/issues for reporting bugs, issues or for suggesting new features to be implemented.

