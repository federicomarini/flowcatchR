About
========

**shinyFlow** is a complementary tool to **flowcatchR** for exploring and inspecting time-lapse imaging datasets in a fully interactive environment granted by the Shiny framework (http://shiny.rstudio.com/).

**shinyFlow** reflects the steps in the workflow proposed in the vignette of **flowcatchR**, available [here](http://bioconductor.org/packages/release/bioc/html/flowcatchR.html).

The user is invited to explore and if needed edit these steps to fit the needs of the analysis, if additional/alternative steps need to be taken for meaningful extraction of information. The files `server.R` and `ui.R` are located in the installation folder of the package **flowcatchR**, which can be usually found with this command:

```r
system.file("shiny", package = "flowcatchR")
```

## Acknowledgements
                          
The **shinyFlow** application is based on the package **flowcatchR**, available at Bioconductor at this address (http://bioconductor.org/packages/release/bioc/html/flowcatchR.html). 


**shinyFlow** is currently developed at the Institute of Medical Biostatistics, Epidemiology and Informatics (IMBEI - University Medical Center, Mainz, Germany) by Federico Marini, under the supervision of Harald Binder and Johanna Mazur. 

Many thanks to Sven Jäckel, Kerstin Jurk and Mareike Döhrmann for providing test datasets and feedback on the features to implement. The TRP A15 Grant funded the development phase of the **flowcatchR** package and of this tool. 

#### Author/Maintainer
Federico Marini (marinif@uni-mainz.de)

#### Webpage 

* http://bioconductor.org/packages/release/bioc/html/flowcatchR.html
* https://github.com/federicomarini/flowcatchR

#### Citation

If you use this tool to analyze your data, please cite the package as suggested here:

```r
citation("flowcatchR")
```
Marini F (2015). *flowcatchR: Tools to analyze in vivo microscopy imaging data focused on tracking flowing blood cells. R package version 1.0.3*, https://github.com/federicomarini/flowcatchR.




