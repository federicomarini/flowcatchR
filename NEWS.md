# flowcatchR 1.16.0

## New features

## Bug fixes

## Other notes

* Added a `NEWS.md` file to track changes to the package.

# flowcatchR 1.14.0

## New features

* the engine for plotting the trajectories is now based on plotly, abandoning the older rgl-based solution
	
## Other notes

* vignette: now also based on Rmarkdown, leaving the older Rnw-pdf be
* shortened example lines for better documentation
* better definition of import statements for better behaviour with other packages
* updated travis.yml for current setup

# flowcatchR 1.4.0

## New features

* added details in the vignette on the dockerflow proposal, where flowcatchR is made available in preconfigured Docker containers

## Bug fixes

* penaltyFunctionGenerator: simmetric penalty function is now correctly computed

## Other notes

* added LazyData: true in the DESCRIPTION
* updated travis.yml to use native R integration
* vignette: improved structure, to enhance readability and usability; removed rgl hook which was not used anyway
* documentation: slight updates and enhancements

# flowcatchR 1.2.0

## New features

* addTrajectory: changed implementation in the addTrajectory function, notable gain in computing time
* particles: implementation of particles() now used BiocParallel - massive gain in performances!
* snap() function to allow interaction of the user with the image, to display additional information on the trajectories identified on the Frames/ParticleSet
* shinyFlow() released! A Shiny App is delivered along with the package, documentation is available to guide exploration of data and parameters
* Jupyter/IPython notebooks available in the installed folder of the package - provided as template for further analysis
* Vignette: updated to reflect the current implementation and newest features
* preprocess.Frames: extended documentation
* BiocViews: added a couple of terms that fit better with current status of the package
* Added url for development version on github. Active development is performed there, therefore feel free to send pull requests/feature wishes (https://github.com/federicomarini/flowcatchR/issues)!
* Added integration of the repository with Travis-CI	
* add.contours: if image is in grayscale, use false colours to enhance detected particles
* plot2d.TrajectorySet is now able to draw a grid and help backtracking points from the trajectories

## Bug fixes

* select.Frames: fixed behaviour in select.Frames when selecting a single frame
* add.contours: corrected bug in add.contours for grayscale images, was preventing correct combining of images. now uses correctly false colours and aids identification
* particles: for computing particles,added check whether dimnames is set or not. previously it raised an error, but it was kind of overkilling
* fixed behaviour in export.Frames
* typos fixed
* Vignette: fixed typos, fixed parameter names in the preprocessing steps
* read.Frames: additional check on the existence of all files
* Grayscale images can now be directly preprocessed by selecting channel = "all"

# flowcatchR 1.0.3

## New features

* addTrajectory: changed implementation in the addTrajectory function, notable gain in computing time
* particles: implementation of particles() now used BiocParallel - massive gain in performances!
* snap() function to allow interaction of the user with the image, to display additional information on the trajectories identified on the Frames/ParticleSet
* shinyFlow() released! A Shiny App is delivered along with the package, documentation is available to guide exploration of data and parameters
* Jupyter/IPython notebooks available in the installed folder of the package - provided as template for further analysis
* Vignette: updated to reflect the current implementation and newest features
* preprocess.Frames: extended documentation
* BiocViews: added a couple of terms that fit better with current status of the package

# flowcatchR 1.0.2

## Bug fixes

* select.Frames: fixed behaviour in select.Frames when selecting a single frame
* add.contours: corrected bug in add.contours for grayscale images, was preventing correct combining of images. now uses correctly false colours and aids identification
* particles: for computing particles,added check whether dimnames is set or not. previously it raised an error, but it was kind of overkilling
* fixed behaviour in export.Frames
* typos fixed

# flowcatchR 1.0.1

## New features

* Added url for development version on github. Active development is performed there, therefore feel free to send pull requests/feature wishes (https://github.com/federicomarini/flowcatchR/issues)!
* Added integration of the repository with Travis-CI	
* add.contours: if image is in grayscale, use false colours to enhance detected particles
* plot2d.TrajectorySet is now able to draw a grid and help backtracking points from the trajectories

## Bug fixes

* Vignette: fixed typos, fixed parameter names in the preprocessing steps
* read.Frames: additional check on the existence of all files
* Grayscale images can now be directly preprocessed by selecting channel = "all"
 
## Other notes


# flowcatchR 1.0.0

## New features

* Initial release to Bioconductor!

# flowcatchR 0.99.4

## New features

* Final fixes. Accepted in the current status at Bioconductor.

# flowcatchR 0.99.3

## New features

* Changes in the code to be adapted to the latest devel version of EBImage (new functions were added for the Frames class).

# flowcatchR 0.99.2

## New features

* Updated/added documentation.

# flowcatchR 0.99.1

## New features

* Adapted after first round of review.

# flowcatchR 1.15.2

## New features

* Initial submit to Bioconductor.
