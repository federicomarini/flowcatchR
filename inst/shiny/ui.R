library(shiny)

shinyUI(
  navbarPage("shinyFlow",
             tabPanel("General Options - Import Data",
                      sidebarLayout(
                        sidebarPanel(
                          "",
                          helpText("Welcome to shinyFlow, the Shiny App companion to the flowcatchR package!"),
                          h4("General Options - Import Data"),
                          helpText("Type the path to the folder containing the time-lapse images - Error messages will be delivered if no images are found. If left blank, it will use the MesenteriumSubset as an example dataset."),
                          textInput(inputId = "dataInput",
                                    label = "Path to folder",
                                    value = ""),
                          # value = "/Volumes/users$/marinif/Development/exportForReimporting"),
                          
                          numericInput("nrFrames","Number of frames to display",value = 9,min = 1,max = 25)
                          
                        ),
                        mainPanel(
                          img(src = "logo_unimedizin.png"),img(src = "logo_bioconductor.gif"),
                          h2("Exploration of the raw images"),
                          p("Number of frames available in the dataset loaded:"), textOutput("totFrames"),
                          p("Currently displayed by the app:"), textOutput("maxAvai"),
                          plotOutput("plotRaw")
                        )
                      )
             ),
             
             
             tabPanel("Channel Selection",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Channel Selection"),
                          helpText("To proceed to the following steps, select a channel of the raw images if they are provided not in GrayScale values - a single channel is expected for the detection of particles. "),
                          selectInput("channel", "Choose a channel to display", choices = c("red", "green", "blue","all"),selected = "red")
                        ),
                        mainPanel(
                          h2("Images on the selected channel"),
                          p("Displayed here are the images related to the selected channel alone."),
                          plotOutput("plotChannel")
                        )
                      )
             ),
             tabPanel("Particles Detection",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Preprocessing Parameters"),
                          helpText("Please find below here a selection of the main parameters for the function preprocess.Frames. The brush is applied for smoothing previous to segmentation via adaptive thresholding."),
                          sliderInput("brushSize","brush size",min = 1,max = 11,value = 3,step = 2),
                          selectInput("brushShape","brush shape",choices = c("box", "disc", "diamond", "Gaussian", "line"),selected = "disc"),
                          sliderInput("atWidth","adaptive threshold window width value",min = 5,max = 100,value = 10),
                          sliderInput("atHeight","adaptive threshold window height value",min = 5,max = 100,value = 10),
                          sliderInput("atOffset","adaptive threshold value",min = 0,max = 2,value = 0.15,step = 0.01)
                        ),
                        mainPanel(
                          h2("Preprocessed images"),
                          p("Below here, the binarized images resulting from the preprocessing steps selected on the side panel."),
                          plotOutput("preprocessed"),
                          p("And here the raw images with contoured particles identified as an immediate feedback on the performances of the preprocessing steps."),
                          plotOutput("contoured"))
                      )
             ),
             
             
             tabPanel("Linking the Trajectories",
                      sidebarLayout(
                        sidebarPanel(h4("Linking the Particles and Generating the Trajectories"),
                                     helpText("Here it is possible to change the parameters for the link.particles function, used by flowcatchR for tracking the particles across frames. L and R refer to the original implementation by Sbalzarini and Koumotsakos, while the following ones are a part of the modification proposed by flowcatchR to account for the directionality of the flow of cells."),
                                     numericInput("trackL","Maximum pixel distance between two consecutive frames for a particle",value = 26,min = 1,max=100,step=1),
                                     numericInput("trackR","Link range",value = 2,min=1,max=7,step=1),
                                     numericInput("trackE1","Epsilon 1 - used for penalty function definition",value=0,min=0,max=1,step=0.001),
                                     numericInput("trackE2","Epsilon 2 - used for penalty function definition",value=0,min=0,max=10,step=0.1),
                                     numericInput("trackL1","Lambda 1 - used for penalty function definition",value=1,min=0,max=5,step=0.001),
                                     numericInput("trackL2","Lambda 2 - used for penalty function definition",value=1,min=0,max=5,step=0.001),
                                     selectInput("trackIncludearea","Include area variation in the cost function",choices=c("Yes","No"),selected = "Yes"),
                                     selectInput("trackIncludeintensity","Include intensity variation in the cost function",choices=c("Yes","No"),selected = "Yes")
                        ),
                        mainPanel(
                          h2("Linking the Trajectories"),
                          p("Displayed here is an overview of the trajectories across all frames."),
                          plotOutput("trajset"),
                          p("The following is a tabular representation of the first trajectory identified, as an example of the information made available."),
                          uiOutput("firstTrajPrint")
                        )
                      )
             ),
             
             
             tabPanel("Results Export",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Exporting Frames and Particles"),
                          helpText("Available functions are triggered by clicking on the buttons in the main panel, to export contoured Frames objects for particles, for trajectories, as well as ParticleSet objects converted to csv format. The UI below allows to select multiple trajectories to be painted one by one on the raw images. This is a starting point for the further analysis done on the kinematic features that can be computed if using flowcatchR on the command line"),
                          p(),
                          uiOutput("selectTraj") 
                          
                        ),
                        mainPanel(
                          h2("Results Export"),
                          p("Type here the path to the folder where Frames/ParticleSet objects will be exported to:"),
                          textInput(inputId = "exportFolder",label = "Select a folder to export the data",value = ""),
                          p("The plot below shows the selected trajectories with highlighted the contours of the corresponding particles in each frame."),
                          plotOutput("plotSelectedTrajs"),
                          
                          p("Click on the following action buttons to actually perform the export of the Frames and/or ParticleSet objects."),
                          actionButton("exportButton",label = "Export the Frames with contoured particles"), p(),
                          actionButton("exportButtonParts",label = "Export the ParticleSet of the dataset"), p(),
                          actionButton("exportButtonSelectedTrajs",label = "Export the Frames with contoured trajectories"), p(), 
                          
                          p("Enjoyed the tour? Got to know flowcatchR and its functionality? It is possible to move now to the command line and generate a fully scripted version of the analysis to guarantee full reproducibility.")
                        )
                      )
             ),
             
             
             tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Information"),
                          helpText("Useful information on shinyFlow and flowcatchR")
                        ),
                        mainPanel(
                          img(src = "logo_unimedizin.png"),img(src = "logo_bioconductor.gif"),
                          includeMarkdown("ABOUT.md")
                        )
                      )
             )
             
             
  )
)









