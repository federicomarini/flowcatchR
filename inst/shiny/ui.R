library(shiny)

shinyUI(
  navbarPage("shinyFlow! - Developmental version",
             tabPanel("Load data/Main options",
                      sidebarLayout(
                        sidebarPanel(
                          "Sidebar panel",
                                                    
                          h4("Data Input/General Options"),
                          helpText("Here really goes a nice general explanation of what should happen in this navbar tab panel"),
                          textInput(inputId = "dataInput",
                                    label = "Select an image of the dataset under analysis",
                                    value = "/Volumes/users$/marinif/Development/exportForReimporting"),
                          #/Volumes/users$/marinif/Development/exportForReimporting #?
                          
                          numericInput("nrFrames","nr of frames to display",value = 9,min = 1,max = 25)
                          
#                           submitButton()
                        ),
                        mainPanel(
                          img(src = "logo_unimedizin.png"),img(src = "logo_bioconductor.gif"),
                          h1("Raw images"),
                          p("Number of frames in the dataset loaded:"), textOutput("totFrames"),
                          p("We will display at most..."),
                          textOutput("maxAvai"),
#                           plotOutput("firstImage"),
                          
                          plotOutput("plotMesente")
                        )
                      )
             ),
             tabPanel("Channel selection",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h4("Channel selection"),
                          selectInput("channel", "Choose a channel to display", choices = c("red", "green", "blue","all"),selected = "red")
#                           submitButton()
                        ),
                        mainPanel(
                          h2("Channel images"),
                          plotOutput("mesenteChannel")
                        )
                      )
             ),
             tabPanel("Preprocessing",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Preprocessing Parameters"),
                          sliderInput("brushSize","brush size",min = 1,max = 11,value = 3,step = 2),
                          selectInput("brushShape","brush shape",choices = c("box", "disc", "diamond", "Gaussian", "line"),selected = "disc"),
                          sliderInput("atWidth","adaptive threshold window width value",min = 5,max = 100,value = 10),
                          sliderInput("atHeight","adaptive threshold window height value",min = 5,max = 100,value = 10),
                          sliderInput("atOffset","adaptive threshold value",min = 0,max = 2,value = 0.15,step = 0.01)
#                           submitButton()
                        ),
                        mainPanel(
                          h2("Preprocessed images"),
                          plotOutput("preprocessed"),
                          plotOutput("contoured"))
                      )
             ),
             tabPanel("Particle Linking",
                      sidebarLayout(
                        sidebarPanel(h4("Linking and Generation of Trajectories"),
                                     numericInput("trackL","Maximum pixel distance between two consecutive frames for a particle",value = 26,min = 1,max=100,step=1),
                                     numericInput("trackR","Link range",value = 3,min=1,max=7,step=1),
                                     numericInput("trackE1","Epsilon 1 - used for penalty function definition",value=0,min=0,max=1,step=0.001),
                                     numericInput("trackE2","Epsilon 2 - used for penalty function definition",value=0,min=0,max=10,step=0.1),
                                     numericInput("trackL1","Lambda 1 - used for penalty function definition",value=1,min=0,max=5,step=0.001),
                                     numericInput("trackL2","Lambda 2 - used for penalty function definition",value=1,min=0,max=5,step=0.001),
                                     selectInput("trackIncludearea","Include area variation in the cost function",choices=c("Yes","No"),selected = "Yes"),
                                     selectInput("trackIncludeintensity","Include intensity variation in the cost function",choices=c("Yes","No"),selected = "Yes")
#                                      submitButton()
                        ),
                        mainPanel(
                          h2("Linked particles"),
                          plotOutput("trajset"),
                          uiOutput("firstTrajPrint")
                        )
                      )
             )
             ,


             tabPanel("Results Export",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Results Export"),
                          
                          uiOutput("selectTraj") 
                          
#                           textInput(inputId = "exportFolder",
#                                     label = "Select a folder to export the data",
#                                     value = "/Volumes/users$/marinif/Development/outputExportTEST/"),
#                           
#                           actionButton("exportButton",label = "Export the dataset as ...")
# #                           checkboxInput("exportCheckbox",label = "Export the dataset as ..."),
#                           #                   textOutput("text")
# #                           submitButton()
                        ),
                        mainPanel(
                          h2("Export results"),
                          plotOutput("plotSelectedTrajs"),
#                           plotOutput("trajset"),
                          textInput(inputId = "exportFolder",label = "Select a folder to export the data",value = "/Volumes/users$/marinif/Development/outputExportTEST/"),
                          p(),
                          actionButton("exportButton",label = "Export the IMAGES with CONTOURED PARTICLES"),
                          actionButton("exportButtonParts",label = "Export the PARTICLES of the dataset"),
                          actionButton("exportButtonSelectedTrajs",label = "Export the IMAGES with CONTOURED TRAJECTORIES")
                          #                           plotOutput("trajset"),
#                           uiOutput("firstTrajPrint")
                        )
                      )
             )



             ,
             tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Information")
                        ),
                        mainPanel(
                          includeMarkdown("ABOUT.md")
                        )
                      )
             ),

              tabPanel("Experiments of interactivity",
                       sidebarLayout(
                         sidebarPanel(
                           h4("Try and click"),
                           actionButton("HighlightParticle",label = "click here and then click on the image to highlight the particle closest to the selected point"),
                           numericInput(inputId = "frameId",label = "frameId to display",value = 1,min = 1,max = 20)
                         ),
                         mainPanel(
                           h4("anything"),
                           plotOutput("interactiveImage")
#                            plotOutput("paintedOne")
                           
                           
                         )
                       )
              )
              
             
             
             
  )
)



                               
                               
                               
                         
#                                
#             
#                                
#  

#                    
#                    
#                    
#                    h2("Second level title - channel"),
#                    plotOutput("mesenteChannel"),
#                    
#                    h3("Third level title"),
#                    plotOutput("preprocessed"),
#                    plotOutput("contoured"),
#                    plotOutput("trajset"),
#                    
#                    uiOutput("firstTrajPrint"),
#                    
#                    
#                    h4("Fourth level title"),
#                    h4("Fifth level title"),
#                    h6("Sixth level title")
#                    #       ,
#                    #       p("p creates a paragraph of text. Note: this paragraph is followed by br(), which makes a blank line."),
#                    #       p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph", style = "font-family: 'times'; font-si16pt"),
#                    #       strong("strong() makes bold text."),
#                    #       em("em() creates italicized (i.e, emphasized) text."),
#                    #       br(),
#                    #       code("code displays your text similar to computer code"),
#                    #       div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
#                    #       br(),
#                    #       p("span does the same thing as div, but it works with",
#                    #         span("groups of words", style = "color:blue"),
#                    #         "that appear inside a paragraph.")
#                  )
#                )
#              )
#   ))












