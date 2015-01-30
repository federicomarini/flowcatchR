library("flowcatchR")


shinyServer(function(input, output) {
  
  rawData <- reactive({
    out <- read.Frames(input$dataInput)
    return(out)
  })
  
  output$totFrames <- reactive({
    length(rawData())
  })
  
  
  
  
  
  output$maxAvai <- reactive({
    fromTheData <- length(rawData())
    fromTheGUI <- input$nrFrames
    maxAvailable <- min(fromTheData,fromTheGUI)
    return(maxAvailable)
  })
  
#   maxFramesToPlot <- observe({
#     fromTheData <- length(rawData())
#     fromTheGUI <- input$nrFrames
#     maxAvailable <- min(fromTheData,fromTheGUI)
#     return(maxAvailable)
#   })
  
#   output$maxAvai <- maxFramesToDisplay()
  
  output$firstImage <- renderPlot({
#     if(input$dataInput!="")
    inspect.Frames(rawData(),1,display.method = "raster")
  })
  
  
  file
  
  data(MesenteriumSubset)
  
  # i have to make sure that the user does not put too many frames to view
  output$plotMesente <- renderPlot({
    inspect.Frames(rawData(),input$nrFrames,display.method = "raster")
  })
  
  
  
  output$mesenteChannel <- renderPlot({
    channeledMesente <- channel.Frames(rawData(),input$channel)
    inspect.Frames(channeledMesente,input$nrFrames,display.method = "raster")
  })
  
  

#   output$preprocessed <- renderPlot({
  binaryMesente <- reactive({

    out <- (preprocess.Frames(channel.Frames(rawData(),input$channel),brush.size = input$brushSize, brush.shape = input$brushShape,
                              at.offset = input$atOffset, 
                              at.wwidth = input$atWidth, at.wheight=input$atHeight,
                              kernSize=3, kernShape="disc",
                              watershedTolerance=1, watershedRadius=1))
    return(out)
  })
  
  mesenteParticles <- reactive({
    platelets <- particles(channel.Frames(rawData(),input$channel), binaryMesente(),input$channel)
    return(platelets)
  })  

  output$preprocessed <- renderPlot({
    inspect.Frames(binaryMesente(),input$nrFrames,display.method = "raster")
  })

  mesenteContoured <- reactive({
    out <- add.contours(raw.frames=rawData(),
                                     binary.frames=binaryMesente(),
                                     mode="particles")
    return(out)
  })



  output$contoured <- renderPlot({
    inspect.Frames(mesenteContoured(),input$nrFrames,display.method = "raster")
  })

  trajMesente <- reactive({
    linkedPlatelets <- link.particles(mesenteParticles(),
                                      L=input$trackL, R=input$trackR,
                                      epsilon1=input$trackE1, epsilon2=input$trackE2,
                                      lambda1=input$trackL1, lambda2=input$trackL2,
                                      penaltyFunction=penaltyFunctionGenerator(),
                                      include.area=1*(input$trackIncludearea=="Yes"),
                                      include.intensity=1*(input$trackIncludeintensity=="Yes"))
    trajs <- trajectories(linkedPlatelets)
    return(trajs)
  })
  
  output$trajset <- renderPlot({
    flowcatchR::plot2D.TrajectorySet(trajMesente(),rawData())
  })

#   observe(
#     if(input$exportButton != 0)
#     {
#       export.Frames(mesenteContoured(),dir = input$exportFolder)
#     }
#   )

  # Take an action when button is clicked
    observe({
      if (input$exportButton == 0)
       return()
      isolate({
        # Your logic here
       export.Frames(mesenteContoured(),dir =input$exportFolder)
       cat("HAHAHAhihihi---")
      })
    })

    observe({
      if (input$exportButtonParts == 0)
        return()
      isolate({
        # Your logic here
        export.particles(mesenteParticles(),dir =input$exportFolder)
        cat("HAHAHAhihihiPARTICLES---")
      })
    })


    observe({
      if (input$exportButtonSelectedTrajs == 0)
        return()
      isolate({
        # Your logic here
        export.Frames(trajPainted(),dir =input$exportFolder,nameStub = "selTrajs_")
        cat("traHAHAHAhihihi---")
      })
    })
  
  output$firstTrajPrint <- renderTable({
    print(trajMesente()[[1]]$trajectory)
  })

  allTrajs <- reactive({
    1:length(trajMesente())
  })

  output$selectTraj <- renderUI({
    selectInput("trajID","Select the trajectory of interest",choices = allTrajs(),selected = 1,multiple = TRUE)  
  })

#   selTrajs <- reactive({
#     
#   })

  trajPainted <- reactive({
    out <- add.contours(rawData(),binaryMesente(),trajMesente(),trajIDs = as.double(input$trajID),mode = "trajectories")
    return(out)
  })

  output$plotSelectedTrajs <- renderPlot({
    inspect.Frames(trajPainted(),input$nrFrames,display.method = "raster")
  })









  output$interactiveImage <- renderPlot({
    library("flowcatchR")
    imageToPaintOn <- MesenteriumSubset
    particleset <- platelets
    binary.frames <- preprocessedPlatelets
    display(getFrame(MesenteriumSubset,input$frameId,type = "render"),method="raster")
  })

#   output$paintedOne <- renderPlot({
#     paintOnThis <- getFrame(MesenteriumSubset,input$frameId,type = "render")
#     paintClosestShiny()
#     
#     display(imgWithParticlePainted,method="raster")
#     
#   })

#   paintClosestShiny <- function()
#   {
#     paintOnThis <- getFrame(MesenteriumSubset,input$frameId,type = "render")
#     imageToPaintOn <- paintOnThis
#     correspondingBinary <- getFrame(binary.frames,input$frameId,type = "render")
#     
#     coordsClick <- locator(1)
#     coordsClick
#     particlesLocations <- platelets[[1]][,1:2] # on the frame corresponding to the selected one
#     # compute euclidean distance to each from the location of the mouse click
#     distances <- data.frame(particles=row.names(platelets[[1]]),
#                             distToThis=rep(0,nrow(platelets[[1]]))
#     )
#     myX <- coordsClick$x
#     myY <- coordsClick$y
#     
#     distToClick <- unlist(lapply(1:nrow(particleLocations),
#                                  function(arg){
#                                    myDist <- sqrt( (myX - particleLocations$cell.0.m.cx[arg])^2 + (myY - particleLocations$cell.0.m.cy[arg])^2 )
#                                  }))
#     distances$distToThis <- distToClick
#     
#     partToDraw <- which.min(distances$distToThis)
#     
#     # put to zero everything but
#     binaryObj <- correspondingBinary
#     binaryObj[correspondingBinary!=partToDraw] <- 0
#     imgWithParticlePainted <- paintObjects(binaryObj,imageToPaintOn,col="yellow")
#     
#     
#   }
# 


#   observe({
#     if (input$HighlightParticle == 0)
#       return()
#     isolate({
#       # Your logic here
#       paintClosestShiny()
#     })
#   })




  
})









