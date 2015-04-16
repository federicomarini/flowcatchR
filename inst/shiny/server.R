library("flowcatchR")


shinyServer(function(input, output) {
  
  rawData <- reactive({
    if(input$dataInput=="") {
      data(MesenteriumSubset)
      out <- MesenteriumSubset
    } else {
      out <- read.Frames(input$dataInput)
    }
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
  

  
  output$firstImage <- renderPlot({
    inspect.Frames(rawData(),1,display.method = "raster")
  })
  
  
  output$plotRaw <- renderPlot({
    inspect.Frames(rawData(),input$nrFrames,display.method = "raster")
  })
  
  
  output$plotChannel <- renderPlot({
    channeledData <- channel.Frames(rawData(),input$channel)
    inspect.Frames(channeledData,input$nrFrames,display.method = "raster")
  })
  
  
  binaryData <- reactive({
    out <- (preprocess.Frames(channel.Frames(rawData(),input$channel),brush.size = input$brushSize, brush.shape = input$brushShape,
                              at.offset = input$atOffset, 
                              at.wwidth = input$atWidth, at.wheight=input$atHeight,
                              kernSize=3, kernShape="disc",
                              watershedTolerance=1, watershedRadius=1))
    return(out)
  })
  
  dataParticles <- reactive({
    parts <- particles(channel.Frames(rawData(),input$channel), binaryData(),input$channel)
    return(parts)
  })  

  output$preprocessed <- renderPlot({
    inspect.Frames(binaryData(),input$nrFrames,display.method = "raster")
  })

  dataContoured <- reactive({
    out <- add.contours(raw.frames=rawData(),
                        binary.frames=binaryData(),
                        mode="particles")
    return(out)
  })



  output$contoured <- renderPlot({
    inspect.Frames(dataContoured(),input$nrFrames,display.method = "raster")
  })

  trajData <- reactive({
    linkedParts <- link.particles(dataParticles(),
                                  L=input$trackL, R=input$trackR,
                                  epsilon1=input$trackE1, epsilon2=input$trackE2,
                                  lambda1=input$trackL1, lambda2=input$trackL2,
                                  penaltyFunction=penaltyFunctionGenerator(),
                                  include.area=1*(input$trackIncludearea=="Yes"),
                                  include.intensity=1*(input$trackIncludeintensity=="Yes"))
    trajs <- trajectories(linkedParts)
    return(trajs)
  })
  
  output$trajset <- renderPlot({
    plot2D.TrajectorySet(trajData(),rawData())
  })
  
  
  trajPainted <- reactive({
    out <- add.contours(rawData(),binaryData(),trajData(),trajIDs = as.double(input$trajID),mode = "trajectories")
    return(out)
  })
  
  output$plotSelectedTrajs <- renderPlot({
    inspect.Frames(trajPainted(),input$nrFrames,display.method = "raster")
  })
  


  # Take an action when button is clicked
  observe({
    if (input$exportButton == 0)
     return()
    isolate({
      # Your logic here
     export.Frames(dataContoured(),dir = input$exportFolder)
     cat("Exported contoured Frames!\n")
    })
  })

  observe({
    if (input$exportButtonParts == 0)
      return()
    isolate({
      # Your logic here
      export.particles(dataParticles(),dir = input$exportFolder)
      cat("Exported ParticleSet to csv format!\n")
    })
  })


  observe({
    if (input$exportButtonSelectedTrajs == 0)
      return()
    isolate({
      # Your logic here
      export.Frames(trajPainted(),dir = input$exportFolder,nameStub = "selTrajs_")
      cat("Exported Frames with contoured selection of trajectories!\n")
    })
  })
  
  
  output$firstTrajPrint <- renderTable({
    print(trajData()[[1]]$trajectory)
  })

  allTrajs <- reactive({
    1:length(trajData())
  })

  output$selectTraj <- renderUI({
    selectInput("trajID","Select the trajectory/ies of interest",choices = allTrajs(),selected = 1,multiple = TRUE)  
  })


})






