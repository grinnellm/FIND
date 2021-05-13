# Define server logic required to draw a histogram
server <- function(input, output) {

  # Show modeal dialogue on startup - requires a click to proceed
  showModal(
    modalDialog(
      title = "Disclaimer",
      HTML(
        "<b>This is a draft; do not use these data for planning or",
        "analyses.</b>",
        "Read the details in the tabs and supporting documents (i.e., links).",
        "Please contact the authors if you have any questions."
      )
    )
  )

  # Get package info
  packInfo <- reactive(
    GetPackages()
  )

  # Get the spill location
  spill <- reactive(
    ConvLocation(xy = c(input$longitude, input$latitude))
  )

  # Clip stock and land shapefiles
  shapesSub <- reactive(
    ClipPolys(
      stocks = secPoly, land = landPoly, pt = spill()$xySP,
      buf = input$bufMap * 1000
    )
  )

  # Make a circle
  circDF <- reactive(
    MakeCircle(
      center = coordinates(spill()$xySP), radius = input$bufSpill * 1000
    )
  )

  # Get spawn data
  spawnSub <- reactive(
    CropSpawn(
      dat = spawn, yrs = input$yrRange, ext = shapesSub()$extBuff,
      grp = input$summary, reg = input$regions, sa = input$statAreas,
      sec = input$sections
    )
  )

  # Get the data
  output$dat <- DT::renderDataTable({

    # Ensure there are spawn locations to show
    validate(need(
      nrow(spawnSub()) >= 1,
      "Error: No spawns match these criteria."
    ))

    # Get the data
    df <- spawnSub()

    # Wrangle into a pretty data table
    res <- WrangleDT(
      dat = df, input = input$summary, optPageLen = 15, optDom = "lftip",
      optNoData = "No data available in table"
    )

    # Return the table
    return(res)
  }) # End data

  # Make the figure (map)
  output$map <- renderPlot(res = 150, {

    # Ensure map buffer is larger than spill buffer
    validate(need(
      input$bufSpill <= input$bufMap,
      "Error: Spill buffer can not exceed map bufer."
    ))

    # Ensure there are spawn locations to show
    validate(need(
      nrow(spawnSub()) >= 1, "Error: No spawns match these criteria."
    ))

    # Plot the area (default map)
    hMap <- ggplot(data = shapesSub()$landDF, aes(x = Eastings, y = Northings)) +
      geom_polygon(
        data = shapesSub()$landDF, aes(group = group), fill = "lightgrey"
      )

    # If showing sections
    if ("sec" %in% input$polys) {
      # Update the map
      hMap <- hMap +
        geom_path(
          data = shapesSub()$secDF, aes(group = Section), size = 0.25,
          colour = "black"
        )
    } # End if showing sections

    # If showing sections labels
    if ("sLab" %in% input$polys) {
      # Ensure polygons are present
      validate(need(
        "sec" %in% input$polys,
        "Error: Enable section polygons to show labels."
      ))
      # Update the map
      hMap <- hMap +
        geom_label(
          data = shapesSub()$secCentDF, alpha = 0.5, aes(label = Section)
        )
    } # End if showing labels

    # If showing SAR boundaries
    if ("reg" %in% input$polys & !is.null(shapesSub()$regDF)) {
      # Update the map
      hMap <- hMap +
        geom_path(
          data = shapesSub()$regDF, aes(group = Region), size = 1,
          colour = "black"
        )
    } # End if showing SARs

    # If showing the point location
    if ("pt" %in% input$location) {
      # Update the map
      hMap <- hMap +
        geom_point(data = spill()$xyDF, colour = "red", shape = 42, size = 8)
    } # End if showing the point location

    # If showing the circle
    if ("circ" %in% input$location) {
      # Update the map
      hMap <- hMap +
        geom_path(data = circDF(), colour = "red", size = 0.25)
    } # End if showing the circle

    # If aggregating by location
    if ("loc" %in% input$summary) {
      # Extract the number of spawns
      nSpawns <- spawnSub()$Number
      # If there are only a few different values
      if (length(unique(nSpawns)) <= 4) {
        # Show the actual numbers
        nSpawnShow <- unique(nSpawns)[order(unique(nSpawns))]
      } else { # End if there are only a few different values, otherwise
        # Get a few nice numbers to show the range
        nSpawnShow <- pretty(nSpawns, n = 5)
      } # End if there are more than a few different values
      # Update the map
      hMap <- hMap +
        geom_point(
          data = spawnSub(), aes(colour = SpawnIndex, size = Number),
          alpha = 0.5
        ) +
        labs(colour = "Mean\nspawn\nindex (t)", size = "Number of\nspawns") +
        guides(
          colour = guide_colourbar(order = 1), size = guide_legend(order = 2)
        ) +
        scale_size_area(breaks = nSpawnShow)
    } else { # End if aggregating by location, otherwise
      # Update the map
      hMap <- hMap +
        geom_point(
          data = spawnSub(), aes(colour = SpawnIndex), size = 4, alpha = 0.5
        ) +
        labs(colour = "Spawn\nindex (t)")
    } # End if not aggregating by location

    # If showing location names
    if ("lNames" %in% input$sDisplay) {
      # Get unique locations
      uSpawns <- spawnSub() %>%
        select(Eastings, Northings, LocationName) %>%
        distinct()
      # Show location names
      hMap <- hMap +
        geom_text_repel(
          data = uSpawns, mapping = aes(label = LocationName), size = 2,
          box.padding = unit(0.5, "lines"), segment.colour = "darkgrey"
        )
    } # End if showing location names

    # Get number of unique years
    nYrs <- length(unique(input$yrRange))

    # Get unique years
    uYrs <- paste(unique(input$yrRange), collapse = " to ")

    # Add map layers
    hMap <- hMap +
      scale_colour_viridis(na.value = "black", labels = comma) +
      coord_equal() +
      labs(
        x = "Eastings (km)", y = "Northings (km)", caption = geoProj,
        title = paste("Year", ifelse(nYrs > 1, "s", ""), ": ", uYrs, sep = "")
      ) +
      scale_x_continuous(
        labels = function(x) comma(x / 1000), expand = c(0, 0)
      ) +
      # sec.axis=sec_axis(trans=~fun(x=.))
      scale_y_continuous(
        labels = function(x) comma(x / 1000), expand = c(0, 0)
      ) +
      expand_limits(
        x = shapesSub()$extDF$Eastings, y = shapesSub()$extDF$Northings
      ) +
      # annotation_north_arrow( location="tl", style=north_arrow_nautical() ) +
      myTheme

    # Save the map (if download requested) -- not sure why this has to be here
    output$downloadFigure <- downloadHandler(
      filename = "SpawnMap.png",
      content = function(file) {
        ggsave(
          filename = file, plot = hMap, dpi = 600, height = 7, width = 7.5
        )
      },
      contentType = "image/png"
    )

    # Print the map
    return(hMap)
  }) # End map

  # Save data (spawn index; if download requested)
  output$downloadTable <- downloadHandler(
    filename = "SpawnData.csv",
    content = function(file) write_csv(x = spawnSub(), path = file),
    contentType = "text/csv"
  )

  # Save herring section polygons
  output$downloadSections <- downloadHandler(
    filename = "SectionPolygons.csv",
    content = function(file) write_csv(x = shapesSub()$secDF, path = file),
    contentType = "text/csv"
  )

  # Save land polygons
  output$downloadLand <- downloadHandler(
    filename = "LandPolygons.csv",
    content = function(file) write_csv(x = shapesSub()$landDF, path = file),
    contentType = "text/csv"
  )

  # Package info
  output$packages <- DT::renderDataTable({
    # Get package info
    res <- packInfo() %>%
      datatable(options = list(
        lengthMenu = list(c(15, -1), list("15", "All")), pageLength = 15
      ))
  })

  # Use mouse click to select points
  output$spawnClick <- renderDataTable({
    # Select point closest to the point
    df <- nearPoints(
      df = spawnSub(), coordinfo = input$plotClick, threshold = 10
    )
    # Custom text if no records
    noRecords <- paste(
      "No spawns selected;",
      "select a point and then click 'Update' to view details."
    )
    # Wrangle into a pretty data table
    res <- WrangleDT(
      dat = df, input = input$summary, optPageLen = -1, optDom = "lftip",
      optNoData = noRecords
    )
    # Return the table
    return(res)
  })

  # # Use mouse hover to select points (fewer details)
  # output$spawnHover <- renderPrint( {
  #   if( !is.null(input$plotHover) ) {
  #     hover=input$plotHover
  #     dist <- sqrt((hover$x-spawn$Eastings)^2+(hover$y-spawn$Northings)^2)
  #     # cat("Location")
  #     if(min(dist) < 3)
  #       spawn$LocationName[which.min(dist)]
  #
  #   }
  # }
  # )

  # # Reset all inputs
  # observeEvent( input$resetAll, reset("form") )
} # End server
