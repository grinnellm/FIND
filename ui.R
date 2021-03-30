# Define UI for application that draws a histogram
ui <- fluidPage(

  # # Allow reset of inputs
  # useShinyjs(),

  # Application title
  titlePanel(title = "FIND", windowTitle = "FIND"),

  # Sidebar with input parameters
  sidebarLayout(

    # Sidebar (input etc)
    sidebarPanel(
      width = 3,

      ##### Event and buffers #####
      bootstrapPage(
        p(HTML(
          "Find Pacific Herring spawn sites.",
          "This is a draft.",
          "Click <b>Update</b> to apply settings and view results."
        )),
        # Default location is PBS (49.21 N, -123.96 W); whole coast is -127.5 N
        # and 52.125 W with a 450 km buffer
        div(
          style = "display:inline-block; width:49%",
          h2(HTML(
            "Event"
            # "(<a href=http://spatialreference.org/ref/sr-org/14/>WGS 1984</a>)"
          )),
          div(
            style = "display:inline-block; width:49%",
            numericInput(
              inputId = "longitude", label = "Longitude", value = -123.96,
              min = rangeSI$Long[1], max = rangeSI$Long[2], step = 0.01
            )
          ),
          div(
            style = "display:inline-block; width:49%",
            numericInput(
              inputId = "latitude", label = "Latitude", value = 49.21,
              min = rangeSI$Lat[1], max = rangeSI$Lat[2], step = 0.01
            )
          )
        ),
        div(
          style = "display:inline-block; width:49%",
          h2("Buffers (km)"),
          div(
            style = "display:inline-block; width:49%",
            numericInput(
              inputId = "bufSpill", label = "Circle radius", value = 8,
              min = 0, step = 1
            )
          ),
          div(
            style = "display:inline-block; width:49%",
            numericInput(
              inputId = "bufMap", label = "Map edge", value = 10, min = 1,
              step = 1
            )
          )
        )
      ),

      ##### Subset spawns #####
      h2("Subset spawns"),
      bootstrapPage(
        div(
          style = "display:inline-block; width:100%; vertical-align:text-top",
          sliderInput(
            inputId = "yrRange", label = "Year(s)", min = min(spawn$Year),
            max = max(spawn$Year), value = range(spawn$Year), sep = ""
          )
        ),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          selectInput(
            inputId = "regions", label = "Region(s)",
            choices = unique(spawn$Region), multiple = TRUE, size = 3,
            selectize = FALSE, selected = unique(spawn$Region)
          )
        ),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          selectInput(
            inputId = "statAreas", label = "Statistical area(s)",
            choices = unique(spawn$StatisticalArea), multiple = TRUE, size = 3,
            selectize = FALSE, selected = unique(spawn$StatisticalArea)
          )
        ),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          selectInput(
            inputId = "sections", label = "Section(s)",
            choices = unique(spawn$Section), multiple = TRUE, size = 3,
            selectize = FALSE, selected = unique(spawn$Section)
          )
        )
      ),

      ##### Summarise spawns #####
      bootstrapPage(
        h2("Summarise spawns"),
        div(
          style = "display:inline-block; width:24%; vertical-align:text-top",
          checkboxGroupInput(
            inputId = "summary", label = "Aggregate",
            choiceNames = c("By Location"), choiceValues = c("loc")
          )
        )
      ),

      ##### Display features #####
      bootstrapPage(
        h2("Display features"),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          checkboxGroupInput(
            inputId = "location", label = "Event",
            choiceNames = c("Point", "Circle"),
            choiceValues = c("pt", "circ"), selected = c("pt", "circ")
          )
        ),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          checkboxGroupInput(
            inputId = "sDisplay", label = "Spawns",
            choiceNames = c("Location names"), choiceValues = c("lNames")
          )
        ),
        div(
          style = "display:inline-block; width:32%; vertical-align:text-top",
          checkboxGroupInput(
            inputId = "polys", label = "Polygons",
            choiceNames = c(
              "SAR boundaries", "Section boundaries", "Section labels"
            ),
            choiceValues = c("reg", "sec", "sLab"), selected = c("sec", "sLab")
          )
        )
      ),

      ##### View results #####
      # h2( "View results" ),
      div(
        style = "text-align:center",
        submitButton("Update", icon("refresh"))
      ) # ,
      # actionButton("resetAll", "Reset all")
    ), # End sidebar panel

    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      # Start tabs
      tabsetPanel(
        type = "tabs", selected = "Figure",

        ##### Figure #####
        tabPanel(
          title = "Figure", br(),
          withSpinner(ui_element = plotOutput(
            outputId = "map", width = "100%", height = "650px",
            click = "plotClick",
            hover = hoverOpts(id = "plotHover", delayType = "debounce")
          )),
          DT::dataTableOutput(outputId = "spawnClick")
        ),

        ##### Table #####
        tabPanel(
          title = "Table", br(),
          withSpinner(ui_element = DT::dataTableOutput(outputId = "dat"))
        ),

        ##### Information #####
        tabPanel(
          title = "Information", br(),
          bootstrapPage(
            div(
              style = "display:inline-block; width:49%;
              vertical-align:text-top",
              p(HTML(
                "In this analysis, we summarise spawns by year and",
                "spawn number in tonnes (t).",
                "We also indicate spawn start and end dates, length and width",
                "in metres (m), as well as the survey method.",
                "Alternatively, users can choose to summarise spawns by",
                "Location (i.e., aggregate spawns over years and spawn",
                "numbers).",
                "In this case, spawns are described by the mean spawn index."
              )),
              p(
                "'Incomplete' spawns are included in this analysis; they are",
                "indicated by grey circles in the figure, and empty cells in",
                "the table.",
                "These spawns are rare, and they include spawns that were",
                "observed but not surveyed, and spawns that were surveyed but",
                "have insufficient data to calculate the spawn index."
              ),
              p(HTML(
                "We use relatively coarse (i.e., low-resolution) land",
                "polygons", SimpleCite(WesselSmith1996), "to enable a",
                "responsive analysis.",
                "However, the coarse land polygons omit some geographic",
                "features which causes some spawns to be displayed in open",
                "water or on land.",
                "Do not use these maps for navigation."
              )),
              h2("Stock assessment regions"),
              p(HTML(
                "Pacific Herring spawn survey observations have a nested",
                "hierarchical structure:",
                "observations are nested within transects,",
                "transects are nested within spawns, and",
                "spawns are nested within Locations",
                SimpleCite(GrinnellEtal, trail = "."),
                "For stock assessment purposes,",
                "Locations are nested within Sections,",
                "Sections are nested within Statistical Areas, and",
                "Statistical Areas are nested within the five major and two",
                "minor stock assessment regions (SARs) in British Columbia",
                "(see Figure).",
                "The terms 'major' and 'minor' describe relative differences",
                "in the geographic and biomass scales represented.",
                "SAR boundaries attempt to capture the habitat range of",
                "relatively discrete migratory herring stocks, and are based",
                "on historical records of commercial catch and spawning",
                "sites."
              )),
              p(HTML(
                "There are differences between major and minor SARs",
                "regarding the amout of effort used to search and survey",
                "Pacific Herring spawn.",
                "Typically, minor SARs receive less search effort than major",
                "SARs which could cause more spawns to be inadvertently",
                "omitted in minor SARs.",
                "In addition, spawn surveyors are more likely to use surface",
                "surveys in minor SARs;",
                "surface surveys are thought to be less accurate than dive",
                "surveys which are used extensively in major SARs",
                SimpleCite(GrinnellEtal, trail = "."),
                "Finally, some spawns are reported by the public, which is",
                "less common in minor SARs because they tend to be more",
                "remote and difficult to access than major SARs.",
                "These differences are accentuated in areas outside SAR",
                "boundaries;",
                "they receieve even less effort than minor SARs."
              )),
              h2("Interpreting spawn index data"),
              p(HTML(
                "There are several challenges to interpreting spawn",
                "index data, including but not limited to",
                SimpleCite(GrinnellEtal, trail = ":"),
                "<ul>",
                "<li>The spawn index is a relative index of spawning biomass,",
                "</li>",
                "<li>The spawn survey is a presence only survey;",
                "thus the spawn index is a minimum spawning biomass,</li>",
                "<li>There are two different spawn survey periods with",
                "substantial differences in survey effort and method:",
                "<ul>",
                "<li>Surface period from",
                paste0(
                  qPeriods$StartDate[1], " to ", qPeriods$EndDate[1],
                  ", and</li>"
                ),
                "<li>Dive period from",
                paste0(qPeriods$StartDate[2], " to present,</li>"),
                "</ul>",
                "<li>Surface spawn surveys use two different methods to",
                "estimate the number of egg layers:",
                "<ul>",
                "<li>Spawn intensity categories:",
                "<ul>",
                "<li>Five categories from 1951 to 1968, and</li>",
                "<li>Nine categories from 1969 to 1978, and</li>",
                "</ul>",
                "<li>Direct estimates from 1979 to present,",
                "</li>",
                "</ul>",
                "<li>The spawn index is derived from surface and dive",
                "observations of egg deposition, and includes uncertainty and",
                "assumptions, and</li>",
                "<li>Spawn index calculations rely on dated parameters and",
                "models.</li>",
                "</ul>"
              ))
            ),
            div(
              style = "display:inline-block; width:39%;
              vertical-align:text-top",
              img(src = "BC.png", style = "width:100%"),
              p(
                "Boundaries for Pacific Herring stock assessment regions",
                "(SARs) in British Columbia.",
                "The major SARs are Haida Gwaii (HG), Prince Rupert District",
                "(PRD), Central Coast (CC), Strait of Georgia (SoG), and West",
                "Coast of Vancouver Island (WCVI).",
                "The minor SARs are Area 27 (A27) and Area 2 West (A2W).",
                "Units: kilometres (km)."
              )
            )
          )
        ),

        ##### Download #####
        tabPanel(
          title = "Download", br(), style = "width:400pt",
          p(HTML(
            "We provide geographic data in the following",
            "<a href=http://spatialreference.org/ref/epsg/nad83-bc-albers/>",
            paste(geoProj, ".", sep = ""), "</a>"
          )),
          bootstrapPage(
            h2("Spawn sites"),
            div(
              style = "display:inline-block",
              downloadButton(
                outputId = "downloadFigure",
                label = "Download figure (*.png)"
              )
            ),
            div(
              style = "display:inline-block",
              downloadButton(
                outputId = "downloadTable",
                label = "Download table (*.csv)"
              )
            )
          ),
          bootstrapPage(
            h2("Polygons"),
            div(
              style = "display:inline-block",
              downloadButton(
                outputId = "downloadSections",
                label = "Download herring sections (*.csv)"
              )
            ),
            div(
              style = "display:inline-block",
              downloadButton(
                outputId = "downloadLand",
                label = "Download land (*.csv)"
              )
            )
          )
        ),

        ##### Contact #####
        tabPanel(
          title = "Contact", br(), style = "width:400pt",
          p(HTML(
            "For more information on Pacific Herring spawn data, contact",
            "<a href=mailto:Jaclyn.Cleary@dfo-mpo.gc.ca>Jaclyn Cleary</a>,",
            "<a href=mailto:Matthew.Grinnell@dfo-mpo.gc.ca>Matt",
            "Grinnell</a>, or",
            "<a href=mailto:Matthew.Thompson@dfo-mpo.gc.ca@dfo-mpo.gc.ca>Matt",
            "Thompson</a>,",
            "DFO Science, Pacific Biological Station.",
            "Contact Matt Grinnellfor questions regarding this App."
          )),
          br(),
          img(src = "HerringDFO.jpg", style = "width:100%"),
          p(HTML(
            "<font color='grey'>Pacific Herring (<em>Clupea",
            "pallasii</em>). Image credit:",
            "<a href=http://www.pac.dfo-mpo.gc.ca/>Fisheries and Oceans",
            "Canada</a>.</font>"
          ))
        ),

        ##### About #####
        tabPanel(
          title = "About", br(), style = "width:400pt",
          p(HTML(
            "<b>FIND</b> (FIND Is Not Difficult) was built using",
            "<a href=https://shiny.rstudio.com/>Shiny</a> inside",
            "<a href=https://www.rstudio.com/>RStudio</a>.",
            "View the source code and report issues on our",
            "<a href=https://github.com/grinnellm/FIND>GitHub repository</a>.",
            "This version",
            # paste("(", Sys.Date(), ")", sep=""),
            "was built with",
            R.version.string,
            "and the following packages."
          )),
          withSpinner(ui_element = DT::dataTableOutput(outputId = "packages"))
        )
      ) # End tabs
    ) # End main panel
  ) # End sidebar layout
) # End ui
