##### Housekeeping #####

# Install missing packages and load required packages (if required)
UsePackages <- function(pkgs, locn = "https://cran.rstudio.com/") {
  # Reverse the list
  rPkgs <- rev(pkgs)
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages()[, "Package"])]
  # Install missing packages if required
  if (length(newPkgs)) install.packages(newPkgs, repos = locn)
  # Loop over all packages
  for (i in 1:length(rPkgs)) {
    # Load required packages using 'library'
    eval(parse(text = paste("suppressPackageStartupMessages(library(", rPkgs[i],
      "))",
      sep = ""
    )))
  } # End i loop over package names
} # End UsePackages function

# Make packages available ("shinyjs" "plotly")
UsePackages(pkgs = c(
  "tidyverse", "raster", "shinycssloaders", "viridis", "scales", "DT", "shiny",
  "ggrepel", "ggspatial", "lubridate", "sf", "devtools", "maptools", 
  "SpawnIndex"
))

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##### Controls #####

# Saved csv datafile (from Spawn.R)
spawnLoc <- file.path("Data", "FIND.csv")

# Input coordinate reference system (point location; spill)
crsSpill <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Input coordinate reference system (herring sections)
crsSect <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Input coordinate reference system (land)
crsLand <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Input coordinate reference system (spawn)
crsSpawn <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Output coordinate reference system (BC Albers)
crsOut <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Geographic projection
geoProj <- "Projection: BC Albers (NAD 1983)"

# Location of the BC stocks shapefiles
locStocks <- file.path("Data", "Polygons", "HerringSections.shp")

# Location of the BC land file
locLand <- file.path("Data", "Polygons", "GSHHS_f_L1_Clip.shp")

# Change default ggplot theme to 'black and white'
theme_set(theme_bw())

# Modify default theme
myTheme <- theme(
  legend.box.background = element_rect(fill = alpha("white", 0.7)),
  legend.box.margin = margin(1, 1, 1, 1, "mm"), legend.key = element_blank(),
  legend.margin = margin(), legend.text.align = 1,
  panel.grid.major = element_line(colour = "darkgrey", linewidth = 0.2),
  panel.grid.minor = element_line(colour = "darkgrey", linewidth = 0.1),
  legend.background = element_rect(fill = "transparent"),
  plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines")
)

##### References #####

# Spawn index technical report
Grinnell2023 <- list(
  link = "https://science-catalogue.canada.ca/record=b4121678~S6",
  text = "Grinnell et al. 2023"
)

# GSHHS polygon
WesselSmith1996 <- list(
  link = "https://doi.org/10.1029/96JB00104",
  text = "Wessel and Smith 1996"
)

# CSAS q
CSAS2018 <- list(
  link = paste("http://www.dfo-mpo.gc.ca/csas-sccs/",
    "Publications/SAR-AS/2018/2018_002-eng.html",
    sep = ""
  ),
  text = "CSAS 2018"
)

##### Functions #####

# Get used packages (for session info)
GetPackages <- function() {
  # Get list of loaded packages
  myPkgs <- names(sessionInfo()$otherPkgs)
  # Get and wrangle packages
  res <- session_info()$packages %>%
    as_tibble() %>%
    dplyr::select(package, loadedversion, source) %>%
    rename(Package = package, Version = loadedversion, Source = source) %>%
    filter(Package %in% myPkgs) %>%
    arrange(Package)
  # Return the table
  return(res)
} # End GetPackages function

# Convert location to Albers
ConvLocation <- function(xy) {
  # Ensure longitude is within range of spawns
  if (xy[1] < rangeSI$Long[1] | xy[1] > rangeSI$Long[2]) {
    stop("Longitude must be between ", paste(rangeSI$Long, collapse = " and "),
      ".",
      sep = "", call. = FALSE
    )
  }
  # Ensure latitude is within range of spawns
  if (xy[2] < rangeSI$Lat[1] | xy[2] > rangeSI$Lat[2]) {
    stop("Latitude must be between ", paste(rangeSI$Lat, collapse = " and "),
      ".",
      sep = "", call. = FALSE
    )
  }
  # Make a matrix
  xyMat <- matrix(xy, ncol = 2)
  # Convert to spatial points
  xySP <- SpatialPoints(coords = xyMat, proj4string = CRS(crsSpill))
  # Transform to correct projection
  xySP <- spTransform(x = xySP, CRSobj = CRS(crsOut))
  # Make a data frame
  xyDF <- data.frame(xySP) %>%
    rename(Eastings = coords.x1, Northings = coords.x2)
  # Return the points
  return(list(xySP = xySP, xyDF = xyDF))
} # End ConvLocation function

# Function to wrangle shapefiles
ClipPolys <- function(stocks, land, pt, buf) {
  # Stop if buffer is negative
  if (buf < 0) {
    stop("Buffer must be non-negative.", call. = FALSE)
  }
  # Function to perform some light wrangling
  UpdateSections <- function(dat) {
    # Some light wrangling
    dat@data <- dat@data %>%
      mutate(
        StatisticalArea = as.character(StatArea),
        Section = as.character(Section)
      ) %>%
      dplyr::select(SAR, StatisticalArea, Section)
    # Get results
    res <- dat
    # Return updated sections
    return(res)
  } # End UpdateSections function
  # Update sections
  secBC <- UpdateSections(dat = stocks)
  # Project to BC
  secBC <- spTransform(x = secBC, CRSobj = CRS(crsOut))
  # Get a buffer around the point in question
  extBuff <- pt %>%
    as("sf") %>%
    st_buffer(dist = buf) %>%
    st_bbox(buff)
  # Convert the extent to a table
  extDF <- tibble(
    Eastings = c(extBuff$xmin, extBuff$xmax),
    Northings = c(extBuff$ymin, extBuff$ymax)
    )
  # Determine x:y aspect ration (for plotting)
  xyRatio <- diff(extDF$Eastings) / diff(extDF$Northings)
  # Crop the sections
  secBC <- crop(x = secBC, y = extBuff)
  # Error if the point is outside the herring sections (study area)
  if (is.null(secBC)) {
    stop("The event is outside the study area.", call. = FALSE)
  }
  # Determine section centroids
  secCent <- secBC %>%
    as("sf") %>%
    st_centroid()
  # Extract coordinates
  secCentCoords <- st_coordinates(secCent)
  # Convert to data frame
  secCentDF <- secCent %>%
    as_tibble() %>%
    select(-geometry) %>%
    mutate(Eastings = secCentCoords[, 1], Northings = secCentCoords[, 2]) %>%
    mutate(Section = formatC(secBC$Section, width = 3, flag = "0")) %>%
    arrange(Section)
  # Convert to data frame and select stat areas in question
  secDF <- secBC %>%
    fortify(region = "Section") %>%
    rename(Eastings = long, Northings = lat, Section = group) %>%
    as_tibble()
  # Transform
  landSPDF <- spTransform(x = land, CRSobj = CRS(crsOut))
  # Clip the land to the buffer: big
  landSPDF <- crop(x = landSPDF, y = extBuff)
  # Convert to data frame
  landDF <- landSPDF %>%
    fortify(region = "id") %>%
    rename(Eastings = long, Northings = lat) %>%
    as_tibble()
  # TODO: Enable this when the spatial packages are updated
  # # Dissolve to region
  # regSPDF <- aggregate(
  #   x = secBC, by = list(Temp = secBC$SAR), FUN = unique, dissolve = TRUE
  # )
  # # Remove non-SAR areas
  # regSPDF <- regSPDF[regSPDF@data$SAR != -1, ]
  # # Convert to data frame
  # if(nrow(regSPDF@data) >=1) {
  #   regDF <- regSPDF %>%
  #   fortify(region = "SAR") %>%
  #   rename(Eastings = long, Northings = lat, Region = group) %>%
  #   as_tibble()
  # } else {
    regDF <- NULL
  # }
  # Build a list to return
  res <- list(
    secDF = secDF, secCentDF = secCentDF, landDF = landDF, extDF = extDF,
    extBuff = extBuff, xyRatio = xyRatio, regDF = regDF
  )
  # Return info
  return(res)
} # End ClipPolys function

# Function to crop (spatially) spawn
CropSpawn <- function(dat, yrs, ext, grp, reg, sa, sec) {
  # Filter spawn index
  dat <- dat %>%
    filter(
      Year >= yrs[1], Year <= yrs[2], Region %in% reg, StatisticalArea %in% sa,
      Section %in% sec
    )
  # Convert to a spatial object
  coordinates(dat) <- ~ Eastings + Northings
  # Give the projection
  crs(dat) <- CRS(crsSpawn)
  # Transform
  datSP <- spTransform(x = dat, CRSobj = CRS(crsOut))
  # Clip to extent
  datSP <- crop(x = datSP, y = ext)
  # Make a data frame
  dat <- as.data.frame(datSP, spatial = TRUE) %>%
    as_tibble() %>%
    rename(Eastings = x, Northings = y)
  # Error if all the spawn data are cropped
  if (nrow(dat) < 1) stop("No spawn data in this area.", call. = FALSE)
  # Light wrangling
  dat <- dat %>%
    mutate(Year = as.integer(Year)) %>%
    dplyr::select(
      Year, Region, StatisticalArea, Section, LocationCode, LocationName,
      SpawnNumber, Eastings, Northings, Latitude, Longitude, StartDate, EndDate,
      Length, Width, Method, SpawnIndex
    ) %>%
    arrange(Year, Region, StatisticalArea, Section, LocationCode)
  # Summarise spawns by location
  if ("loc" %in% grp) {
    dat <- dat %>%
      group_by(
        Region, StatisticalArea, Section, LocationCode, LocationName
      ) %>%
      summarise(
        Number = n(),
        Eastings = unique(Eastings), Northings = unique(Northings),
        Latitude = unique(Latitude), Longitude = unique(Longitude),
        StartDate = min(StartDate, na.rm = TRUE),
        EndDate = max(EndDate, na.rm = TRUE),
        Length = MeanNA(Length), Width = MeanNA(Width),
        Method = ifelse(length(unique(Method)) > 1, "Various", Method),
        SpawnIndex = MeanNA(SpawnIndex)
      ) %>%
      ungroup() %>%
      mutate(Number = as.integer(Number))
  } else { # End if summarising by location, otherwise
    # Format dates: month day
    dat <- dat %>%
      mutate(
        StartDate = format(as.Date(paste(StartDate, Year),
                                   format = "%j %Y"), "%b %d"),
        EndDate = format(as.Date(paste(EndDate, Year), format = "%j %Y"),
                         "%b %d")
      )
  }
  # Return the data
  return(dat)
} # End CropSpawn function

# Draw a circle
MakeCircle <- function(center = c(0, 0), radius = 1, nPts = 100) {
  # Stop if radius is negative
  if (radius < 0) {
    stop("Radius must be non-negative.", call. = FALSE)
  }
  # Vector of points
  tt <- seq(from = 0, to = 2 * pi, length.out = nPts)
  # X values (math!)
  xx <- center[1] + radius * cos(tt)
  # Y values (and geometry!)
  yy <- center[2] + radius * sin(tt)
  # Return the data (x and y for a circle)
  return(tibble(Eastings = xx, Northings = yy))
} # End MakeCircle function

# Wrangle to data table
WrangleDT <- function(dat, input, optPageLen, optDom, optNoData) {
  # Modify names and values for nicer printing
  res <- dat %>%
    select(-Eastings, -Northings) %>%
    mutate(LocationCode = as.character(LocationCode)) %>%
    rename(
      SAR = Region, "Statistical Area" = StatisticalArea,
      "Location code" = LocationCode, "Location name" = LocationName,
      "Spawn index (t)" = SpawnIndex
    )
  # If grouping by location
  if ("loc" %in% input) {
    # Rename and format
    res <- res %>%
      rename(
        "Start day of year" = StartDate, "End day of year" = EndDate,
        "Number of spawns" = Number, "Mean length (m)" = Length,
        "Mean width (m)" = Width, "Mean spawn index (t)" = "Spawn index (t)"
      ) %>%
      datatable(options = list(
        lengthMenu = list(c(15, -1), list("15", "All")),
        pageLength = optPageLen, dom = optDom,
        language = list(zeroRecords = optNoData)
      )) %>%
      formatRound(
        columns = c("Mean spawn index (t)", "Latitude", "Longitude"),
        digits = 3
      ) %>%
      formatRound(columns = c("Mean length (m)", "Mean width (m)"), digits = 0)
  } else { # End if grouping by location, otherwise
    # Format
    res <- res %>%
      rename(
        "Spawn number" = SpawnNumber, "Start date" = StartDate,
        "End date" = EndDate, "Length (m)" = Length, "Width (m)" = Width
      ) %>%
      datatable(options = list(
        lengthMenu = list(c(15, -1), list("15", "All")),
        pageLength = optPageLen, dom = optDom,
        language = list(zeroRecords = optNoData)
      )) %>%
      formatRound(
        columns = c("Spawn index (t)", "Latitude", "Longitude"), digits = 3
      ) %>%
      formatRound(columns = c("Length (m)", "Width (m)"), digits = 0)
  } # End if not grouping by location
  # Return the data
  return(res)
} # End WrangleDT function

# Insert simple citation
SimpleCite <- function(ref, parens = TRUE, trail = "") {
  # Format hyperlink and text
  dat <- paste("<a href=", ref$link, ">", ref$text, "</a>", sep = "")
  # Add parentheses if requested
  if (parens) dat <- paste("(", dat, ")", sep = "")
  # Add trailing formatting if any
  res <- paste(dat, trail, sep = "")
  # Return citation
  return(res)
} # End SimpleCite function

# Calculate sum if there are non-NA values, return NA if all values are NA
SumNA <- function(x, omitNA = TRUE) {
  # An alternate version to sum(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the sum.
  # If all NA, NA; otherwise, sum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- sum(x, na.rm = omitNA)
  )
  # Return the result
  return(res)
} # End SumNA function

# Calculate mean if there are non-NA values, return NA if all values are NA
MeanNA <- function(x, omitNA = TRUE) {
  # An alternate version to mean(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the mean.
  # If all NA, NA; otherwise, mean
  ifelse(all(is.na(x)),
    res <- NA,
    res <- mean(x, na.rm = omitNA)
  )
  # Return the result
  return(res)
} # End MeanNA function

##### Data #####

# Load spawn data, and aggregate by location code
spawn <- read_csv(file = spawnLoc, col_types = cols(), guess_max = 10000) %>%
  # spawn <- read_csv(file = paste(
  #   "https://pacgis01.dfo-mpo.gc.ca", "FGPPublic",
  #   "Pacific_Herring_Spawn_Index_Data",
  #   "Pacific_herring_spawn_index_data_EN.csv",
  #   sep = "/"
  # ),
  # col_types = cols(),
  # guess_max = 10000
  # ) %>%
  mutate(
    StartDate = yday(StartDate), EndDate = yday(EndDate),
    StatisticalArea = formatC(StatisticalArea, width = 2, format = "d",
                              flag = "0"),
    Section = formatC(Section, width = 3, format = "d", flag = "0"),
    Survey = ifelse(Year < pars$years$dive, "Surface", "Dive")
  ) %>%
  group_by(
    Year, Region, StatisticalArea, Section, LocationCode, LocationName,
    SpawnNumber
  ) %>%
  summarise(
    StartDate = unique(StartDate), EndDate = unique(EndDate),
    Length = unique(Length), Width = unique(Width),
    Method = unique(Method),
    Eastings = unique(Eastings), Northings = unique(Northings),
    Latitude = unique(Latitude), Longitude = unique(Longitude),
    SpawnIndex = SumNA(c(Surface, Macrocystis, Understory)),
    Survey = unique(Survey)
  ) %>%
  ungroup() %>%
  filter(!is.na(Eastings), !is.na(Northings)) %>%
  dplyr::select(
    Year, Region, StatisticalArea, Section, LocationCode, LocationName,
    SpawnNumber, StartDate, EndDate, Eastings, Northings, Latitude, Longitude,
    Length, Width, Method, SpawnIndex, Survey
  ) %>%
  replace_na(replace = list(Region = "Other")) %>%
  mutate(
    Region = factor(Region, levels = c(
      "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W", "Other"
    ))
  ) %>%
  arrange(Region, StatisticalArea, Section, LocationCode, Year) # %>%
# st_as_sf( coords=c("Longitude", "Latitude"), crs=4326 ) %>%
# st_transform( 3347 ) %>%
# as_Spatial()

# Get survey time periods
qPeriods <- spawn %>%
  mutate(Survey = factor(Survey, levels = c("Surface", "Dive"))) %>%
  group_by(Survey) %>%
  summarise(StartDate = min(Year), EndDate = max(Year)) %>%
  ungroup()

# Range of longitude and latitude in spawn data
rangeSI <- list(
  Lat = round(range(spawn$Latitude, na.rm = TRUE), digits = 2),
  Long = round(range(spawn$Longitude, na.rm = TRUE), digits = 2)
)

# Load the Section shapefile (has Statistical Areas and Regions)
secPoly <- st_read(locStocks, quiet = TRUE) %>%
  st_transform(crs = 3005) %>%
  as_Spatial()

# Load land polygon
landPoly <- st_read(locLand, quiet = TRUE) %>%
  st_transform(crs = 3005) %>%
  as_Spatial()
