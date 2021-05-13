##### Header #####
#
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Stock Assessment and Research, Quantitative Assessment Methods
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matt.grinnell@dfo-mpo.gc.ca | tel: 250.756.7055
# Project:      Herring
# Code name:    FIND (FIND Is Not Difficult)
# Version:      1.0
# Date started: Jan 08, 2019
# Date edited:  May 13, 2021
#
# Overview:
# Show herring spawn events within a given distance from a point.
#
# Requirements:
# RStudio, various R packages, herring spawn index data, as well as land and
# herring section shapefiles.
#
# Notes:
# This is a Shiny web application; run it by clicking the 'Run App' button in
# RStudio.

# Run the application
shinyApp(ui = ui, server = server)
