# Find Pacific Herring spawns

Find Pacific Herring spawn sites in a given area.
In this analysis, we summarise spawns by year and Location in tonnes (t).
Alternatively, users can choose to summarise spawns by Location only (i.e., aggregate spawns over years).
Users can view spawns as a figure and as a table, both of which can be downloaded.
For more information on Pacific Herring spawn data, contact
[Jaclyn Cleary](mailto:Jaclyn.Cleary@dfo-mpo.gc.ca),
[Matthew Grinnell](mailto:Matthew.Grinnell@dfo-mpo.gc.ca), or
[Matt Thompson](mailto:Matthew.Thompson@dfo-mpo.gc.ca),
DFO Science, Pacific Biological Station.
([Grinnell et al. 2023](https://science-catalogue.canada.ca/record=b4121678~S6))
has details on how we calculate the Pacific Herring spawn index.
Note that the 'spawn index' is not scaled by the spawn survey scaling parameter *q*
([CSAS 2018](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2018/2018_002-eng.html));
therefore it is a relative index of spawning biomass.

## Launch the app

**FIND** (FIND Is Not Difficult) is a [Shiny](https://shiny.rstudio.com/) app.
Launch the app in a few simple steps.
First, install [RStudio](https://www.rstudio.com/) or [R](https://www.r-project.org/).
Second, install the **R** package "shiny" `install.packages(pkgs = "shiny")`.
Finally, launch **FIND** in one of two ways (the first option is easier):

1. Run the following command in the **R** console `shiny::runGitHub(repo = "FIND", username = "grinnellm")`, or
2. Clone or download the [FIND](https://github.com/grinnellm/FIND) repository from **GitHub**, and then either 
	* Click "Run App" in **RStudio**, or
	* Run the following command in the **R** console `shiny::runApp()`.
