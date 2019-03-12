# Find Pacific Herring spawn index sites

Find Pacific Herring spawn index sites around a point.
By default, spawns are summarised by year and Location in tonnes (t).
Alternatively, users can choose to summarise spawns by Location only (i.e., aggregate spawns over years).
Users can view spawns as a figure and as a table, both of which can be downloaded.
For more information on Pacific Herring spawn data, contact
[Jaclyn Cleary](mailto:Jaclyn.Cleary@dfo-mpo.gc.ca),
[Matt Grinnell](mailto:Matthew.Grinnell@dfo-mpo.gc.ca), or
[Matt Thompson](mailto:Matthew.Thompson@dfo-mpo.gc.ca),
DFO Science, Pacific Biological Station.
Grinnell et al.
([In prep.](https://github.com/grinnellm/HerringSpawnDocumentation/blob/master/SpawnIndexTechnicalReport.pdf))
has details on how we calculate the Pacific Herring spawn index.
Note that the 'spawn index' represents the raw survey data only,
and is not scaled by the spawn survey scaling parameter *q*
([CSAS 2018](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2018/2018_002-eng.html));
therefore it is a relative index of spawning biomass.

## Launch the app

**FIND** (FIND Is Not Difficult) is a [Shiny](https://shiny.rstudio.com/) app.
Launch the app in a few simple steps.
First, install [RStudio](https://www.rstudio.com/) or [R](https://www.r-project.org/).
Second, install the **R** package "shiny" `install.packages( pkgs="shiny" )`.
Finally, launch **FIND** in one of two ways:

1. Clone or download the [FIND](https://github.com/grinnellm/FIND) repository from **GitHub**, and either 
	* Click "Run App" in **RStudio**, or
	* Run the following command in the **R** console `shiny::runApp( )`, or
2. Run the following command in the **R** console `shiny::runGitHub( repo="FIND", username="grinnellm" )`.
