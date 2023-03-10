<<<<<<< HEAD
<<<<<<< HEAD
source("/home/adeyemi.n/renv/activate.R")

options(
  scipen = 999,
  digits = 6,
  warn = -1,
  languageserver.formatting_style = function(options) {
    styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
  }
)

# Utilities ----------------------------
# library(tidyverse, quietly = T)
if(!interactive()){
  library(tidyverse, quietly = T)
}
library(data.table, quietly = T)
library(readxl, quietly = T)
library(openxlsx, quietly = T)
library(writexl, quietly = T)
library(tictoc, quietly = T)
library(gtools, quietly = T)
library(ps, quietly = T)
library(lubridate, quietly = T)
library(pracma, quietly = T)
library(dplyr,quietly = T)
library(stringr,quietly = T)

# Packages for Statistics/Bootstrapping/etc. ------------------------------
library(fitdistrplus, quietly = T)
library(boot, quietly = T)
library(simpleboot, quietly = T)
library(EnvStats, quietly = T)

# Packages for Discrete Event Simulation ----------------------------------
library(simmer, quietly = T)
library(simmer.plot, quietly = T)
library(simtimer, quietly = T)

# Packages for Parallel Processing ----------------------------------------
library(doParallel, quietly = T)
library(pbmcapply, quietly = T)
library(parallelly, quietly = T)

# Packages for DB-PSA -------------------------------------------------------
library(labdsv, quietly = T)
library(mco, quietly = T)
library(scatterplot3d, quietly = T)
library(nsga2R, quietly = T)
library(cramer, quietly = T)
library(bayesmeta, quietly = T)
library(fpc, quietly = T)
library(rslurm, quietly = T)
library(ggrepel,quietly = T)
library(optimization,quietly = T)
library(partitions,quietly = T)

invisible(lapply(
  X = file.path('.', 'Functions', list.files(path = file.path('.', 'Functions'))),
  FUN = source,
  echo = FALSE
))
source(file = file.path(".", 'Simulations', 'Minnesota MH Network Simulation.R'))
=======
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
=======
setwd("/home/adeyemi.n/")
>>>>>>> 8a86754 (Center and scale the Bhattacharya distances of the Pareto set to give usable probabilities of selection.)
source("renv/activate.R")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
Sys.setenv(TERM_PROGRAM="vscode")
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}

options(scipen = 999,
        digits = 6,
        warn = -1)

# Utilities ----------------------------
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(gtools))
suppressPackageStartupMessages(library(ps))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(pracma))

# Packages for Statistics/Bootstrapping/etc. ------------------------------
suppressPackageStartupMessages(library(fitdistrplus))
suppressPackageStartupMessages(library(boot))
suppressPackageStartupMessages(library(simpleboot))
suppressPackageStartupMessages(library(EnvStats))

# Packages for Discrete Event Simulation ----------------------------------
suppressPackageStartupMessages(library(simmer))
suppressPackageStartupMessages(library(simmer.plot))
suppressPackageStartupMessages(library(simtimer))

# Packages for Parallel Processing ----------------------------------------
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(pbmcapply))
suppressPackageStartupMessages(library(parallelly))

# Packages for MOSO -------------------------------------------------------
suppressPackageStartupMessages(library(labdsv))
suppressPackageStartupMessages(library(mco))
suppressPackageStartupMessages(library(scatterplot3d))
suppressPackageStartupMessages(library(nsga2R))
suppressPackageStartupMessages(library(cramer))
suppressPackageStartupMessages(library(bayesmeta))
suppressPackageStartupMessages(library(fpc))

setwd("/home/adeyemi.n/MH_Simulation/Inpatient Bed Allocation Optimization")
source(file.path('functions.R'))
source(file.path("Simulations",'Minnesota MH Network Simulation.R'))
source('MOSA Functions.R')
res_dir <-
  file.path(".", "Data", "Sample MOSA Results") |>  
<<<<<<< HEAD
    {function(i) file.path(i, list.files(i)[length(list.files(i))])}()

>>>>>>> cd7e8ac (Initial Commit)
=======
    {function(i) file.path(i, list.files(i)[length(list.files(i))])}() 
>>>>>>> 8a86754 (Center and scale the Bhattacharya distances of the Pareto set to give usable probabilities of selection.)
