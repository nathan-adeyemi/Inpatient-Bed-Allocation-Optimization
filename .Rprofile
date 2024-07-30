# source("/home/adeyemi.n/renv/activate.R")

options(
  scipen = 999,
  digits = 6,
  warn = -1,
  languageserver.formatting_style = function(options) {
    styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
  }
)

# Utilities ----------------------------

suppressPackageStartupMessages({
  if(!interactive()){
    library(tidyverse)
  }
  library(jsonlite)
  library(data.table)
  library(openxlsx)
  library(writexl)
  library(tictoc)
  library(gtools)
  library(lubridate)
  library(stringdist)

  # Packages for Statistics/Bootstrapping/etc. ------------------------------
  library(fitdistrplus)
  library(boot)
  library(simpleboot)
  library(EnvStats)

  # Packages for Discrete Event Simulation ----------------------------------
  library(simmer)
  library(simtimer)

  # Packages for Parallel Processing ----------------------------------------
  library(doParallel)
  library(parallelly)
  library(jsonlite)
})

source(file = "src/Simulations/testbed_functions.R")
source(file = "src/r_utils.R")
source(file = "src/Simulations/ed_mh_simulation.R")

# source("renv/activate.R")
Sys.setenv(TERM_PROGRAM="vscode")
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}