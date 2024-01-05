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
if(!interactive()){
  library(tidyverse, quietly = T)
}
library(data.table, quietly = T)
# library(EnvStats, quietly = T)

# Packages for Discrete Event Simulation ----------------------------------
library(simmer, quietly = T)
library(simmer.plot, quietly = T)
library(simtimer, quietly = T)
library(parallel, quietly = T)
library(jsonlite, quietly = T)

source(file = file.path('modules', 'Simulations','ed_mh_simulation', 'ed_mh_simulation.R'))
source(file = file.path("modules", 'Simulations','testbeds', 'run_test_bench.R'))
invisible(lapply(
  X = file.path("modules", 'Simulations','testbeds','helper_functions', list.files(path = file.path("modules", 'Simulations','testbeds','helper_functions'))),
  FUN = source,
  echo = FALSE
))

# source("renv/activate.R")
Sys.setenv(TERM_PROGRAM="vscode")
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}