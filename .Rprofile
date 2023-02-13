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
source("renv/activate.R")

options(scipen = 999,
        digits = 6,
        echo = F)

# Utilities ----------------------------
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(ps)
library(lubridate)
library(pracma)

# Packages for Statistics/Bootstrapping/etc. ------------------------------
library(fitdistrplus)
library(boot)
library(simpleboot)
library(EnvStats)

# Packages for Discrete Event Simulation ----------------------------------
library(simmer)
library(simmer.plot)
library(simtimer)

# Packages for Parallel Processing ----------------------------------------
library(doParallel)
library(pbmcapply)
library(parallelly)

# Packages for MOSO -------------------------------------------------------
library(labdsv)
library(mco)
library(scatterplot3d)
library(nsga2R)
library(cramer)
# library(ecr)

setwd("..")
source(file.path('functions.R'))
source(file.path("Simulations",'Minnesota MH Network Simulation.R'))
setwd('Inpatient Bed Allocation Optimization')
source('MOSA Functions.R')
res_dir <-
  file.path(".", "Data", "Sample MOSA Results") |>  
    {function(i) file.path(i, list.files(i)[length(list.files(i))])}()

>>>>>>> cd7e8ac (Initial Commit)
