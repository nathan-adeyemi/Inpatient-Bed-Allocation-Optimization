<<<<<<< HEAD
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
<<<<<<< HEAD
<<<<<<< HEAD
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
=======
source("/home/adeyemi.n/renv/activate.R")
>>>>>>> 1487b4d (Commit Attempt 2)

options(
  scipen = 999,
  digits = 6,
  warn = -1,
  languageserver.formatting_style = function(options) {
    styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
  }
)

# Utilities ----------------------------
library(tidyverse, quietly = T)
=======
>>>>>>> d020c10 (Updated function descriptions for some of the DB_PSA functions.)
=======
if(!interactive()){
  library(tidyverse, quietly = T)
}
>>>>>>> a420328 (Git Repo updates)
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

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
setwd(file.path('.','MH_Simulation','Inpatient Bed Allocation Optimization'))
source(file.path('.','Code','functions.R'))
source(file.path("Simulations",'Minnesota MH Network Simulation.R'))
<<<<<<< HEAD
source('MOSA Functions.R')
res_dir <-
  file.path(".", "Data", "Sample MOSA Results") |>  
<<<<<<< HEAD
    {function(i) file.path(i, list.files(i)[length(list.files(i))])}()

>>>>>>> cd7e8ac (Initial Commit)
=======
    {function(i) file.path(i, list.files(i)[length(list.files(i))])}() 
>>>>>>> 8a86754 (Center and scale the Bhattacharya distances of the Pareto set to give usable probabilities of selection.)
=======
source(file.path('.','Code','MOSA Functions.R'))
>>>>>>> dc52996 (Minor update: removed files and modified the .Rprofile)
=======
lapply(X = list.files(path = "./Functions"), FUN = source)
=======
lapply(X = file.path('.','Functions',list.files(path = file.path('.','Functions'))), FUN = source)
>>>>>>> 8c8946d (Fixed some custom functions.)
=======
invisible(lapply(
  X = file.path('.', 'Functions', list.files(path = file.path('.', 'Functions'))),
  FUN = source,
  echo = FALSE
))
>>>>>>> 2d8d6de (Further Updates)
source(file = file.path(".", 'Simulations', 'Minnesota MH Network Simulation.R'))
>>>>>>> 1487b4d (Commit Attempt 2)
