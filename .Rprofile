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

