setwd("/home/adeyemi.n/")
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

setwd(file.path('.','MH_Simulation','Inpatient Bed Allocation Optimization'))
source(file.path('.','Code','functions.R'))
source(file.path("Simulations",'Minnesota MH Network Simulation.R'))
source(file.path('.','Code','MOSA Functions.R'))
