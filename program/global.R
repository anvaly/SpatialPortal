# ----------------------------------------
# --          PROGRAM global.R          --
# ----------------------------------------
# USE: Global variables and functions
#
# NOTEs:
#   - All variables/functions here are
#     globally scoped and will be available
#     to server, UI and session scopes
# ----------------------------------------
library(assertthat)
library(dplyr)
library(tidyr)
library(glue)
library(canvasXpress)
library(stringr)
library(shinyjs)
library(rintrojs)

options(dplyr.summarise.inform = FALSE)

source('program/fxn/stBox_module.R')
source('program/fxn/supporting.R')
source('program/modules/walkThrough.R')

# -- Setup your Application --
set_app_parameters(title       = "BMS Spatial Portal",
                   titleinfo   = NULL,
                   loglevel    = "DEBUG",
                   showlog     = FALSE,
                   app_version = "2.0")

# login settings
g_require_password <- FALSE
g_password_file    <- "program/data/st_pub.pass"

# display defaults
g_top_n_sig_genes       <- 10
g_top_n_spot_genes      <- 3
g_dynamic_range_minimum <- 0  # q1-q99 min allowed range - use zero to allow all values

# metadata file names
g_sample_meta_file   <- "meta_data.txt"
g_signatures_file    <- "signatures.txt"
g_genes_file_prefix  <- "genes_"

# file locations
g_sample_directory   <- "program/data/samples"
g_metadata_directory <- "program/data"

# Domino testing
# g_sample_directory   <- '/mnt/brettc/spatial-transcriptomics/sample_files'
# g_metadata_directory <- '/mnt/brettc/spatial-transcriptomics/config_files'

g_walkthrough_data <- read.csv('program/data/walkthrough_data.csv',
                               comment.char     = '#',
                               stringsAsFactors = FALSE)
