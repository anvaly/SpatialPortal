# ----------------------------------------
# --      PROGRAM server_global.R       --
# ----------------------------------------
# USE: Server-specific variables and
#      functions for the main reactive
#      shiny server functionality.  All
#      code in this file will be put into
#      the framework outside the call to
#      shinyServer(function(input, output, session)
#      in server.R
#
# NOTEs:
#   - All variables/functions here are
#     SERVER scoped and are available
#     across all user sessions, but not to
#     the UI.
#
#   - For user session-scoped items
#     put var/fxns in server_local.R
#
# FRAMEWORK VARIABLES
#     none
# ----------------------------------------

# -- IMPORTS --
library(Seurat)


# -- VARIABLES --

sg_samples           <- read_samples_data()
sg_st_objects        <- build_st_data_objects(samples = sg_samples)
sg_signatures        <- read_signatures()

sg_signature_choices <- get_all_signature_names(signatures = sg_signatures)
sg_genes_choices     <- get_organism_genes(samples = sg_samples)



# -- FUNCTIONS --
