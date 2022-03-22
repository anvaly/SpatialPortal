# ----------------------------------------
# --       PROGRAM server_local.R       --
# ----------------------------------------
# USE: Session-specific variables and
#      functions for the main reactive
#      shiny server functionality.  All
#      code in this file will be put into
#      the framework inside the call to
#      shinyServer(function(input, output, session)
#      in server.R
#
# NOTEs:
#   - All variables/functions here are
#     SESSION scoped and are ONLY
#     available to a single session and
#     not to the UI
#
#   - For globally scoped session items
#     put var/fxns in server_global.R
#
# FRAMEWORK VARIABLES
#     input, output, session - Shiny
#     ss_userAction.Log - Reactive Logger S4 object
# ----------------------------------------

# -- IMPORTS --
library(uuid)


# -- VARIABLES --
userSel           <- reactiveValues()
userSel$boxIds    <- list()
userSel$caller_id <- NULL
userSel$logged_in <- FALSE

# -- FUNCTIONS --


#' remove_shiny_inputs
#'   Remove spatial visualization box from session's input object. This function is called when the user closes a box.
#'
#' @param id    - id of box to remove
#' @param input - shiny session's input object
#'
#' @return list
remove_shiny_inputs <- function(id, input) {
    lapply(grep(id, names(input), value = TRUE), function(i) {
        .subset2(input, "impl")$.values$remove(i)
    })
}

# -- MODULES --
callModule(walkThrough, "walkthroughBtn", ss_userAction.Log,
           steps   = add_title_to_intro(intro_dt = g_walkthrough_data),
           options = list('nextLabel' = 'Next',
                          'prevLabel' = 'Prev',
                          'doneLabel' = 'Exit',
                          'skipLabel' = 'Exit'))

# -- REACTIVE FUNCTIONS --
add_box <- reactive({
    caller_id <- userSel$caller_id
    if (!is.null(caller_id)) {
        datasetSel     <- input[[glue("{caller_id}-datasetSel")]]
        signatureSel   <- input[[glue("{caller_id}-signatureSel")]]
        sampleSel      <- input[[glue("{caller_id}-sampleSel")]]
        singleGeneSel  <- input[[glue("{caller_id}-singleGeneSel")]]
        userSel$caller_id <- NULL
    } else {
        datasetSel    <- character(0)
        signatureSel  <- character(0)
        sampleSel     <- character(0)
        singleGeneSel <- character(0)
    }

    box_id         <- glue("box-{UUIDgenerate()}")
    userSel$boxIds <- c(userSel$boxIds, box_id)

    insertUI(selector  = "#stBoxList",
             where     = "afterBegin",
             ui        = stBoxUI(box_id),
             immediate = TRUE)
    stBoxServer(box_id,
                sg_genes_choices,
                sg_signature_choices,
                sg_signatures,
                sg_samples,
                sg_st_objects,
                datasetSel,
                signatureSel,
                sampleSel,
                singleGeneSel,
                ss_userAction.Log)

    observeEvent(input[[glue("{box_id}-closeBoxBtn")]], {
        removeUI(selector  = glue("#{box_id}-stBoxContainerDiv"),
                 immediate = TRUE)
        remove_shiny_inputs(box_id, input)
        gc()
        loginfo("Removed spatial visualization box from list", logger = ss_userAction.Log)
    })

    observeEvent(input[[glue("{box_id}-cloneButton")]], {
        userSel$caller_id <- box_id
        add_box()
    })

    return(box_id)
})


# ----------------------------------------
# --          SHINY SERVER CODE         --
# ----------------------------------------


# Initialization Only
observeEvent(TRUE, {
    add_box()
    loginfo(glue("Application started with {NROW(unique(sg_samples$Organism))} organisms, {NROW(sg_signatures)} signatures and {NROW(sg_samples)} samples"),
            logger = ss_userAction.Log)
}, once = TRUE)

observeEvent(input$loginButton, {
    valid_password <- !is.null(input$password) && (input$password == sg_password)

    feedbackDanger("password",
                   !valid_password,
                   "Incorrect password")

    if (valid_password) {
        userSel$logged_in <- TRUE
        toggleModal(session, "loginModal", "close")
    } else {
        updateTextInput(inputId = "password", value = "")
    }
})

observeEvent(input$newBoxButton, {
    add_box()
    loginfo("Added new spatial visualization box to list", logger = ss_userAction.Log)
})

output$logged_in <- reactive({
    userSel$logged_in
})

outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
