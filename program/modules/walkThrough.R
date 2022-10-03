# WalkThrough Shiny Module --

#' WalkThroughButton UI
#'
#' Creates a custom button to start a walkthrough.
#'
#' @param id character id for the object
#' @param label label for the button
#' @param hovertext tooltip hover text
#'
#' @section Example:
#' \code{walkThroughButton("mybuttonID1", "My Walkthrough", "Start the walkthrough!")}
#'
#' @section Shiny Usage:
#' Call this function at the place in ui.R where the button should be placed.
#'
#' It is paired with a call to \code{walkThrough(id, ...)}
#' in server.R
#'
#' @export
walkThroughButton <- function(id,
                              label,
                              hovertext = NULL) {
    ns <- shiny::NS(id)

    shiny::span(
        shiny::actionButton(inputId  = ns("button"),
                            label    = label,
                            icon     = shiny::icon("info-circle")),
        shinyBS::bsTooltip(id        = ns("button"),
                           title     = hovertext,
                           placement = "top"))
}


#' WalkThrough Module
#'
#' Server-side function for the walkThroughButton  This is a custom
#' button to start a walkthrough.
#' The server function is used to provide the input for the walkthrough.
#'
#' @param ... shiny module default inputs (input, output, session)
#' @param logger logger to use
#' @param steps a data.frame [or function/reactive expression providing a data.frame] providing the
#' text for the steps
#' @param options list of configuration options, see \href{https://introjs.com/docs/intro/options/}{introjs}
#' @param events list of events, NULL by default. Each event should have a name and a javascript function
#' that get's executed when the event is triggered. The javascript function must be wrapped in I()
#'
#' @section Shiny Usage:
#' This function is not called directly by consumers - it is accessed in
#' server.R using the same id provided in \code{walkThroughButton}:
#'
#' \strong{\code{walkThrough(id, logger, steps, options, events)}}
#'
#' @seealso \link[periscope.bms]{walkThroughButton}
#' @seealso \link[rintrojs]{introjs}
#'
#' @export
walkThrough <- function(...,
                        logger,
                        steps,
                        options,
                        events = NULL) {
    call <- match.call()
    params <- list(...)
    param_index <- 1
    params_length <- length(params)

    # get session parameters
    if (call[[1]] == "module") {
        input   <- params[[param_index]]
        param_index <- param_index + 1
        output  <- params[[param_index]]
        param_index <- param_index + 1
        session <- params[[param_index]]
        param_index <- param_index + 1
    } else {
        id <- params[[param_index]]
        param_index <- param_index + 1
    }

    # get rest of the function parameters
    if (missing(logger) && params_length >= param_index) {
        logger <- params[[param_index]]
        param_index <- param_index + 1
    }

    if (missing(steps) && params_length >= param_index) {
        steps <- params[[param_index]]
        param_index <- param_index + 1
    }

    if (missing(options) && params_length >= param_index) {
        options <- params[[param_index]]
        param_index <- param_index + 1
    }

    if (missing(events) && params_length >= param_index) {
        events <- params[[param_index]]
        param_index <- param_index + 1
    }

    if (call[[1]] == "module") {
        walk_through(input,
                     output,
                     session,
                     logger,
                     steps,
                     options,
                     events)
    }
    else {
        shiny::moduleServer(
            id,
            function(input, output, session) {
                walk_through(input,
                             output,
                             session,
                             logger,
                             steps,
                             options,
                             events)
            })
    }

}

walk_through <- function(input, output, session, logger,
                         steps, options, events = NULL) {

    shiny::observeEvent(input$button, {
        wt_steps <- steps
        if (inherits(steps, c("reactiveExpr", "function"))) {
            wt_steps <- steps()
        }
        options <- append(list(steps = wt_steps),
                          options)

        rintrojs::introjs(session = session,
                          options = options,
                          events  = events)
    })
}
