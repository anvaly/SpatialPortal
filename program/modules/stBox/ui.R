#' stBoxUI
#'   Create a UI box for visualizing a spatial transcriptomics sample
#'
#' @param id - character - id to use in the module's namespace
#'
#' @return shiny.tag
stBoxUI <- function(id) {
    ns <- NS(id)

    tags$div(
        id = ns("stBoxContainerDiv"),
        useShinyjs(),
        box(id          = ns("box"),
            title       = textOutput(ns("boxTitle")),
            status      = "primary",
            width       = 12,
            collapsible = TRUE,
            collapsed   = FALSE,
            style       = "padding-bottom: 25px;",
            tags$table(width = "100%",
                       style = "border:0; table-layout:fixed;",
                       tags$tr(
                           tags$td(
                               style = "width:20%; vertical-align:top; border-right-style:groove; padding:0 2em 0em 1em;",
                               tags$div(style = "min-width:220px;",
                                        selectizeInput(
                                            inputId  = ns("datasetSel"),
                                            label    = uiOutput(ns("organismTooltip")),
                                            width    = "100%",
                                            choices  = NULL,
                                            multiple = FALSE,
                                            options  = list(placeholder = "Type/Click then Select")),
                                        selectizeInput(
                                            inputId  = ns("sampleSel"),
                                            label    = uiOutput(ns("sampleTooltip")),
                                            width    = "100%",
                                            choices  = NULL,
                                            multiple = FALSE,
                                            options  = list(placeholder = "Type/Click then Select")),
                                        tags$div(style = "margin-left: 10%",
                                                 tags$span(class = "st-signature-selectize",
                                                           selectizeInput(inputId  = ns("signatureSel"),
                                                                          label    = uiOutput(ns("signatureTooltip")),
                                                                          width    = "100%",
                                                                          choices  = NULL,
                                                                          multiple = FALSE)
                                                 ),
                                                 tags$h5(style = "width: 100%; text-align: center;
                                                             border-bottom: 1px solid #000;
                                                             line-height: 0.1em; margin: 6% 0 5%;",
                                                         tags$span("OR", style = "background:#fff; padding:0 10px;")),
                                                 selectizeInput(
                                                     inputId  = ns("singleGeneSel"),
                                                     label    = uiOutput(ns("geneTooltip")),
                                                     width    = "100%",
                                                     choices  = NULL,
                                                     multiple = FALSE,
                                                     options  = list(placeholder = "Type/Click then Select"))
                                        ),
                                        tags$br(),
                                        tags$span(bsButton(inputId = ns("closeBoxBtn"),
                                                           label   = "Close"),
                                                  bsButton(inputId = ns("cloneButton"),
                                                           label   = "Clone")),
                                        tags$span(style = "float:right;",
                                                  bsButton(inputId = ns("getPlotBtn"),
                                                           label   = tags$div("Update", icon("arrow-right")),
                                                           style   = "primary")),
                                        shinyjs::hidden(
                                            tags$div(id = ns("selectionFeedbackDiv"),
                                                     style = "margin-top: 1px; color: red; width:100%; text-align: end; float: left",
                                                     tags$b(icon("exclamation-circle"),
                                                            "Gene Signature and Single Gene",
                                                            tags$br(),
                                                            "cannot both be selected or blank"))),
                                        tags$br(),
                                        tags$table(style = "width: 100%;",
                                                   tags$tr(
                                                       tags$td(
                                                           tags$h5(style = "text-align: center;
                                                                            border-bottom: 2px solid #000;
                                                                            line-height: 0.1em; margin: 10% 0 8%;",
                                                                   tags$span(style = "background:#fff; padding:0 10px; font-weight: bold;",
                                                                             "View Toggle")
                                                           ))),
                                                   tags$tr(
                                                       tags$td(
                                                           tags$div(style = "width:100%; display: inline-flex; justify-content: center;",
                                                                    bsButton(inputId = ns("viewToggleBtn_std"),
                                                                             label   = "Overview",
                                                                             type    = "toggle",
                                                                             class   = "viewToggleBtn",
                                                                             value   = TRUE),
                                                                    bsButton(inputId = ns("viewToggleBtn_adv"),
                                                                             label   = "Extended",
                                                                             type    = "toggle",
                                                                             class   = "viewToggleBtn",
                                                                             value   = FALSE))))
                                        )
                               )
                           ),
                           tags$td(
                               style = "vertical-align:top; padding-left:20px;",
                               tabsetPanel(
                                   id = ns("viewTypeTabs"),
                                   type = "hidden",
                                   tabPanelBody(
                                       value = "Regular",
                                       tags$table(
                                           tags$td(
                                               style = "vertical-align:top; width:300px;",
                                               tags$h4(class = "text-primary", "Sample: "),
                                               tags$h5(style = "margin-left:1em; margin-right:1em;",
                                                       textOutput(outputId = ns("infoSample"))),
                                               tags$p(style = "padding:5px;"),
                                               tags$h4(class = "text-primary",
                                                       textOutput(outputId = ns("signatureGeneLbl"))),
                                               tags$h5(style = "margin-left:1em; margin-right:1em;",
                                                       textOutput(outputId = ns("infoSignature"))),
                                               tags$p(style = "padding:5px;"),
                                               tags$h4(class = "text-primary",  style = "white-space:nowrap;",
                                                       "Top Represented Genes: "),
                                               tags$h5(style = "margin-left:1em; margin-right:1em;",
                                                       htmlOutput(outputId = ns("infoGenes")))
                                           ),
                                           tags$td(
                                               style = "vertical-align:center; padding-left:25px; padding-right:25px; width:100%; align:center;",
                                               tags$div(style = "display:flex; height:450px; justify-content:space-between;",
                                                        uiOutput(outputId = ns("spatialPlot"), inline = FALSE),
                                                        tags$div(style = "display:flex; flex-direction:column; justify-content:center;
                                                                          text-align:left; padding-left:25px; width:300px;",
                                                                 shinyjs::hidden(
                                                                     radioButtons(inputId = ns("plotScalingRadio"),
                                                                                  label   = uiOutput(ns("plotScalingRadioTT")),
                                                                                  choices = c("Native"  = "native",
                                                                                              "Trimmed" = "trim",
                                                                                              "Fixed"   = "center",
                                                                                              "Custom"  = "custom"),
                                                                                  selected = "native")),
                                                                 shinyjs::hidden(
                                                                     splitLayout(
                                                                         id       = ns("customScalingLayout"),
                                                                         style    = "padding-left:20px;",
                                                                         cellArgs = list(style = "vertical-align:bottom;text-align:center;"),
                                                                         textInput(inputId     = ns("customScaling1"),
                                                                                   label       = "from",
                                                                                   value       = "",
                                                                                   placeholder = "0.5"),
                                                                         textInput(inputId     = ns("customScaling2"),
                                                                                   label       = "to",
                                                                                   value       = "",
                                                                                   placeholder = "1.5"),
                                                                         actionButton(inputId = ns("customScalingBtn"),
                                                                                      label   = "",
                                                                                      icon    = icon("sync"),
                                                                                      style   = "margin-bottom:10px;overflow-hidden;")))
                                                        )
                                               )
                                           )#td
                                       )#table
                                   ),
                                   tabPanelBody(
                                       value = "Advanced",
                                       uiOutput(outputId = ns("spatialPlotAdvanced"),
                                                inline   = FALSE),
                                   )
                               )
                           )#td
                       )
            )#table
        )
    )
}
