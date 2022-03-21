# ----------------------------------------
# --          PROGRAM ui_body.R         --
# ----------------------------------------
# USE: Create UI elements for the
#      application body (right side on the
#      desktop; contains output) and
#      ATTACH them to the UI by calling
#      add_ui_body()
#
# NOTEs:
#   - All variables/functions here are
#     not available to the UI or Server
#     scopes - this is isolated
# ----------------------------------------

# -- IMPORTS --
library(shinyFeedback)

# ----------------------------------------
# --      BODY ELEMENT CREATION         --
# ----------------------------------------

# -- Create Elements

headerChanges <- tags$head(
    introjsUI(),
    useShinyFeedback(),
    tags$style(
        "#walkthroughBtn-button {width:52px; height:45px;}",
        ".fa-info-circle {font-size:18px;}",
        ".st-signature-selectize .selectize-dropdown-content {max-height:50vh;}",
        ".tab-content {margin:0px;padding:0px;}",
        ".viewToggleBtn {border-radius:0px;}"))

topElements <- tags$table(id    = "searchWidget",
                          width = "99%",
                          tags$tr(width = "100%",
                                  tags$td(width = "5%",
                                          style = "text-align:center; vertical-align:top;",
                                          bsButton(inputId = "newBoxButton",
                                                   label   = "",
                                                   size    = "large",
                                                   icon    = icon("plus")),
                                          bsTooltip(id        = "newBoxButton",
                                                    title     = "Create a new UI Box Element",
                                                    placement = "bottom")),
                                  tags$td(width = "90%",
                                          box(id          = "aboutBox",
                                              title       = "About This Application",
                                              width       = "100%",
                                              collapsible = TRUE,
                                              collapsed   = TRUE,
                                              read_about())),
                                  tags$td(width = "5%",
                                          style = "text-align:right; vertical-align:top;",
                                          walkThroughButton("walkthroughBtn",
                                                            label     = "",
                                                            hovertext = "Launch the Walkthrough"))
                          ))

boxList <- tagList(
    fluidRow(
        column(width = 12,
               tags$div(id = "stBoxList")))
)


# -- Register Elements in the ORDER SHOWN in the UI
add_ui_body(list(headerChanges, topElements, boxList))
