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
        ".selectize-dropdown [data-selectable],",
        ".selectize-dropdown .optgroup-header {padding: 3px 12px;}",
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

loginModal <- bsModal(id      = "loginModal",
                      title   = "BMS Spatial Portal",
                      trigger = NULL,
                      size    = "small",
                      tags$head(tags$script("$(document).ready(function(){$('#loginModal').modal({backdrop: 'static', keyboard: false});});"),
                                tags$style("#loginModal .modal-header button.close{display:none;}"),
                                tags$style("#loginModal .modal-title {font-weight:bold;}"),
                                tags$style("#loginModal .modal-footer {display:none;}")),
                      tags$p(style = "font-weight: bold; text-align: center;",
                              "Pre-Release Password Required"),
                      tags$p("A password is required to access this application prior to the publication date for the paper containing the data.  Please contact the collaborators you are working with for the password if you do not know it."),
                      passwordInput(inputId     = "password",
                                    label       = NULL,
                                    value       = "",
                                    width       = "100%",
                                    placeholder = "Password"),
                      bsButton(inputId = "loginButton",
                               label   = "Continue",
                               width   = "100%"))

appBody <- if (g_require_password) {
    tagList(loginModal,
            conditionalPanel(condition = "output.logged_in",
                             topElements,
                             boxList))
} else {
    tagList(topElements, boxList)
}


# -- Register Elements in the ORDER SHOWN in the UI
add_ui_body(list(headerChanges, appBody))
