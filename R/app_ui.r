#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {


library("shiny")
library("shinyjs")
inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}

 fluidPage(
  fluidRow(
    # p funcionar o hide/show
    useShinyjs(),
    titlePanel("General"),
    column(
      3,
      h3("Table"),
      tableOutput("table")
    ),
    column(
      6,
      inline(textInput("jogador",
        label = h4("Player name (s):")
      )),
      inline(actionButton(
        inputId = "addplayerbutton",
        label = "Add player"
      )),
      inline(actionButton(
        inputId = "readytoplay",
        label = "Let's roll"
      )),
      tags$div(id = "placeholder"),
      tags$div(id = "daquiprabaixotabela"),
    )
  )
 )

}
