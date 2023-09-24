library(shiny)
ui <- fluidPage(
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
