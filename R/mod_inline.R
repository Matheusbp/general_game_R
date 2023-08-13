#' inline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inline_ui <- function(x) {
  tags$div(style = "display:inline-block;", x)
  tagList(

  )
}

## To be copied in the server
# mod_inline_server("inline_1")
