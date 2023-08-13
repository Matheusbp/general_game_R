library("dplyr")
library("shinydashboard")
library("shinyjs")
library("gt")
library("tidyverse")

# auxiliar objects
players <- c()

n_jogada_dado <- 0

escolhas <- c()

aux_inicio_server <- 0

aux_players <- 0

aux_inicio_jogo <- 0
aux_total_pts <- c()

tosum <- c()

primeira_jogada <- 0


# list of dices' choices
lista_choicenames <- list(1, 2, 3, 4, 5)

# list with play icons
lista_repeat <- list(1, 2, 3, 4, 5)



inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}

# functions
`%notin%` <- Negate(`%in%`)

fixar <- function(x) {
  x_sorted <- sort(x)
  if (1 %notin% x_sorted) {
    d1 <<- sample(6, 1)
  }
  if (2 %notin% x_sorted) {
    d2 <<- sample(6, 1)
  }
  if (3 %notin% x_sorted) {
    d3 <<- sample(6, 1)
  }
  if (4 %notin% x_sorted) {
    d4 <<- sample(6, 1)
  }
  if (5 %notin% x_sorted) {
    d5 <<- sample(6, 1)
  }

  output_dados <- c(d1, d2, d3, d4, d5)

  output_dados
}

# creating the main table
nome_linhas <- as.character(c(1:6, "S", "F", "Q", "G", "G*", "Total"))

folha_aux <- data.frame(
  Lines = nome_linhas,
  "Player" = rep("", length(nome_linhas))
)

table_colors <- blues9

# function to generate the gt table

folha_generator <- function(df) {
  folha <- df %>%
    gt() |>
    tab_header(
      title = md("General"),
    ) %>%
    tab_source_note(md("Made by Matheus B. Pamplona")) |>
    # changing tables color
    tab_options(
      table.background.color = table_colors[1],
      stub.background.color = table_colors[1],
      source_notes.background.color = table_colors[9],
      heading.background.color = table_colors[9],
      column_labels.background.color = table_colors[8]
    ) |>
    # all bold in lines table
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[2])
      ),
      locations = cells_body(
        rows = Lines == "S"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[3])
      ),
      locations = cells_body(
        rows = Lines == "F"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[4])
      ),
      locations = cells_body(
        rows = Lines == "Q"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[4])
      ),
      locations = cells_body(
        rows = Lines == "G"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[5])
      ),
      locations = cells_body(
        rows = Lines == "G*"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = table_colors[6])
      ),
      locations = cells_body(
        rows = Lines == "Total"
      )
    )

  return(folha)
}
