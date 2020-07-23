import("R6")
import("utils")
import("shiny")
import("glue")
import("dplyr")
import("htmltools")
import("shiny.grid")
import("shiny.semantic")

export("text_card")

#' Creates text card with header and description
#'
#' @param header Header text at the top of the card
#' @param description Card's description
#'
#' @return shiny.semantic uicard element
text_card <- function(category,
                      score,
                      description,
                      area,
                      class,
                      background_color = "#d6d6d6",
                      border_color = "",
                      header_color = "",
                      score_color = "",
                      description_color = "") {

  uicard(
    p(
      class = "header",
      style = glue("color: {header_color}; border-bottom-color: {header_color};"),
      category,
      span(class = "header--score", style = glue("color: {score_color};"), score)
    ),
    p(
      class = "description",
      style = glue("color: {description_color};"),
      paste(description)
    ),
    class = glue("text-card {class};"),
    style = glue("
      background-color: {background_color} !important;
      border-color: {border_color} !important;
    ")
  )
}