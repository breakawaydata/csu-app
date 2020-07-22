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
text_card <- function(category, score, description, class, colors) {  
  uicard(
    p(
      class = "header",
      style = glue("
        color: {colors$header};
        border-bottom-color: {colors$header};
      "),
      category,
      span(
        class = "header--score",
        style = glue("color: {colors$score};"),
        score
      )
    ),
    p(
      class = "description",
      style = glue("color: {colors$description};"),
      paste(description)
    ),
    class = glue("text-card {class};"),
    style = glue("
      background-color: {colors$background} !important;
      border-color: {colors$border} !important;
    ")
  )
}