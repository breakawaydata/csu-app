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
text_card <- function(category, score, description) {  
  uicard(
    div(class = "header", glue::glue("{category}: {score}")),
    div(class = "description", paste(description)),
    class = "text-card"
  )
}