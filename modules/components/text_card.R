import("shiny")
import("shiny.semantic")
import("glue")
export("text_card")

#' Creates text card with header and description
#'
#' @param category Category of the card in the card's header
#' @param score Score value related to the category
#' @param description Card's decription text
#' @param class Custom CSS class
#' @param background_color Custom color of card's background. Default = "#d6d6d6"
#' @param border_color Custom color of card's border.
#' @param header_color Custom font color of card's header
#' @param score_color Custom font color of score value in card's header
#' @param description_color Custom font color of description text
#'
#' @examples
#' text_card(
#'   "Example title",
#'   23,
#'   This is demo description,
#'   class = "custom-class",
#'   background_color = "#ccc",
#'   border_color = "darkgray",
#'   header_color = "tomato",
#'   score_color = "black",
#'   description_color = "white"
#' )
#'
#' @return shiny.semantic uicard element
#'
text_card <- function(category,
                      score,
                      description,
                      class,
                      background_color = "#d6d6d6",
                      border_color = "",
                      header_color = "",
                      score_color = "",
                      description_color = "") {
  card(
    p(
      class = "header",
      style = glue("color: {header_color}; border-bottom-color: {header_color};"),
      category,
      span(
        class = "header--score",
        style = glue("color: {score_color}; border-color: {header_color}"),
        score
      )
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
