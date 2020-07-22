add_one <- function(value) {
  value + 1
}

text_card <- function(header, description) {
  
  uicard(
    div(class = "header", paste(header)),
    div(class = "description", paste(description)),
    class = "text-card"
  )
}