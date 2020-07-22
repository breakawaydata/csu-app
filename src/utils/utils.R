add_one <- function(value) {
  value + 1
}

text_card <- function(header, content) {
  
  uicard(div(
    class = "content",
    div(class = "header", paste(header)),
    div(class = "description", paste(content))
  ), class = "text-card")
}