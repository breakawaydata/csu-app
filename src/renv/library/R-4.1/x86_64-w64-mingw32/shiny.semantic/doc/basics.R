## ----setup, include = FALSE, screenshot.force = FALSE-------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(shiny)
library(shiny.semantic)

## ----echo=T, include = T, eval = FALSE, screenshot.force = FALSE--------------
#  div(class = "ui raised segment",
#    div(
#      a(class="ui green ribbon label", "Link"),
#      p("Lorem ipsum, lorem ipsum, lorem ipsum"),
#      actionButton("button", "Click")
#    )
#  )

## ----echo=F, include = T, screenshot.force = FALSE----------------------------
uirender(
  div(class = "ui raised segment",
    div(
      a(class="ui green ribbon label", "Link"),
      p("Lorem ipsum, lorem ipsum, lorem ipsum"),
      actionButton("button", "Click")
    )
  ),
width = "400px", height = "150px")

## ----echo = T, include = T, eval = FALSE, screenshot.force = FALSE------------
#  range_input("range", 10, 15, 0, 20)
#  text_input("txt", type = "text", placeholder = "Enter Text")
#  date_input("date")

## ----echo = T, include = T, eval = FALSE, screenshot.force = FALSE------------
#  ?counter_button

