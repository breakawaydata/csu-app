library(modules)
library(shiny)
library(R6)
library(glue)
library(stringi)

# bodychart.R
context("Body Chart")

setwd("../../")
module <- use("modules/components/body_chart.R")

id <- "test-id"
widget <- module$bodyChart(id)

context("Widget creation")
test_that("Widget creation", {
  expect_equal(TRUE, is.R6(widget))
})

ui <- as.character(widget$ui("test-id"))

test_that("Widget UI", {
  expect_equal(FALSE, is.null(ui))

  expect_match(ui, glue::glue('class="body_chart {id}-grid"'))
  expect_match(ui, glue::glue('id="{id}"'))
  expect_match(ui, 'class="body background_body"')

  expect_equal(lengths(regmatches(ui, gregexpr('stat_value body_stat', ui))), 6)
})

test_that("Widget state", {
  expect_equal(TRUE, is.reactivevalues(widget$state))
  expect_equal(isolate(widget$state$id), id)
  expect_equal(TRUE, all(c("total", "left", "right") %in% isolate(names(widget$state$values))))
  expect_equal(isolate(widget$state$options$title), "Explosive Score")
})

context("Property value checks")
number_of_test_cases <- 10

lapply(seq_len(number_of_test_cases), function(index) {
  id <- stringi::stri_rand_strings(1, sample(5:15, 1))
  widget <- module$bodyChart(id)
  ui <- as.character(widget$ui(id))

  test_that("Exists", {
    expect_equal(TRUE, is.R6(widget))
  })
  test_that("ID", {
    expect_match(ui, glue::glue('id="{id}"'))
  })
})
