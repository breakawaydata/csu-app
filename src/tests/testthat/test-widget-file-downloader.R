library(modules)
library(shiny)
library(R6)
library(glue)
library(stringi)

# bodychart.R
context("File Generator")

setwd("../../")
module <- use("modules/components/file_generator.R")

id <- "test-id"
widget <- module$fileDownloader(id)

context("Widget creation")
test_that("Widget creation", {
  expect_equal(TRUE, is.R6(widget))
})

ui <- as.character(widget$ui())

test_that("Widget UI", {
  expect_equal(FALSE, is.null(ui))
  expect_match(ui, glue::glue('id="{id}"'))
})

test_that("Widget state", {
  expect_equal(TRUE, is.reactivevalues(widget$state))
  expect_equal(isolate(widget$state$id), id)
})

context("Property value checks")
number_of_test_cases <- 10

lapply(1:number_of_test_cases, function(index) {
  id <- stringi::stri_rand_strings(1, sample(5:15, 1))
  widget <- module$fileDownloader(id)
  ui <- as.character(widget$ui())

  test_that("Exists", {
    expect_equal(TRUE, is.R6(widget))
  })
  test_that("ID", {
    expect_match(ui, glue::glue('id="{id}"'))
  })
})
