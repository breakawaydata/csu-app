library(modules)
library(shiny)
library(R6)
library(glue)
library(stringi)

# stat_chart.R
context("Stat Chart")

setwd("../../")
module <- use("modules/components/stat_chart.R")

id <- "test-id"
title <- "test-title"
icon <- "test-icon"
number_of_bars <- 5
widget <- module$statChart(id, title, icon, number_of_bars)

context("Widget creation")
test_that("Initialize stat chart widget", {
  expect_equal(TRUE, is.R6(widget))
})

ui <- as.character(widget$ui("test-id"))

test_that("Widget UI", {
  expect_equal(FALSE, is.null(ui))

  expect_match(ui, 'class="stats-progress-widget"')
  expect_match(ui, glue::glue('id="{id}"'))
  expect_match(ui, glue::glue('id="{id}-total_score"'))
  expect_match(ui, '<div class="chart_title">test-title</div>')
  expect_match(ui, '<img class="icon" src="test-icon"/>')

  # number of bars is correct
  expect_match(ui, glue::glue('id="{id}-bar_list"'))
  expect_equal(lengths(regmatches(ui, gregexpr('progress-bar', ui))), number_of_bars)

  lapply(seq_len(number_of_bars), function(index) {
    expect_match(ui, glue::glue('id="{id}-{index}"'))
  })
})

test_that("Widget state", {
  expect_equal(TRUE, is.reactivevalues(widget$state))
  expect_equal(isolate(widget$state$id), id)
  expect_equal(isolate(widget$state$options$title), title)
  expect_equal(isolate(widget$state$options$icon), icon)
  expect_equal(isolate(widget$state$options$bar_number), number_of_bars)
})

context("Property value checks")
number_of_test_cases <- 10

lapply(seq_len(number_of_test_cases), function(index) {
  id <- stringi::stri_rand_strings(1, sample(5:15, 1))
  widget <- module$statChart(id, "test-title", "test-icon", 5)
  ui <- as.character(widget$ui(id))

  test_that("Variable ID", {
    expect_equal(TRUE, is.R6(widget))
    expect_equal(FALSE, is.null(ui))
    expect_match(ui, glue::glue('id="{id}"'))
  })
})

lapply(seq_len(number_of_test_cases), function(index) {
  title <- stringi::stri_rand_strings(1, sample(5:15, 1))
  widget <- module$statChart("test-id", title, "test-icon", 5)
  ui <- as.character(widget$ui("test-id"))

  test_that("Variable Title", {
    expect_equal(TRUE, is.R6(widget))
    expect_equal(FALSE, is.null(ui))
    expect_match(ui, glue::glue('<div class="chart_title">{title}</div>'))
  })
})

lapply(seq_len(number_of_test_cases), function(index) {
  icon <- stringi::stri_rand_strings(1, sample(5:15, 1))
  widget <- module$statChart("test-id", "test-title", icon, 5)
  ui <- as.character(widget$ui("test-id"))

  test_that("Variable Title", {
    expect_equal(TRUE, is.R6(widget))
    expect_equal(FALSE, is.null(ui))
    expect_match(ui, glue::glue('<img class="icon" src="{icon}"/>'))
  })
})

lapply(seq_len(number_of_test_cases), function(index) {
  number_of_bars <- sample(seq_len(15), 1)
  widget <- module$statChart("test-id", "test-title", "test-icon", number_of_bars)
  ui <- as.character(widget$ui("test-id"))

  test_that("Variable Bar number", {
    expect_equal(TRUE, is.R6(widget))
    expect_match(ui, 'id="test-id-bar_list"')
    expect_equal(lengths(regmatches(ui, gregexpr('progress-bar', ui))), number_of_bars)
  })
})

lapply(seq_len(number_of_test_cases), function(index) {
  id <- stringi::stri_rand_strings(1, sample(5:15, 1))
  title <- stringi::stri_rand_strings(1, sample(5:15, 1))
  icon <- stringi::stri_rand_strings(1, sample(5:15, 1))
  number_of_bars <- sample(seq_len(15), 1)

  widget <- module$statChart(id, title, icon, number_of_bars)

  test_that("Exists", {
    expect_equal(TRUE, is.R6(widget))
  })

  ui <- as.character(widget$ui(id))

  test_that("ID", {
    expect_match(ui, glue::glue('id="{id}"'))
  })

  test_that("title", {
    expect_match(ui, glue::glue('<div class="chart_title">{title}</div>'))
  })

  test_that("icon", {
    expect_match(ui, glue::glue('<img class="icon" src="{icon}"/>'))
  })

  test_that("bars", {
    expect_equal(lengths(regmatches(ui, gregexpr('progress-bar', ui))), number_of_bars)
  })
})
