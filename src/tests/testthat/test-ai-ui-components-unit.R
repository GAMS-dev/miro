library(R6)
library(shiny)
library(htmltools)

source("../../modules/ai_ui_components.R")

mockLang <- list(
  btCopy = "Copy",
  chatBubbleUserHeader = "User:",
  chatBubbleAssistantHeader = "Assistant:"
)

# ==============================================================================
# 1. renderMarkdownPreservingKatexDelimiters (pure string transform)
# ==============================================================================

test_that("renderMarkdownPreservingKatexDelimiters keeps `\\[...\\]` display delimiters intact", {
  html <- renderMarkdownPreservingKatexDelimiters("Cost:\n\n\\[\n300 \\times 90\n\\]\n")
  expect_match(html, "\\\\\\[ 300 \\\\times 90 \\\\\\]", fixed = FALSE)
  expect_false(grepl("<code>", html, fixed = TRUE))
})

test_that("renderMarkdownPreservingKatexDelimiters keeps `\\(...\\)` inline delimiters intact", {
  html <- renderMarkdownPreservingKatexDelimiters("Inline \\(x + y = z\\) here.")
  expect_match(html, "\\\\\\(x \\+ y = z\\\\\\)", fixed = FALSE)
  expect_false(grepl("<code>", html, fixed = TRUE))
})

test_that("renderMarkdownPreservingKatexDelimiters preserves a literal dollar sign inside math", {
  html <- renderMarkdownPreservingKatexDelimiters("\\[\n300 \\times 90 = \\$27{,}000.\n\\]")
  expect_match(html, "\\$27{,}000", fixed = TRUE)
})

test_that("renderMarkdownPreservingKatexDelimiters preserves other LaTeX escapes inside math", {
  html <- renderMarkdownPreservingKatexDelimiters("\\[ 50\\% \\times \\{1, 2\\} \\times x\\_1 \\]")
  expect_match(html, "50\\% ", fixed = TRUE)
  expect_match(html, "\\{1, 2\\}", fixed = TRUE)
  expect_match(html, "x\\_1", fixed = TRUE)
})

test_that("renderMarkdownPreservingKatexDelimiters leaves ordinary markdown escaping outside math untouched", {
  html <- renderMarkdownPreservingKatexDelimiters("See my\\_variable\\_name and 50\\% off.")
  # Outside math, CommonMark's own escape handling should still apply, i.e.
  # the backslash is dropped and no emphasis is triggered by the underscores.
  expect_equal(html, "<p>See my_variable_name and 50% off.</p>\n")
})

test_that("renderMarkdownPreservingKatexDelimiters does not affect ordinary emphasis", {
  html <- renderMarkdownPreservingKatexDelimiters("This is *italic* and _also italic_ and **bold**.")
  expect_match(html, "<em>italic</em>", fixed = TRUE)
  expect_match(html, "<em>also italic</em>", fixed = TRUE)
  expect_match(html, "<strong>bold</strong>", fixed = TRUE)
})

test_that("renderMarkdownPreservingKatexDelimiters does not affect genuine code spans", {
  html <- renderMarkdownPreservingKatexDelimiters("Use the `queryScenarioDatabase` tool.")
  expect_match(html, "<code>queryScenarioDatabase</code>", fixed = TRUE)
})

test_that("renderMarkdownPreservingKatexDelimiters handles multiple separate math blocks", {
  html <- renderMarkdownPreservingKatexDelimiters("First \\[ a + b \\] then \\[ c + d \\] and inline \\(e\\).")
  expect_equal(lengths(regmatches(html, gregexpr("\\\\\\[", html))), 2)
  expect_equal(lengths(regmatches(html, gregexpr("\\\\\\(", html))), 1)
  expect_false(grepl("<code>", html, fixed = TRUE))
})

# ==============================================================================
# 2. renderAiChatBubble (integration: attribute placement + hydration contract)
# ==============================================================================

test_that("renderAiChatBubble keeps math delimiters intact inside the data-raw-html attribute", {
  msg <- list(role = "model", text = "\\[\n300 \\times 90 = \\$27{,}000.\n\\]")
  bubble <- as.character(renderAiChatBubble(msg, mockLang))

  rawHtmlAttr <- regmatches(bubble, regexpr('data-raw-html="[^"]*"', bubble))
  expect_length(rawHtmlAttr, 1)
  # htmltools entity-encodes the attribute value; decode just enough to
  # check the backslash-delimiter sequences survived the round trip.
  expect_match(rawHtmlAttr, "\\\\\\[", fixed = FALSE)
  expect_match(rawHtmlAttr, "\\\\\\]", fixed = FALSE)
  expect_match(rawHtmlAttr, "\\\\\\$27", fixed = FALSE)
})

test_that("renderAiChatBubble does not set data-raw-html for user messages", {
  msg <- list(role = "user", text = "\\[ 1 + 1 \\]")
  bubble <- as.character(renderAiChatBubble(msg, mockLang))

  expect_false(grepl("data-raw-html", bubble, fixed = TRUE))
  # User text is rendered as plain, auto-escaped content -- not run through
  # commonmark/KaTeX delimiter preservation at all.
  expect_match(bubble, "\\[ 1 + 1 \\]", fixed = TRUE)
})
