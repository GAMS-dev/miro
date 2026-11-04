library(R6)
library(shiny)
library(htmltools)

source("../../modules/ai_ui_components.R")
# the change proposal card formats values with the helpers the AI worker uses
source("../../components/ai_adapter.R")

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

test_that("renderAiChatBubble never emits untrusted assistant text as live markup (stored XSS guard)", {
  # Regression test for 01efd6faa "Fix potential stored XSS": a scenario
  # named `<img src=x onerror=alert(1)>` and echoed back by the AI is valid
  # CommonMark input, and commonmark passes raw HTML through unchanged. The
  # only thing standing between that and script execution is that this
  # markup must never reach the DOM as live tag children -- it is kept inert
  # inside the `data-raw-html` attribute (htmltools entity-encodes attribute
  # values), and the client only ever turns it into real DOM after running it
  # through DOMPurify (see ai_assistant.js::hydrateAiMessageContent).
  msg <- list(role = "model", text = "Sandbox scenario: <img src=x onerror=alert(1)>")
  bubble <- as.character(renderAiChatBubble(msg, mockLang))

  rawHtmlAttr <- regmatches(bubble, regexpr('data-raw-html="[^"]*"', bubble))
  expect_length(rawHtmlAttr, 1)
  expect_match(rawHtmlAttr, "&lt;img", fixed = TRUE)

  # the literal, executable tag must not appear anywhere in the server-
  # rendered markup, i.e. it was never emitted as live tag content
  expect_false(grepl("<img", bubble, fixed = TRUE))
})

test_that("renderAiChatBubble never emits a raw <script> tag echoed by the AI as live markup", {
  msg <- list(role = "model", text = "<script>alert(document.cookie)</script>")
  bubble <- as.character(renderAiChatBubble(msg, mockLang))

  rawHtmlAttr <- regmatches(bubble, regexpr('data-raw-html="[^"]*"', bubble))
  expect_length(rawHtmlAttr, 1)
  expect_false(grepl("<script", rawHtmlAttr, fixed = TRUE))
  expect_false(grepl("<script", bubble, fixed = TRUE))
})

# ==============================================================================
# 4. renderAiInputChangeProposal (confirmation card for an input data change)
# ==============================================================================

mockChangeLang <- list(
  changeProposalHeader = "Proposed input data change",
  changeProposalBefore = "before",
  changeProposalAfter = "after",
  changeProposalRow = "1 row",
  changeProposalRows = "%s rows",
  changeRowAdded = "new",
  changeRowRemoved = "removed",
  changeProposalTruncated = "Showing the first %s of %s changed values.",
  changeOpMultiply = "multiply by %s",
  changeOpSet = "set to %s",
  changeOpUpsert = "write rows",
  changeOpDelete = "delete rows",
  changeStatusApplied = "Applied to the sandbox scenario.",
  changeStatusDiscarded = "Discarded.",
  changeStatusOutdated = "Outdated.",
  changeStatusFailed = "Could not be applied: %s",
  btApplyChange = "Apply",
  btDiscardChange = "Discard"
)

mockTableChange <- function(status = "pending", ...) {
  modifyList(list(
    changeId = "aichg_test",
    symbolName = "demand", symbolAlias = "Customer Demand",
    isScalar = FALSE, operation = "MULTIPLY", value = 1.1,
    targetColumn = "value", targetColumnIdx = 3L, targetColumnAlias = "Value",
    columnAliases = c("Region", "Year", "Value"),
    filters = list(list(column = "region", columnAlias = "Region", columnIdx = 1L, operator = "=", value = "EU")),
    reason = "User asked for 10% more European demand.",
    status = status,
    preview = list(
      labels = data.frame(Region = c("EU", "EU"), Year = c("2024", "2025"), stringsAsFactors = FALSE),
      columns = "Value", before = list(c("150", "200")), after = list(c("165", "220")),
      noRows = 2L, noRowsShown = 2L
    )
  ), list(...))
}

test_that("renderAiInputChangeProposal shows the summary, the before -> after values and both buttons", {
  html <- as.character(renderAiInputChangeProposal(mockTableChange(), mockChangeLang))

  expect_match(html, 'data-change-id="aichg_test"', fixed = TRUE)
  expect_match(html, "<span>Customer Demand</span>\\s*<span>multiply by 1.1</span>\\s*<span class=\"miro-ai-change-filter\">Region = EU</span>\\s*<span>2 rows</span>")
  inChange <- mockTableChange()
  inChange$filters <- list(list(column = "year", columnAlias = "Year", columnIdx = 2L, operator = "IN", value = "2024,2025 "))
  expect_match(
    as.character(renderAiInputChangeProposal(inChange, mockChangeLang)),
    "<span class=\"miro-ai-change-filter\">Year IN 2024, 2025</span>",
    fixed = TRUE
  )
  # the column is named once, by the table header, rather than in the summary too
  expect_false(grepl("<span>Value</span>", html, fixed = TRUE))
  expect_match(html, "<th>Value</th>", fixed = TRUE)
  expect_match(html, 'title="before">150</span>', fixed = TRUE)
  expect_match(html, 'title="after">165</span>', fixed = TRUE)
  expect_match(html, 'title="after">220</span>', fixed = TRUE)
  expect_match(html, "\u2192", fixed = TRUE)
  expect_match(html, "Miro.resolveAiInputChange(this, true)", fixed = TRUE)
  expect_match(html, "Miro.resolveAiInputChange(this, false)", fixed = TRUE)
})

test_that("renderAiInputChangeProposal replaces the buttons once the change was resolved", {
  for (status in c("applied", "discarded", "outdated")) {
    html <- as.character(renderAiInputChangeProposal(mockTableChange(status = status), mockChangeLang))
    expect_false(grepl("Miro.resolveAiInputChange", html, fixed = TRUE))
  }
  applied <- as.character(renderAiInputChangeProposal(mockTableChange(status = "applied"), mockChangeLang))
  expect_match(applied, "Applied to the sandbox scenario.", fixed = TRUE)

  failed <- as.character(renderAiInputChangeProposal(
    mockTableChange(status = "failed", statusDetail = "boom"), mockChangeLang
  ))
  expect_match(failed, "Could not be applied: boom", fixed = TRUE)
})

test_that("renderAiInputChangeProposal reports a truncated preview", {
  change <- mockTableChange()
  change$preview$noRows <- 4096L
  html <- as.character(renderAiInputChangeProposal(change, mockChangeLang))

  expect_match(html, "Showing the first 2 of 4,096 changed values.", fixed = TRUE)
})

test_that("renderAiInputChangeProposal drops the column/row facts for a scalar widget", {
  change <- list(
    changeId = "aichg_scalar", symbolName = "maxstock", symbolAlias = "Maximum stock",
    isScalar = TRUE, operation = "SET", value = 8, targetColumnAlias = "Maximum stock",
    filters = list(), reason = NULL, status = "pending",
    preview = list(labels = NULL, before = list("4"), after = list("8"), noRows = 1L, noRowsShown = 1L)
  )
  html <- as.character(renderAiInputChangeProposal(change, mockChangeLang))

  expect_match(html, "set to 8", fixed = TRUE)
  expect_match(html, 'title="before">4</span>', fixed = TRUE)
  expect_match(html, 'title="after">8</span>', fixed = TRUE)
  # a single value has no row count and no table header
  expect_false(grepl("1 row", html, fixed = TRUE))
  expect_false(grepl("<thead>", html, fixed = TRUE))
})

test_that("renderAiInputChangeProposal marks added and deleted rows", {
  upsert <- mockTableChange(operation = "UPSERT", value = NULL, filters = list())
  # assigned, as modifyList() would merge it into the default preview
  upsert$preview <- list(
    labels = data.frame(Region = c("EU", "AS"), Year = c("2024", "2026")),
    columns = "Value", before = list(c("150", "")), after = list(c("175", "80")),
    status = c("updated", "added"), noRows = 2L, noRowsShown = 2L
  )
  html <- as.character(renderAiInputChangeProposal(upsert, mockChangeLang))
  expect_match(html, "write rows", fixed = TRUE)
  expect_match(html, "<span>2 rows</span>", fixed = TRUE)
  expect_match(html, "<th>Value</th>", fixed = TRUE)
  expect_match(html, '<tr class="miro-ai-change-row-added">', fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("miro-ai-change-row-", html, fixed = TRUE))), 1L)
  # an added row has no old value to cross out, just the tag next to its key
  expect_match(html, 'AS\\s*<span class="miro-ai-change-tag miro-ai-change-tag-added">new</span>')
  expect_match(html, '<span class="miro-ai-change-value-after">80</span>', fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("miro-ai-change-value-before", html, fixed = TRUE))), 1L)

  delete <- mockTableChange(operation = "DELETE", value = NULL, targetColumnAlias = NULL)
  delete$preview <- list(
    labels = data.frame(Region = "EU", Year = "2024", Value = "150"),
    columns = character(0L), before = list(), after = list(),
    status = "removed", noRows = 1L, noRowsShown = 1L
  )
  html <- as.character(renderAiInputChangeProposal(delete, mockChangeLang))
  expect_match(html, "delete rows", fixed = TRUE)
  expect_match(html, "<span>1 row</span>", fixed = TRUE)
  expect_match(html, '<tr class="miro-ai-change-row-removed">', fixed = TRUE)
  expect_match(html, '<span class="miro-ai-change-tag miro-ai-change-tag-removed">removed</span>', fixed = TRUE)
  expect_match(html, "<th>Region</th>\\s*<th>Year</th>\\s*<th>Value</th>")
  expect_false(grepl("miro-ai-change-value-", html, fixed = TRUE))
})

test_that("renderAiInputChangeProposal escapes assistant supplied text", {
  change <- mockTableChange(reason = "<img src=x onerror=alert(1)>")
  html <- as.character(renderAiInputChangeProposal(change, mockChangeLang))

  # the card is injected as-is on the client, so nothing the model wrote may
  # survive as markup
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_match(html, "&lt;img", fixed = TRUE)
  expect_false(grepl("data-raw-html", html, fixed = TRUE))
})

test_that("renderAiChatItem dispatches on the chat log item type", {
  proposal <- as.character(renderAiChatItem(
    list(role = "inputChange", change = mockTableChange()), mockChangeLang
  ))
  expect_match(proposal, "miro-ai-change-proposal", fixed = TRUE)

  divider <- as.character(renderAiChatItem(list("Sandbox scenario: foo"), mockChangeLang))
  expect_match(divider, "miro-ai-chat-context-divider", fixed = TRUE)

  bubble <- as.character(renderAiChatItem(list(role = "user", text = "hi"), mockLang))
  expect_match(bubble, "miro-ai-chat-user", fixed = TRUE)
})

test_that("renderAiChatLogContent offers to go back to the sandbox only on the active comparison context", {
  # the spinner at the end of the log is defined elsewhere in the app
  if (!exists("genSpinner", envir = globalenv())) {
    assign("genSpinner", function(...) NULL, envir = globalenv())
    withr::defer(rm("genSpinner", envir = globalenv()))
  }
  backLang <- c(mockLang, list(backToSandbox = "Back to sandbox"))
  chatHistory <- list(
    list("Sandbox scenario: foo"),
    list(role = "user", text = "hi"),
    list("Comparison of 2 scenarios <b>")
  )
  html <- as.character(renderAiChatLogContent(chatHistory, backLang, contextIsSandbox = FALSE))
  expect_identical(lengths(regmatches(html, gregexpr("miro-ai-context-back-btn", html, fixed = TRUE))), 1L)
  expect_match(html, "Comparison of 2 scenarios &lt;b&gt;</span>\\s*<button", fixed = FALSE)
  expect_match(html, "btAiBackToSandbox", fixed = TRUE)

  html <- as.character(renderAiChatLogContent(chatHistory, backLang, contextIsSandbox = TRUE))
  expect_false(grepl("miro-ai-context-back-btn", html, fixed = TRUE))
})

test_that("renderAiInputChangeProposal leaves out an empty preview table", {
  change <- mockTableChange(status = "failed", statusDetail = "no matching row")
  change$preview <- list(
    labels = NULL, before = list(), after = list(),
    noRows = 0L, noRowsShown = 0L
  )
  html <- as.character(renderAiInputChangeProposal(change, mockChangeLang))

  expect_false(grepl("miro-ai-change-table", html, fixed = TRUE))
  # without a table header to name it, the summary names the column
  expect_match(html, "<span>Value</span>", fixed = TRUE)
  expect_false(grepl("rows</span>", html, fixed = TRUE))
  expect_match(html, "Could not be applied: no matching row", fixed = TRUE)
})

test_that("renderAiInputChangeProposal renders a dropdown selection change", {
  change <- list(
    changeId = "aichg_choice", symbolName = "modeltype", symbolAlias = "Model type",
    isScalar = TRUE, isChoice = TRUE, operation = "SET", value = "mip",
    targetColumnAlias = "Model type", filters = list(),
    reason = "User asked to solve it as a MIP.", status = "pending",
    preview = list(labels = NULL, before = list("lp"), after = list("mip"), noRows = 1L, noRowsShown = 1L)
  )
  html <- as.character(renderAiInputChangeProposal(change, mockChangeLang))

  expect_match(html, "Model type", fixed = TRUE)
  expect_match(html, "set to mip", fixed = TRUE)
  expect_match(html, ">lp<", fixed = TRUE)
  expect_match(html, ">mip<", fixed = TRUE)
  # a single choice has no row count
  expect_false(grepl("1 row", html, fixed = TRUE))
  expect_match(html, "Miro.resolveAiInputChange(this, true)", fixed = TRUE)
})

# ==============================================================================
# 5. renderAiToolActivity (live "what did the model query" card)
# ==============================================================================

mockActivityLang <- list(
  toolActivityStatusError = "Error",
  toolActivityStatusRows = "%s rows",
  toolActivitySymbol = "Symbol",
  toolActivityScenarios = "Scenarios",
  toolActivityAggregation = "Aggregation",
  toolActivityAnalysisMode = "Comparison",
  toolActivityGroupBy = "Grouped by",
  toolActivityFilter = "Rows",
  toolActivityOrderBy = "Sorted by",
  toolActivityLimit = "Limit",
  toolActivityRows = "Rows returned"
)

mockActivity <- function(...) {
  modifyList(list(
    symbolName = "demand", symbolAlias = "Customer Demand",
    scenarioNames = "Base",
    filters = list(list(column = "region", operator = "=", value = "NA")),
    rowCount = 1L, truncated = FALSE
  ), list(...))
}

test_that("renderAiToolActivity shows the symbol, scenarios, filters and row count", {
  html <- as.character(renderAiToolActivity(mockActivity(), mockActivityLang))

  expect_match(html, "Customer Demand", fixed = TRUE)
  expect_match(html, "Base", fixed = TRUE)
  expect_match(html, "region = NA", fixed = TRUE)
  expect_match(html, "1 rows", fixed = TRUE)
  expect_false(grepl("miro-ai-activity-card-error", html, fixed = TRUE))
})

test_that("renderAiToolActivity falls back to the raw symbol name when no alias is known", {
  html <- as.character(renderAiToolActivity(mockActivity(symbolAlias = NULL, symbolName = "unknown_sym"), mockActivityLang))
  expect_match(html, "unknown_sym", fixed = TRUE)
})

test_that("renderAiToolActivity marks a failed query and shows the error instead of a row count", {
  activity <- mockActivity(error = "Symbol 'foo' does not exist.", rowCount = NULL, filters = NULL)
  html <- as.character(renderAiToolActivity(activity, mockActivityLang))

  expect_match(html, "miro-ai-activity-card-error", fixed = TRUE)
  expect_match(html, "Symbol 'foo' does not exist.", fixed = TRUE)
  expect_false(grepl("Rows returned", html, fixed = TRUE))
})

test_that("renderAiToolActivity shows the truncation warning", {
  activity <- mockActivity(truncated = TRUE, warning = "WARNING: Results truncated to 1 rows.")
  html <- as.character(renderAiToolActivity(activity, mockActivityLang))

  expect_match(html, "miro-ai-activity-warning", fixed = TRUE)
  expect_match(html, "WARNING: Results truncated to 1 rows.", fixed = TRUE)
})

test_that("renderAiToolActivity shows aggregation, comparison mode, grouping, sort order and limit when present", {
  activity <- mockActivity(
    aggregation = "SUM", analysisMode = "DIFFERENCE", groupByColumn = "region",
    orderBy = "_delta", sortDirection = "DESC", limit = 5L
  )
  html <- as.character(renderAiToolActivity(activity, mockActivityLang))

  expect_match(html, "SUM", fixed = TRUE)
  expect_match(html, "DIFFERENCE", fixed = TRUE)
  expect_match(html, "region", fixed = TRUE)
  expect_match(html, "_delta (DESC)", fixed = TRUE)
  expect_match(html, ">5<", fixed = TRUE)
})

test_that("renderAiToolActivity joins multiple scenario names and group-by columns", {
  activity <- mockActivity(scenarioNames = list("Base", "High Demand"), groupByColumn = list("region", "year"))
  html <- as.character(renderAiToolActivity(activity, mockActivityLang))

  expect_match(html, "Base, High Demand", fixed = TRUE)
  expect_match(html, "region, year", fixed = TRUE)
})

test_that("renderAiToolActivity never emits model-controlled text as live markup (stored XSS guard)", {
  activity <- mockActivity(symbolAlias = "<img src=x onerror=alert(1)>", error = "<script>alert(1)</script>")
  html <- as.character(renderAiToolActivity(activity, mockActivityLang))

  expect_false(grepl("<img", html, fixed = TRUE))
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_match(html, "&lt;img", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("data-raw-html", html, fixed = TRUE))
})

test_that("renderAiChatItem dispatches tool activity items to renderAiToolActivity", {
  html <- as.character(renderAiChatItem(list(role = "toolActivity", activity = mockActivity()), mockActivityLang))
  expect_match(html, "miro-ai-tool-activity", fixed = TRUE)
  expect_match(html, "Customer Demand", fixed = TRUE)
})

# ==============================================================================
# Rebased proposals and "Apply all"
# ==============================================================================

mockGroupLang <- c(mockChangeLang, list(
  changeRebased = "Preview updated.",
  changeGroupRebased = "Group preview updated.",
  changeGroupHeader = "%d proposed changes",
  changeGroupIndependent = "These changes do not affect each other.",
  changeGroupCombined = "Applied together, in this order:",
  changeGroupStatusApplied = "All changes applied.",
  changeGroupStatusRetired = "Handled one by one.",
  btApplyAllChanges = "Apply all %d"
))
mockGroup <- function(...) {
  modifyList(list(
    groupId = "group_aichg_a", changeIds = c("aichg_a", "aichg_b"),
    status = "pending", independent = TRUE, steps = NULL
  ), list(...))
}

test_that("a rebased proposal says its preview was updated, but only while pending", {
  rebased <- as.character(renderAiInputChangeProposal(mockTableChange(rebased = TRUE), mockGroupLang))
  expect_match(rebased, "Preview updated.", fixed = TRUE)
  expect_match(rebased, "Miro.resolveAiInputChange(this, true)", fixed = TRUE)

  applied <- as.character(renderAiInputChangeProposal(
    mockTableChange(status = "applied", rebased = TRUE), mockGroupLang
  ))
  expect_false(grepl("Preview updated.", applied, fixed = TRUE))
  expect_false(grepl("Preview updated.", as.character(
    renderAiInputChangeProposal(mockTableChange(), mockGroupLang)
  ), fixed = TRUE))
})

test_that("the group card offers Apply all and can be replaced like a proposal card", {
  html <- as.character(renderAiInputChangeGroup(mockGroup(), mockGroupLang))
  expect_match(html, "2 proposed changes", fixed = TRUE)
  expect_match(html, "These changes do not affect each other.", fixed = TRUE)
  expect_match(html, "Apply all 2", fixed = TRUE)
  expect_match(html, "Miro.resolveAiInputChangeGroup(this)", fixed = TRUE)
  # gms-updateAiChatCard finds cards by this class and attribute
  expect_match(html, "miro-ai-change-proposal", fixed = TRUE)
  expect_match(html, 'data-change-id="group_aichg_a"', fixed = TRUE)
})

test_that("the group card shows the combined effect step by step when changes interact", {
  step <- mockTableChange()
  step$preview$after <- list(c("170", "225"))
  html <- as.character(renderAiInputChangeGroup(
    mockGroup(independent = FALSE, steps = list(mockTableChange(), step)), mockGroupLang
  ))
  expect_match(html, "Applied together, in this order:", fixed = TRUE)
  expect_match(html, '<span class="miro-ai-change-step-no">1</span>\\s*<div class="miro-ai-change-meta">\\s*<span>Customer Demand</span>\\s*<span>multiply by 1.1</span>\\s*<span class="miro-ai-change-filter">Region = EU</span>')
  expect_match(html, '<span class="miro-ai-change-step-no">2</span>', fixed = TRUE)
  expect_match(html, 'title="after">225</span>', fixed = TRUE)
  expect_false(grepl("do not affect each other", html, fixed = TRUE))
})

test_that("a proposal in a pending group leaves the primary action to Apply all", {
  grouped <- as.character(renderAiInputChangeProposal(mockTableChange(grouped = TRUE), mockGroupLang))
  expect_match(grouped, 'class="btn btn-sm miro-ai-change-apply btn-default"', fixed = TRUE)
  alone <- as.character(renderAiInputChangeProposal(mockTableChange(), mockGroupLang))
  expect_match(alone, 'class="btn btn-sm miro-ai-change-apply btn-primary"', fixed = TRUE)
})

test_that("the group card replaces its button once resolved", {
  for (status in c("applied", "retired", "outdated", "failed")) {
    html <- as.character(renderAiInputChangeGroup(
      mockGroup(status = status, statusDetail = "too large"), mockGroupLang
    ))
    expect_false(grepl("Miro.resolveAiInputChangeGroup", html, fixed = TRUE))
  }
  expect_match(as.character(renderAiInputChangeGroup(mockGroup(status = "applied"), mockGroupLang)),
    "All changes applied.",
    fixed = TRUE
  )
  expect_match(as.character(renderAiInputChangeGroup(
    mockGroup(status = "failed", statusDetail = "too large"), mockGroupLang
  )), "Could not be applied: too large", fixed = TRUE)
  expect_match(as.character(renderAiInputChangeGroup(
    mockGroup(rebased = TRUE), mockGroupLang
  )), "Group preview updated.", fixed = TRUE)
})

test_that("renderAiChatItem dispatches group items", {
  html <- as.character(renderAiChatItem(list(role = "inputChangeGroup", group = mockGroup()), mockGroupLang))
  expect_match(html, "miro-ai-change-group", fixed = TRUE)
})

test_that("aiChartIdsIn finds the charts embedded in an answer", {
  expect_identical(
    aiChartIdsIn("A\n```mirachart\naichart_abc123\n```\nB\n```mirachart\naichart_abc123\n```\n```r\naichart_x\n```"),
    "aichart_abc123"
  )
  expect_identical(aiChartIdsIn("no charts"), character(0L))
})

getBubbleCharts <- function(bubble) {
  chartsJson <- tagQuery(bubble)$find(".miro-ai-chat-content-html")$selectedTags()[[1]]$attribs[["data-charts"]]
  return(if (length(chartsJson)) jsonlite::fromJSON(chartsJson, simplifyVector = FALSE))
}
test_that("renderAiChatBubble passes the chart data of an answer to the client", {
  chart <- list(
    id = "aichart_abc123", title = "Cost", type = "bar", labels = "EU",
    datasets = list(list(label = "Base", data = NA_real_, color = "#d62728")), view = list(rows = "region"), sids = 1L
  )
  bubble <- renderAiChatBubble(
    list(role = "model", text = "```mirachart\naichart_abc123\n```", charts = list(aichart_abc123 = chart)),
    mockLang
  )
  charts <- getBubbleCharts(bubble)
  expect_identical(charts[[1]]$id, "aichart_abc123")
  expect_identical(charts[[1]]$labels, list("EU"))
  expect_identical(charts[[1]]$datasets[[1]]$data, list(NULL))
  expect_identical(charts[[1]]$datasets[[1]]$color, "#d62728")
  expect_null(charts[[1]]$view)
  expect_false(charts[[1]]$canOpenInPivot)
  chart$datasets[[1]]$colors <- c(NA, "#d62728")
  bubble <- renderAiChatBubble(list(role = "model", text = "", charts = list(aichart_abc123 = chart)), mockLang)
  charts <- getBubbleCharts(bubble)
  expect_identical(charts[[1]]$datasets[[1]]$colors, list(NULL, "#d62728"))
  bubble <- renderAiChatBubble(list(role = "model", text = "No chart"), mockLang)
  expect_null(getBubbleCharts(bubble))
})

test_that("renderAiChatWindow passes the chart palette to the client", {
  chartPalette <- function(...) {
    jsonlite::fromJSON(renderAiChatWindow(mockLang, list(), ...)[[1]]$attribs[["data-chart-palette"]])
  }
  expect_identical(chartPalette(), defaultChartColors)
  expect_identical(chartPalette(chartPalette = c("#111111", "#222222")), c("#111111", "#222222"))
})
