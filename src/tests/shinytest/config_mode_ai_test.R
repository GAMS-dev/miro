app <- AppDriver$new("../../",
  name = "config_mode_ai_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)
jsonPath <- file.path("..", "model", "pickstock_config_mode", "conf_pickstock_configuration")

readAiConfig <- function() {
  configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  ))
  return(configNew$aiAssistant)
}

# ==========================================================================
# General settings: enable checkbox + domain context/instructions editors
# ==========================================================================
app$click(selector = "a[data-value='new_gen']")
Sys.sleep(1)
app$click(selector = "a[data-value='AI Assistant']")
Sys.sleep(1)

# enabled by default when no aiAssistant key is present in the config yet
expect_true(app$get_value(input = "general_enableAiAssistant"))
expect_true(app$get_js("$('#btEditAiContext').is(':visible');"))
expect_true(app$get_js("$('#btEditAiInstructions').is(':visible');"))

app$set_inputs(general_enableAiAssistant = FALSE)
Sys.sleep(1)
expect_identical(readAiConfig()$enable, FALSE)
expect_false(app$get_js("$('#btEditAiContext').is(':visible');"))

app$set_inputs(general_enableAiAssistant = TRUE)
Sys.sleep(1)
expect_identical(readAiConfig()$enable, TRUE)

app$click(selector = "#btEditAiContext")
app$wait_for_js("$('.modal-body').is(':visible');", timeout = 5000)
app$run_js("
  var ta = document.getElementById('mdContent');
  ta.value = 'Custom domain context for testing.';
")
app$click(selector = "#btAiContextSave")
Sys.sleep(1)
expect_identical(readAiConfig()$domainContext, "Custom domain context for testing.")

app$click(selector = "#btEditAiInstructions")
app$wait_for_js("$('.modal-body').is(':visible');", timeout = 5000)
app$run_js("
  var ta = document.getElementById('mdContent');
  ta.value = 'Always answer in bullet points.';
")
app$click(selector = "#btAiInstructionsSave")
Sys.sleep(1)
expect_identical(readAiConfig()$customInstructions, "Always answer in bullet points.")

# ==========================================================================
# Graphs tab: AI Copilot drawer
# ==========================================================================
app$click(selector = "a[data-value='new_graph']")
Sys.sleep(1)
app$click(selector = "button[id='dbInput']")
Sys.sleep(5)
app$set_inputs(gams_symbols = "abserror")
Sys.sleep(4)

expect_true(app$get_js("$('#btOpenAiCopilot').length === 1;"))
app$click(selector = "#btOpenAiCopilot")
app$wait_for_js("$('#miroAiDrawer').hasClass('open');", timeout = 5000)
Sys.sleep(1)

expect_true(app$get_js("$('body').hasClass('drawer-open');"))
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant .miro-ai-chat-content-raw').text().includes('custom renderer code');"))
expect_identical(app$get_js("$('#miroAiUserText').attr('placeholder');"), "What should we build today?")
expect_true(app$get_js("$('#btMiroAiApplyCode').is(':visible');"))

app$run_js(
  "var ta = document.getElementById('miroAiUserText');
  ta.value = 'Build me a bar chart';
  ta.dispatchEvent(new Event('input', {bubbles: true}));"
)
app$run_js("$('#btMiroAiSend').click();")
app$wait_for_js("$('#miroAiChatLog .miro-ai-chat-user').length === 1;", timeout = 5000)
expect_true(app$get_js("$('#miroAiChatSpinner').is(':visible');"))

# no real AI credentials are configured for tests, so the request fails --
# verify the app surfaces this gracefully rather than hanging
app$wait_for_js("$('#miroAiChatLog .miro-ai-chat-assistant').length === 2;", timeout = 30000)
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').last().find('.miro-ai-chat-content-raw').text().includes('Error');"))
expect_true(app$get_js("$('#miroAiChatSpinner').is(':hidden');"))

# applying code from a response with no code blocks surfaces a notification
# instead of silently doing nothing
app$click(selector = "#btMiroAiApplyCode")
app$wait_for_js("$('.shiny-notification-content').length > 0;", timeout = 5000)
expect_true(app$get_js("$('.shiny-notification-content').text().includes('Neither output nor renderer function found');"))

app$click(selector = "#btMiroAiClearChat")
Sys.sleep(1)
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-bubble-wrapper').length;"), 1)

app$run_js("Miro.closeMiroAiDrawer();")
Sys.sleep(1)
expect_false(app$get_js("$('#miroAiDrawer').hasClass('open');"))
expect_false(app$get_js("$('body').hasClass('drawer-open');"))

app$stop()
