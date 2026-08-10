app <- AppDriver$new("../../",
  name = "ai_chat_output_analysis_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)

# load the default scenario into the sandbox so the AI assistant has data to
# analyze (mirrors the setup used by download_files_test.R)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)

# nav icon is only rendered once the AI assistant module is activated
expect_true(app$get_js("$('#btAIOutput').length === 1;"))

app$click(selector = "#btAIOutput")
app$wait_for_js("$('.ai-assistant-modal').is(':visible');", timeout = 10000)
Sys.sleep(1)

# initial state: welcome message bubble + scenario context divider, nothing else
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-bubble-wrapper').length;"), 1)
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-context-divider').length;"), 1)
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant .miro-ai-chat-content-raw').text().includes('MIRO AI Assistant');"))
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-context-divider').text().includes('Sandbox scenario:');"))

# model dropdown defaults to the (only) configured provider/model
expect_equal(app$get_value(input = "miroAiModel"), "anthropic/claude-sonnet-5")

# disclaimer is always shown
expect_equal(app$get_js("$('.miro-ai-disclaimer').length;"), 1)

# send button only appears once the user has typed something (textarea is not
# a standard shiny input, so it is not covered by app$set_inputs())
app$run_js(
  "var ta = document.getElementById('miroAiUserText');
  ta.value = 'What is the total cost?';
  ta.dispatchEvent(new Event('input', {bubbles: true}));"
)
expect_equal(app$get_js("$('#btMiroAiSend').css('display');"), "flex")

# sending renders the user bubble immediately (optimistic UI), disables the
# textarea and shows the spinner/stop button while the request is in flight
app$run_js("$('#btMiroAiSend').click();")
app$wait_for_js("$('#miroAiChatLog .miro-ai-chat-user').length === 1;", timeout = 5000)
expect_true(app$get_js("$('#miroAiUserText').prop('disabled');"))
expect_equal(app$get_js("$('#btMiroAiStop').css('display');"), "block")
expect_true(app$get_js("$('#miroAiChatSpinner').is(':visible');"))

# no real AI credentials are configured for tests, so the request to the
# provider is rejected -- verify the app surfaces this gracefully instead of
# leaving the chat stuck in the "waiting for a response" state
app$wait_for_js("$('#miroAiChatLog .miro-ai-chat-assistant').length === 2;", timeout = 30000)
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').last().find('.miro-ai-chat-content-raw').text().includes('Error');"))
expect_false(app$get_js("$('#miroAiUserText').prop('disabled');"))
expect_equal(app$get_js("$('#btMiroAiStop').css('display');"), "none")
expect_true(app$get_js("$('#miroAiChatSpinner').is(':hidden');"))

# failed responses don't count towards token usage
expect_equal(app$get_js("$('#miroAiInputTokens').text();"), "Input Tokens: 0")
expect_equal(app$get_js("$('#miroAiOutputTokens').text();"), "Output Tokens: 0")

# clear chat resets the log back to welcome message + context divider
app$click(selector = "#btMiroAiClearChat")
Sys.sleep(1)
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-bubble-wrapper').length;"), 1)
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-context-divider').length;"), 1)

# ==========================================================================
# Stored XSS guard: assistant messages can echo back attacker-controlled data
# (e.g. a scenario named `<img src=x onerror=alert(1)>`), so the client must
# sanitize the `data-raw-html` payload with DOMPurify before it ever becomes
# live DOM -- see ai_assistant.js::hydrateAiMessageContent (01efd6faa "Fix
# potential stored XSS"). We cannot make the (fake-keyed) provider echo such
# a payload back for real, so this simulates a malicious assistant bubble
# reaching the chat log and re-fires the same `shown.bs.modal` event the app
# already listens for, exercising the real, already-wired hydration path
# instead of a re-implementation of it.
# ==========================================================================
app$run_js(
  "window.__miroXssFired = false;
  var payload = '<img src=x onerror=\"window.__miroXssFired=true\">' +
    '<script>window.__miroXssFired=true;</script>';
  var $bubble = $(
    '<div class=\"miro-ai-chat-bubble-wrapper miro-ai-chat-assistant\">' +
    '<div class=\"miro-ai-chat-bubble\">' +
    '<div class=\"miro-ai-chat-content-html miro-markdown\"></div>' +
    '</div></div>'
  );
  $bubble.find('.miro-ai-chat-content-html').attr('data-raw-html', payload);
  $('#miroAiChatLog').prepend($bubble);
  $('.ai-assistant-modal').trigger('shown.bs.modal');"
)
Sys.sleep(0.5)

expect_false(app$get_js("window.__miroXssFired;"))
expect_equal(app$get_js("$('#miroAiChatLog [data-raw-html]').length;"), 0)
expect_equal(app$get_js("document.querySelectorAll('#miroAiChatLog script').length;"), 0)
expect_equal(app$get_js("document.querySelectorAll('#miroAiChatLog img[onerror]').length;"), 0)

# ==========================================================================
# LaTeX/math rendering: assistant text can contain KaTeX math delimiters
# (`\(...\)`, `\[...\]`, `$$...$$`) preserved through
# renderMarkdownPreservingKatexDelimiters (see test-ai-ui-components-unit.R),
# and is expected to render as real math once hydrated -- see
# ai_assistant.js's `gms-addAiChatBubble` handler, which calls the exported
# `Miro.parseKatex` with this exact delimiter set right after hydration (a
# bare `$...$` is deliberately excluded there, since it is ambiguous with a
# plain currency mention in a cost-analysis tool). We cannot make the
# (fake-keyed) provider return real LaTeX, so this simulates a bubble
# reaching the chat log the same way the stored-XSS check above does, then
# exercises the real, already-wired hydrate -> Miro.parseKatex pipeline
# instead of a re-implementation of it.
# ==========================================================================
app$run_js(
  "var $bubble = $(
    '<div class=\"miro-ai-chat-bubble-wrapper miro-ai-chat-assistant\">' +
    '<div class=\"miro-ai-chat-bubble\">' +
    '<div class=\"miro-ai-chat-content-html miro-markdown\"></div>' +
    '</div></div>'
  );
  var payload = '<p>Inline \\\\(x^2 + y^2 = z^2\\\\) inline math.</p>' +
    '<p>\\\\[ 300 \\\\times 90 = 27{,}000 \\\\]</p>' +
    '<p>Display via dollars: $$a + b = c$$</p>' +
    '<p>Not math: $50 to $75.</p>';
  $bubble.find('.miro-ai-chat-content-html').attr('data-raw-html', payload);
  $('#miroAiChatLog').prepend($bubble);
  $('.ai-assistant-modal').trigger('shown.bs.modal');
  Miro.parseKatex($bubble.find('.miro-ai-chat-content-html').get(0), {
    delimiters: [
      { left: '$$', right: '$$', display: true },
      { left: '\\\\(', right: '\\\\)', display: false },
      { left: '\\\\[', right: '\\\\]', display: true }
    ]
  });"
)
Sys.sleep(0.5)

# every math block (2 backslash-delimited + 1 dollar-delimited) was actually
# picked up and rendered by KaTeX, with no parse errors ...
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').first().find('.katex').length;"), 3)
expect_equal(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').first().find('.katex-error').length;"), 0)
# ... the raw backslash delimiters are gone from the text once KaTeX has
# taken over rendering them ...
expect_false(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').first().text().includes('\\\\(');"))
expect_false(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').first().text().includes('\\\\[');"))
# ... but a plain currency mention outside any math delimiter is left
# untouched, since a bare `$` is deliberately not treated as a delimiter
expect_true(app$get_js("$('#miroAiChatLog .miro-ai-chat-assistant').first().text().includes('$50 to $75');"))

# closing the dialog does not error out
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
expect_false(app$get_js("$('.ai-assistant-modal').is(':visible');"))

app$stop()
