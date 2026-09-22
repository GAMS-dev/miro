/* global $:false, Shiny:false */

import DOMPurify from 'dompurify';
import { parseKatex } from './util';

function copyTextWithFeedback(button, text) {
  navigator.clipboard.writeText(text).then(() => {
    const icon = button.querySelector('i');
    if (icon) {
      const originalClass = icon.className;
      icon.className = 'fas fa-check';
      button.style.color = '#28a745';

      setTimeout(() => {
        icon.className = originalClass;
        button.style.color = '';
      }, 1500);
    }
  }).catch((err) => {
    // eslint-disable-next-line no-console
    console.error('Failed to copy AI chat content: ', err);
  });
}
export function copyChatContent(button) {
  const wrapper = button.closest('.miro-ai-chat-bubble-wrapper');
  const contentToCopy = wrapper.querySelector('.miro-ai-chat-content-raw').textContent;
  copyTextWithFeedback(button, contentToCopy);
}
function addCodeCopyButtons(root) {
  const copyLabel = document.getElementById('miroAiChatLog')?.dataset.copyLabel || '';
  root.querySelectorAll('pre').forEach((pre) => {
    if (pre.querySelector('.miro-ai-code-copy-btn')) return;
    const code = pre.querySelector('code') || pre;
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'miro-ai-code-copy-btn';
    button.title = copyLabel;
    button.innerHTML = '<i class="fas fa-copy" aria-hidden="true"></i>';
    button.addEventListener('click', () => {
      copyTextWithFeedback(button, code.textContent);
    });
    pre.appendChild(button);
  });
}
export function toggleChatCollapse(btn) {
  const bubble = btn.closest('.miro-ai-chat-bubble');
  const content = bubble.querySelector('.miro-ai-chat-content-html');
  const icon = btn.querySelector('i');

  if (content.classList.contains('miro-ai-text-collapsed')) {
    content.classList.remove('miro-ai-text-collapsed');
    icon.classList.remove('fa-chevron-down');
    icon.classList.add('fa-chevron-up');
  } else {
    content.classList.add('miro-ai-text-collapsed');
    icon.classList.remove('fa-chevron-up');
    icon.classList.add('fa-chevron-down');
  }
}
export function toggleAiActivityGroup(btn) {
  const group = btn.closest('.miro-ai-activity-group');
  if (!group) return;
  const expanded = group.classList.toggle('miro-ai-activity-group-expanded');
  btn.setAttribute('aria-expanded', String(expanded));
}

function createAiActivityGroup(chatLog) {
  const wrapper = document.createElement('div');
  wrapper.className = 'miro-ai-chat-bubble-wrapper miro-ai-chat-assistant miro-ai-activity-group-wrapper';
  wrapper.dataset.countLabel = chatLog?.dataset.activityGroupLabel || '%s';
  wrapper.innerHTML = `
    <div class="miro-ai-activity-group">
      <button type="button" class="miro-ai-activity-group-toggle" onclick="Miro.toggleAiActivityGroup(this)" aria-expanded="false">
        <i class="fas fa-magnifying-glass" aria-hidden="true"></i>
        <span class="miro-ai-activity-group-count"></span>
        <i class="fas fa-chevron-down miro-ai-activity-group-chevron" aria-hidden="true"></i>
      </button>
      <div class="miro-ai-activity-group-body"></div>
    </div>
  `;
  return wrapper;
}

// Moves a single already-rendered `.miro-ai-tool-activity` bubble's card
// into `group`, drops its now-redundant bubble wrapper, and refreshes the
// group's summary count and error state.
function moveCardIntoGroup(group, cardWrapper) {
  const card = cardWrapper.querySelector('.miro-ai-activity-card');
  if (card) {
    group.querySelector('.miro-ai-activity-group-body').appendChild(card);
    if (card.classList.contains('miro-ai-activity-card-error')) {
      group.classList.add('miro-ai-activity-group-has-error');
    }
  }
  cardWrapper.remove();
  const count = group.querySelectorAll('.miro-ai-activity-card').length;
  const template = group.dataset.countLabel || '%s';
  const countLabel = group.querySelector('.miro-ai-activity-group-count');
  countLabel.textContent = template.replace('%s', String(count));
}

// Called once, on a freshly (re-)rendered chat log (initial history, or the
// whole log after Clear Chat), to merge every existing run of 2+ adjacent
// activity cards. Later arrivals are merged incrementally instead, in the
// `gms-addAiChatCard` handler below.
export function groupAdjacentAiActivityCards(chatLog) {
  const children = Array.from(chatLog.children);
  let run = [];
  const flushRun = () => {
    if (run.length > 1) {
      const group = createAiActivityGroup(chatLog);
      run[0].before(group);
      run.forEach((cardWrapper) => moveCardIntoGroup(group, cardWrapper));
    }
    run = [];
  };
  children.forEach((el) => {
    if (el.classList.contains('miro-ai-tool-activity')) {
      run.push(el);
    } else {
      flushRun();
    }
  });
  flushRun();
}

// A new activity card just arrived while the request is still running.
// Merges it into the previous element if that is itself a group already, or
// if it is a lone activity card that now needs upgrading into one.
function appendAiActivityCard(chatLog, spinner, cardWrapper) {
  const prev = spinner.previousElementSibling;
  if (prev?.classList.contains('miro-ai-activity-group-wrapper')) {
    moveCardIntoGroup(prev, cardWrapper);
    return;
  }
  spinner.before(cardWrapper);
  if (prev?.classList.contains('miro-ai-tool-activity')) {
    const group = createAiActivityGroup(chatLog);
    prev.before(group);
    moveCardIntoGroup(group, prev);
    moveCardIntoGroup(group, cardWrapper);
  }
}
export function resolveAiInputChange(button, apply) {
  const card = button.closest('.miro-ai-change-proposal');
  if (!card) return;
  card.querySelectorAll('button').forEach((el) => {
    // eslint-disable-next-line no-param-reassign
    el.disabled = true;
  });
  Shiny.setInputValue('miroAiInputChangeAction', {
    id: card.dataset.changeId,
    apply,
  }, {
    priority: 'event',
  });
}
export function openMiroAiDrawer() {
  document.getElementById('miroAiDrawer').classList.add('open');
  document.body.classList.add('drawer-open');
  document.getElementById('miroAiUserText').focus();
  $('body').addClass('sidebar-collapse');
  setTimeout(() => {
    window.dispatchEvent(new Event('resize'));
  }, 300);
}
export function closeMiroAiDrawer() {
  document.getElementById('miroAiDrawer').classList.remove('open');
  document.body.classList.remove('drawer-open');
  $('body').removeClass('sidebar-collapse');
  setTimeout(() => {
    window.dispatchEvent(new Event('resize'));
  }, 300);
}

// Helper function for height adjustment
function adjustTextareaHeight(el) {
  el.style.height = 'auto';
  const maxHeight = parseFloat(getComputedStyle(el).lineHeight) * 10;
  if (el.scrollHeight <= maxHeight) {
    el.style.height = `${el.scrollHeight}px`;
    el.style.overflowY = 'hidden';
  } else {
    el.style.height = `${maxHeight}px`;
    el.style.overflowY = 'auto';
  }
}

function sendCurrentUserText(textarea) {
  const content = textarea.value.trim();
  if (content) {
    Shiny.setInputValue('miroAiUserText', content, {
      priority: 'event',
    });
  }
  textarea.value = '';
  adjustTextareaHeight(textarea);
  document.getElementById('btMiroAiSend').style.display = 'none';
}

// Single-$ deliberately excluded: KaTeX's auto-render has no heuristic to tell
// "$x^2$" apart from an ordinary currency mention like "$50 to $75" (a
// near-guarantee in a cost-analysis tool), and would try to render the text
// between them as math either way.
const AI_KATEX_DELIMITERS = [
  { left: '$$', right: '$$', display: true },
  { left: '\\(', right: '\\)', display: false },
  { left: '\\[', right: '\\]', display: true },
];

// Assistant messages are rendered server-side with their (untrusted, LLM-
// generated) HTML stashed in a `data-raw-html` attribute rather than as live
// DOM children -- attribute values are inert text to the browser, so nothing
// can execute no matter which mechanism inserted the markup (a Shiny modal
// re-render, our own custom message handler, etc.). This reads that attribute,
// sanitizes it with DOMPurify, and only then injects it as real HTML.
function hydrateAiMessageContent(el) {
  el.innerHTML = DOMPurify.sanitize(el.dataset.rawHtml);
  delete el.dataset.rawHtml;
  parseKatex(el, { delimiters: AI_KATEX_DELIMITERS });
  addCodeCopyButtons(el);
}
export function hydrateAiMessagesIn(root) {
  root.querySelectorAll('[data-raw-html]').forEach(hydrateAiMessageContent);
}

export function registerAiHandlers() {
  $(document).on('shown.bs.modal', '.ai-assistant-modal', function () {
    $('#miroAiUserText').focus();
    hydrateAiMessagesIn(this);
    const chatLog = document.getElementById('miroAiChatLog');
    if (!chatLog) return;
    groupAdjacentAiActivityCards(chatLog);
    const userBubbles = chatLog.querySelectorAll('.miro-ai-chat-user');
    if (userBubbles.length > 0) {
      userBubbles[userBubbles.length - 1].scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  });
  $(document).on('hidden.bs.modal', '.ai-assistant-modal', () => {
    Shiny.setInputValue('abortAiCall', 1, {
      priority: 'event',
    });
  });
  $(document).on('keydown', '#miroAiUserText', function (e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      // Prevent newline
      e.stopPropagation();
      e.stopImmediatePropagation();
      e.preventDefault();
      sendCurrentUserText(this);
    }
  });
  $(document).on('input', '#miroAiUserText', function () {
    adjustTextareaHeight(this);
    document.getElementById('btMiroAiSend').style.display = this.value.trim() ? 'flex' : 'none';
  });
  $(document).on('click', '#btMiroAiSend', () => {
    sendCurrentUserText(document.getElementById('miroAiUserText'));
  });
  Shiny.addCustomMessageHandler('gms-resetAiChatRequest', (content) => {
    const textarea = document.getElementById('miroAiUserText');
    const stopButton = document.getElementById('btMiroAiStop');
    stopButton.style.display = 'none';
    textarea.disabled = false;
    $('#miroAiChatSpinner').hide();
    if (content?.dividerHtml) {
      const $newElement = $(content.dividerHtml);
      const $chatLog = $('#miroAiChatLog');
      $('#miroAiChatSpinner').before($newElement);
      $chatLog.scrollTop($chatLog[0].scrollHeight);
    }
  });
  // Input-data change proposals are plain server-rendered markup (no
  // `data-raw-html` payload), so they are appended as-is rather than hydrated.
  Shiny.addCustomMessageHandler('gms-addAiChatCard', (content) => {
    const chatLog = document.getElementById('miroAiChatLog');
    const spinner = document.getElementById('miroAiChatSpinner');
    const newElement = $(content.html).get(0);
    if (newElement.classList.contains('miro-ai-tool-activity')) {
      appendAiActivityCard(chatLog, spinner, newElement);
    } else {
      spinner.before(newElement);
    }
    const $chatLog = $(chatLog);
    $chatLog.animate({ scrollTop: $chatLog[0].scrollHeight }, 300);
  });
  Shiny.addCustomMessageHandler('gms-updateAiChatCard', (content) => {
    const card = document.querySelector(`.miro-ai-change-proposal[data-change-id="${content.id}"]`);
    if (!card) return;
    card.outerHTML = content.html;
  });
  Shiny.addCustomMessageHandler('gms-addAiChatBubble', (content) => {
    const $newElement = $(content.html);
    hydrateAiMessagesIn($newElement.get(0));
    $('#miroAiChatSpinner').before($newElement);

    const $chatLog = $('#miroAiChatLog');
    const textarea = document.getElementById('miroAiUserText');
    const stopButton = document.getElementById('btMiroAiStop');
    if (content.role === 'user') {
      textarea.value = '';
      textarea.disabled = true;
      stopButton.style.display = 'block';
      adjustTextareaHeight(textarea);
      $('#miroAiChatSpinner').show();
      $chatLog.scrollTop($chatLog[0].scrollHeight);
    } else {
      $('#miroAiChatSpinner').hide();
      const scrollToPosition = $newElement.position().top + $chatLog.scrollTop();
      $chatLog.animate({ scrollTop: scrollToPosition }, 300);
      stopButton.style.display = 'none';
      textarea.disabled = false;
    }
  });
}
